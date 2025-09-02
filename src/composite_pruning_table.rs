use permutation_group::PermutationGroup as PG;
use invertable::Invertable;
use equivalence_class::EquivalenceClass;
use table_traits::{ TableTurn, TableSymTurn, TableRawIndexToSymIndex, TableRepCount, TableSearchToken, TableSearch };
use flat_pruning_table::{PruningTable, LowerBoundToken};

use std::sync::Arc;
use std::collections::{BTreeSet, BTreeMap};
use std::collections::VecDeque;
use enum_iterator::{all, Sequence};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
// Opaque type to prevent accidental misuse
// NOTE: I don't think we need PermIndex to prevent possible confusions?
pub struct CompositeLowerBoundToken<SearchTokenA, SearchTokenB>(u8, SearchTokenA, SearchTokenB);

impl<SearchTokenA: TableSearchToken, SearchTokenB: TableSearchToken> CompositeLowerBoundToken<SearchTokenA, SearchTokenB> {
    pub fn new(a: SearchTokenA, b: SearchTokenB) -> Self {
        Self(std::cmp::max(a.table_get_lower_bound(), b.table_get_lower_bound()), a, b)
    }

    pub fn get_tokens(self) -> (SearchTokenA, SearchTokenB) {
        (self.1, self.2)
    }
}

impl<SearchTokenA: TableSearchToken, SearchTokenB: TableSearchToken> TableSearchToken for CompositeLowerBoundToken<SearchTokenA, SearchTokenB> {
    type Index = (SearchTokenA::Index, SearchTokenB::Index);

    fn table_get_index(&self) -> Self::Index {
        (self.1.table_get_index(), self.2.table_get_index())
    }

    fn table_get_lower_bound(&self) -> u8 {
        self.0
    }
}

#[derive(Debug)]
pub struct CompositePruningTable<SymA, SymB, PermIndexA, PermIndexB, RepIndexA, RepIndexB, Turn, MoveTableA, MoveTableB> {
    a: PruningTable<SymA, PermIndexA, RepIndexA, Turn, MoveTableA>,
    b: PruningTable<SymB, PermIndexB, RepIndexB, Turn, MoveTableB>,
}

impl<MoveTableA: TableTurn<SymA, RepIndexA, Turn> + TableSymTurn<SymA, RepIndexA, Turn> + TableRawIndexToSymIndex<SymA, PermIndexA, RepIndexA> + TableRepCount, MoveTableB: TableTurn<SymB, RepIndexB, Turn> + TableSymTurn<SymB, RepIndexB, Turn> + TableRawIndexToSymIndex<SymB, PermIndexB, RepIndexB> + TableRepCount, Turn: std::fmt::Debug + Sequence + Copy + Invertable + EquivalenceClass<SymA> + EquivalenceClass<SymB> + Ord, SymA: std::fmt::Debug + Sequence + Copy + Clone + PG + Ord, SymB: std::fmt::Debug + Sequence + Copy + Clone + PG + Ord, PermIndexA: Sequence + Copy + Ord, PermIndexB: Sequence + Copy + Ord, RepIndexA: std::fmt::Debug + Copy + Ord + Into<usize>, RepIndexB: std::fmt::Debug + Copy + Ord + Into<usize>> CompositePruningTable<SymA, SymB, PermIndexA, PermIndexB, RepIndexA, RepIndexB, Turn, MoveTableA, MoveTableB> {
    pub fn new(a: PruningTable<SymA, PermIndexA, RepIndexA, Turn, MoveTableA>, b: PruningTable<SymB, PermIndexB, RepIndexB, Turn, MoveTableB>) -> Self {
        CompositePruningTable { a, b }
    }

    pub fn start_search(&self, (pi_a, pi_b): (PermIndexA, PermIndexB)) -> CompositeLowerBoundToken<LowerBoundToken<(RepIndexA, SymA), Turn>, LowerBoundToken<(RepIndexB, SymB), Turn>> {
        let lbt_a = self.a.start_search(pi_a);
        let lbt_b = self.b.start_search(pi_b);
        CompositeLowerBoundToken::new(lbt_a, lbt_b)
    }

    pub fn continue_search(&self, lbt: CompositeLowerBoundToken<LowerBoundToken<(RepIndexA, SymA), Turn>, LowerBoundToken<(RepIndexB, SymB), Turn>>, t: Turn) -> CompositeLowerBoundToken<LowerBoundToken<(RepIndexA, SymA), Turn>, LowerBoundToken<(RepIndexB, SymB), Turn>> {
        let (token_a, token_b) = lbt.get_tokens();
        let lbt_a = self.a.continue_search(token_a, t);
        let lbt_b = self.b.continue_search(token_b, t);
        CompositeLowerBoundToken::new(lbt_a, lbt_b)
    }

    pub fn solve(&self, pi: (PermIndexA, PermIndexB)) -> Vec<Turn> {
        let mut queue = BTreeSet::new();
        // NOTE: Even though all turns are the same weight, we still have to
        // account for possibly finding faster routes to states later.  The
        // following graph shows an example of where the overly optimistic
        // lower bounds for b and c could have us discover f via c, which
        // would have us think that the cost to f is 3, when actually it could
        // be 2.  We just won't discover this path right away, because the more
        // accurate lower bound on e means it won't be explored until after c.
        // 
        // state(minimum possible total turns, estimated remaining turns)
        //     a -> b(2,1) -> c(3,1) -> d -> *
        //      \             v             /
        //       -> e(3,2) -> f ------------
        let mut shortest_known_path: BTreeMap<_, u8> = BTreeMap::new();
        let lbt = self.start_search(pi);
        queue.insert((lbt.table_get_lower_bound(), lbt, Vec::new()));
        shortest_known_path.insert(lbt.table_get_index(), 0);

        loop {
            let (c, lbt, turns) = queue.pop_first().expect("Invariant violation: It's impossible that the queue is empty, as that would imply that the position is unsolvable!");
            if lbt.table_get_lower_bound() == 0 {
                break turns
            }


            for t in all::<Turn>() {
                let lbt2 = self.continue_search(lbt, t);
                let si = lbt2.clone().table_get_index();
                if shortest_known_path.get(&si).map((|n| turns.len() as u8 + 1 < *n)).unwrap_or(true) {
                    shortest_known_path.insert(si, turns.len() as u8 + 1);
                    // TODO: use linked lists to cut down on memory usage
                    let mut turns2 = turns.clone();
                    turns2.push(t);
                    queue.insert((lbt2.table_get_lower_bound() + turns2.len() as u8, lbt2, turns2));
                }
            }
        }
    }
}

impl<MoveTableA: TableTurn<SymA, RepIndexA, Turn> + TableSymTurn<SymA, RepIndexA, Turn> + TableRawIndexToSymIndex<SymA, PermIndexA, RepIndexA> + TableRepCount, MoveTableB: TableTurn<SymB, RepIndexB, Turn> + TableSymTurn<SymB, RepIndexB, Turn> + TableRawIndexToSymIndex<SymB, PermIndexB, RepIndexB> + TableRepCount, Turn: std::fmt::Debug + Sequence + Copy + Invertable + EquivalenceClass<SymA> + EquivalenceClass<SymB> + Ord, SymA: std::fmt::Debug + Sequence + Copy + Clone + PG + Ord, SymB: std::fmt::Debug + Sequence + Copy + Clone + PG + Ord, PermIndexA: Sequence + Copy + Ord, PermIndexB: Sequence + Copy + Ord, RepIndexA: std::fmt::Debug + Copy + Ord + Into<usize>, RepIndexB: std::fmt::Debug + Copy + Ord + Into<usize>> TableSearch<(PermIndexA, PermIndexB), Turn> for CompositePruningTable<SymA, SymB, PermIndexA, PermIndexB, RepIndexA, RepIndexB, Turn, MoveTableA, MoveTableB> {
    type SearchToken = CompositeLowerBoundToken<LowerBoundToken<(RepIndexA, SymA), Turn>, LowerBoundToken<(RepIndexB, SymB), Turn>>;

    fn table_start_search(&self, i: (PermIndexA, PermIndexB)) -> Self::SearchToken {
        self.start_search(i)
    }

    fn table_continue_search(&self, st: Self::SearchToken, t: Turn) -> Self::SearchToken {
        self.continue_search(st, t)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use three_triangles;
    use three_triangles_stack::*;
    use representative_table::RepresentativeTable;
    use flat_move_table::MoveTable;

    #[test]
    fn pruning_table_is_correct_for_three_triangles_stack_even_parity_with_no_symmetry_via_composite() {
        // TODO: Ideally these use the same move table!
        let top_rep_table = Arc::new(RepresentativeTable::new::<TopThreeTriangles>());
        let top_move_table: MoveTable<NoSymmetry, three_triangles::ThreeTrianglesEvenIndex, Turns> = MoveTable::new::<TopThreeTriangles>(top_rep_table.clone());
        let top_pruning_table: PruningTable<NoSymmetry, three_triangles::ThreeTrianglesEvenIndex, _, Turns, _> = PruningTable::new(top_move_table, std::iter::once(three_triangles::ThreeTriangles::identity().into()));

        let bottom_rep_table = Arc::new(RepresentativeTable::new::<BottomThreeTriangles>());
        let bottom_move_table: MoveTable<NoSymmetry, three_triangles::ThreeTrianglesEvenIndex, Turns> = MoveTable::new::<BottomThreeTriangles>(bottom_rep_table.clone());
        let bottom_pruning_table: PruningTable<NoSymmetry, three_triangles::ThreeTrianglesEvenIndex, _, Turns, _> = PruningTable::new(bottom_move_table, std::iter::once(three_triangles::ThreeTriangles::identity().into()));
        let pruning_table = CompositePruningTable::new(top_pruning_table, bottom_pruning_table);

        // Our simple implementation (s small enough to solve naively) matches our more complex one
        let tt_table = moves_to_solve();
        for top_pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            for bottom_pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
                let pi = (top_pi, bottom_pi);
                let p = (top_pi.into(), bottom_pi.into());
                assert_eq!(tt_table.get(&p).unwrap().len(), pruning_table.solve(pi).len());
            }
        }
    }

    #[test]
    fn pruning_table_is_correct_for_three_triangles_stack_even_parity_with_mirror_ud_symmetry_via_composite() {
        // TODO: Ideally these use the same move table!
        let top_rep_table = Arc::new(RepresentativeTable::new::<TopThreeTriangles>());
        let top_move_table: MoveTable<MirrorUDSymmetry, three_triangles::ThreeTrianglesEvenIndex, Turns> = MoveTable::new::<TopThreeTriangles>(top_rep_table.clone());
        let top_pruning_table: PruningTable<MirrorUDSymmetry, three_triangles::ThreeTrianglesEvenIndex, _, Turns, _> = PruningTable::new(top_move_table, std::iter::once(three_triangles::ThreeTriangles::identity().into()));

        let bottom_rep_table = Arc::new(RepresentativeTable::new::<BottomThreeTriangles>());
        let bottom_move_table: MoveTable<MirrorUDSymmetry, three_triangles::ThreeTrianglesEvenIndex, Turns> = MoveTable::new::<BottomThreeTriangles>(bottom_rep_table.clone());
        let bottom_pruning_table: PruningTable<MirrorUDSymmetry, three_triangles::ThreeTrianglesEvenIndex, _, Turns, _> = PruningTable::new(bottom_move_table, std::iter::once(three_triangles::ThreeTriangles::identity().into()));
        let pruning_table = CompositePruningTable::new(top_pruning_table, bottom_pruning_table);

        // Our simple implementation (s small enough to solve naively) matches our more complex one
        let tt_table = moves_to_solve();
        for top_pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            for bottom_pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
                let pi = (top_pi, bottom_pi);
                let p = (top_pi.into(), bottom_pi.into());
                assert_eq!(tt_table.get(&p).unwrap().len(), pruning_table.solve(pi).len());
            }
        }
    }

    #[test]
    fn pruning_table_is_correct_for_three_triangles_stack_even_parity_with_rotational_symmetry_via_composite() {
        // TODO: Ideally these use the same move table!
        let top_rep_table = Arc::new(RepresentativeTable::new::<TopThreeTriangles>());
        let top_move_table: MoveTable<RotationalSymmetry, three_triangles::ThreeTrianglesEvenIndex, Turns> = MoveTable::new::<TopThreeTriangles>(top_rep_table.clone());
        let top_pruning_table: PruningTable<RotationalSymmetry, three_triangles::ThreeTrianglesEvenIndex, _, Turns, _> = PruningTable::new(top_move_table, std::iter::once(three_triangles::ThreeTriangles::identity().into()));

        let bottom_rep_table = Arc::new(RepresentativeTable::new::<BottomThreeTriangles>());
        let bottom_move_table: MoveTable<RotationalSymmetry, three_triangles::ThreeTrianglesEvenIndex, Turns> = MoveTable::new::<BottomThreeTriangles>(bottom_rep_table.clone());
        let bottom_pruning_table: PruningTable<RotationalSymmetry, three_triangles::ThreeTrianglesEvenIndex, _, Turns, _> = PruningTable::new(bottom_move_table, std::iter::once(three_triangles::ThreeTriangles::identity().into()));
        let pruning_table = CompositePruningTable::new(top_pruning_table, bottom_pruning_table);

        // Our simple implementation (s small enough to solve naively) matches our more complex one
        let tt_table = moves_to_solve();
        for top_pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            for bottom_pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
                let pi = (top_pi, bottom_pi);
                let p = (top_pi.into(), bottom_pi.into());
                assert_eq!(tt_table.get(&p).unwrap().len(), pruning_table.solve(pi).len());
            }
        }
    }

    #[test]
    fn pruning_table_is_correct_for_three_triangles_stack_even_parity_with_full_symmetry_via_composite() {
        // TODO: Ideally these use the same move table!
        let top_rep_table = Arc::new(RepresentativeTable::new::<TopThreeTriangles>());
        let top_move_table: MoveTable<FullSymmetry, three_triangles::ThreeTrianglesEvenIndex, Turns> = MoveTable::new::<TopThreeTriangles>(top_rep_table.clone());
        let top_pruning_table: PruningTable<FullSymmetry, three_triangles::ThreeTrianglesEvenIndex, _, Turns, _> = PruningTable::new(top_move_table, std::iter::once(three_triangles::ThreeTriangles::identity().into()));

        let bottom_rep_table = Arc::new(RepresentativeTable::new::<BottomThreeTriangles>());
        let bottom_move_table: MoveTable<FullSymmetry, three_triangles::ThreeTrianglesEvenIndex, Turns> = MoveTable::new::<BottomThreeTriangles>(bottom_rep_table.clone());
        let bottom_pruning_table: PruningTable<FullSymmetry, three_triangles::ThreeTrianglesEvenIndex, _, Turns, _> = PruningTable::new(bottom_move_table, std::iter::once(three_triangles::ThreeTriangles::identity().into()));
        let pruning_table = CompositePruningTable::new(top_pruning_table, bottom_pruning_table);

        // Our simple implementation (s small enough to solve naively) matches our more complex one
        let tt_table = moves_to_solve();
        for top_pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            for bottom_pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
                let pi = (top_pi, bottom_pi);
                let p = (top_pi.into(), bottom_pi.into());
                assert_eq!(tt_table.get(&p).unwrap().len(), pruning_table.solve(pi).len());
            }
        }
    }

    #[test]
    fn pruning_table_is_correct_for_three_triangles_stack_even_parity_with_distinct_symmetry_via_composite() {
        // TODO: Ideally these use the same move table!
        let top_rep_table = Arc::new(RepresentativeTable::new::<TopThreeTriangles>());
        let top_move_table: MoveTable<MirrorUDSymmetry, three_triangles::ThreeTrianglesEvenIndex, Turns> = MoveTable::new::<TopThreeTriangles>(top_rep_table.clone());
        let top_pruning_table: PruningTable<MirrorUDSymmetry, three_triangles::ThreeTrianglesEvenIndex, _, Turns, _> = PruningTable::new(top_move_table, std::iter::once(three_triangles::ThreeTriangles::identity().into()));

        let bottom_rep_table = Arc::new(RepresentativeTable::new::<BottomThreeTriangles>());
        let bottom_move_table: MoveTable<RotationalSymmetry, three_triangles::ThreeTrianglesEvenIndex, Turns> = MoveTable::new::<BottomThreeTriangles>(bottom_rep_table.clone());
        let bottom_pruning_table: PruningTable<RotationalSymmetry, three_triangles::ThreeTrianglesEvenIndex, _, Turns, _> = PruningTable::new(bottom_move_table, std::iter::once(three_triangles::ThreeTriangles::identity().into()));
        let pruning_table = CompositePruningTable::new(top_pruning_table, bottom_pruning_table);

        // Our simple implementation (s small enough to solve naively) matches our more complex one
        let tt_table = moves_to_solve();
        for top_pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            for bottom_pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
                let pi = (top_pi, bottom_pi);
                let p = (top_pi.into(), bottom_pi.into());
                assert_eq!(tt_table.get(&p).unwrap().len(), pruning_table.solve(pi).len());
            }
        }
    }

    use three_trapezoids as tt;
    use three_trapezoids::inner as tt_inner;
    use three_trapezoids::outer as tt_outer;

    #[test]
    fn pruning_table_is_correct_for_three_trapezoids_inner_and_outer_with_no_symmetry_via_composite() {
        let inner_rep_table = Arc::new(RepresentativeTable::new::<tt_inner::ThreeTrapezoidsInner>());
        let inner_move_table: MoveTable<tt::NoSymmetry, tt_inner::ThreeTrapezoidsInnerIndex, tt::Turns> = MoveTable::new_on_pattern::<tt::ThreeTrapezoids, tt_inner::ThreeTrapezoidsInner>(inner_rep_table.clone());
        let inner_pruning_table: PruningTable<tt::NoSymmetry, tt_inner::ThreeTrapezoidsInnerIndex, _, tt::Turns, _> = PruningTable::new(inner_move_table, std::iter::once(tt_inner::ThreeTrapezoidsInner::from(tt::ThreeTrapezoids::identity()).into()));

        let outer_rep_table = Arc::new(RepresentativeTable::new::<tt_outer::ThreeTrapezoidsOuter>());
        let outer_move_table: MoveTable<tt::NoSymmetry, tt_outer::ThreeTrapezoidsOuterIndex, tt::Turns> = MoveTable::new_on_pattern::<tt::ThreeTrapezoids, tt_outer::ThreeTrapezoidsOuter>(outer_rep_table.clone());
        let outer_pruning_table: PruningTable<tt::NoSymmetry, tt_outer::ThreeTrapezoidsOuterIndex, _, tt::Turns, _> = PruningTable::new(outer_move_table, std::iter::once(tt_outer::ThreeTrapezoidsOuter::from(tt::ThreeTrapezoids::identity()).into()));
        let pruning_table = CompositePruningTable::new(inner_pruning_table, outer_pruning_table);

        // Our simple implementation (small enough to solve naively) matches our more complex one
        // NOTE: we can still expect the optimal solve here!
        let tt_table = tt::moves_to_solve();
        for pi in all::<tt::ThreeTrapezoidsIndex>() {
            let p: tt::ThreeTrapezoids = pi.into();
            let inner_pi: tt_inner::ThreeTrapezoidsInner = p.into();
            let outer_pi: tt_outer::ThreeTrapezoidsOuter = p.into();
            let cpi = (inner_pi.into(), outer_pi.into());
            assert_eq!(*tt_table.get(&pi).unwrap(), pruning_table.solve(cpi).len());
        }
    }

    #[test]
    fn pruning_table_is_correct_for_three_trapezoids_inner_and_outer_with_mirror_ud_symmetry_via_composite() {
        let inner_rep_table = Arc::new(RepresentativeTable::new::<tt_inner::ThreeTrapezoidsInner>());
        let inner_move_table: MoveTable<tt::MirrorUDSymmetry, tt_inner::ThreeTrapezoidsInnerIndex, tt::Turns> = MoveTable::new_on_pattern::<tt::ThreeTrapezoids, tt_inner::ThreeTrapezoidsInner>(inner_rep_table.clone());
        let inner_pruning_table: PruningTable<tt::MirrorUDSymmetry, tt_inner::ThreeTrapezoidsInnerIndex, _, tt::Turns, _> = PruningTable::new(inner_move_table, std::iter::once(tt_inner::ThreeTrapezoidsInner::from(tt::ThreeTrapezoids::identity()).into()));

        let outer_rep_table = Arc::new(RepresentativeTable::new::<tt_outer::ThreeTrapezoidsOuter>());
        let outer_move_table: MoveTable<tt::MirrorUDSymmetry, tt_outer::ThreeTrapezoidsOuterIndex, tt::Turns> = MoveTable::new_on_pattern::<tt::ThreeTrapezoids, tt_outer::ThreeTrapezoidsOuter>(outer_rep_table.clone());
        let outer_pruning_table: PruningTable<tt::MirrorUDSymmetry, tt_outer::ThreeTrapezoidsOuterIndex, _, tt::Turns, _> = PruningTable::new(outer_move_table, std::iter::once(tt_outer::ThreeTrapezoidsOuter::from(tt::ThreeTrapezoids::identity()).into()));
        let pruning_table = CompositePruningTable::new(inner_pruning_table, outer_pruning_table);

        // Our simple implementation (small enough to solve naively) matches our more complex one
        // NOTE: we can still expect the optimal solve here!
        let tt_table = tt::moves_to_solve();
        for pi in all::<tt::ThreeTrapezoidsIndex>() {
            let p: tt::ThreeTrapezoids = pi.into();
            let inner_pi: tt_inner::ThreeTrapezoidsInner = p.into();
            let outer_pi: tt_outer::ThreeTrapezoidsOuter = p.into();
            let cpi = (inner_pi.into(), outer_pi.into());
            assert_eq!(*tt_table.get(&pi).unwrap(), pruning_table.solve(cpi).len());
        }
    }

    #[test]
    fn pruning_table_is_correct_for_three_trapezoids_inner_and_outer_with_rotational_symmetry_via_composite() {
        let inner_rep_table = Arc::new(RepresentativeTable::new::<tt_inner::ThreeTrapezoidsInner>());
        let inner_move_table: MoveTable<tt::RotationalSymmetry, tt_inner::ThreeTrapezoidsInnerIndex, tt::Turns> = MoveTable::new_on_pattern::<tt::ThreeTrapezoids, tt_inner::ThreeTrapezoidsInner>(inner_rep_table.clone());
        let inner_pruning_table: PruningTable<tt::RotationalSymmetry, tt_inner::ThreeTrapezoidsInnerIndex, _, tt::Turns, _> = PruningTable::new(inner_move_table, std::iter::once(tt_inner::ThreeTrapezoidsInner::from(tt::ThreeTrapezoids::identity()).into()));

        let outer_rep_table = Arc::new(RepresentativeTable::new::<tt_outer::ThreeTrapezoidsOuter>());
        let outer_move_table: MoveTable<tt::RotationalSymmetry, tt_outer::ThreeTrapezoidsOuterIndex, tt::Turns> = MoveTable::new_on_pattern::<tt::ThreeTrapezoids, tt_outer::ThreeTrapezoidsOuter>(outer_rep_table.clone());
        let outer_pruning_table: PruningTable<tt::RotationalSymmetry, tt_outer::ThreeTrapezoidsOuterIndex, _, tt::Turns, _> = PruningTable::new(outer_move_table, std::iter::once(tt_outer::ThreeTrapezoidsOuter::from(tt::ThreeTrapezoids::identity()).into()));
        let pruning_table = CompositePruningTable::new(inner_pruning_table, outer_pruning_table);

        // Our simple implementation (small enough to solve naively) matches our more complex one
        // NOTE: we can still expect the optimal solve here!
        let tt_table = tt::moves_to_solve();
        for pi in all::<tt::ThreeTrapezoidsIndex>() {
            let p: tt::ThreeTrapezoids = pi.into();
            let inner_pi: tt_inner::ThreeTrapezoidsInner = p.into();
            let outer_pi: tt_outer::ThreeTrapezoidsOuter = p.into();
            let cpi = (inner_pi.into(), outer_pi.into());
            assert_eq!(*tt_table.get(&pi).unwrap(), pruning_table.solve(cpi).len());
        }
    }

    #[test]
    fn pruning_table_is_correct_for_three_trapezoids_inner_and_outer_with_full_symmetry_via_composite() {
        let inner_rep_table = Arc::new(RepresentativeTable::new::<tt_inner::ThreeTrapezoidsInner>());
        let inner_move_table: MoveTable<tt::FullSymmetry, tt_inner::ThreeTrapezoidsInnerIndex, tt::Turns> = MoveTable::new_on_pattern::<tt::ThreeTrapezoids, tt_inner::ThreeTrapezoidsInner>(inner_rep_table.clone());
        let inner_pruning_table: PruningTable<tt::FullSymmetry, tt_inner::ThreeTrapezoidsInnerIndex, _, tt::Turns, _> = PruningTable::new(inner_move_table, std::iter::once(tt_inner::ThreeTrapezoidsInner::from(tt::ThreeTrapezoids::identity()).into()));

        let outer_rep_table = Arc::new(RepresentativeTable::new::<tt_outer::ThreeTrapezoidsOuter>());
        let outer_move_table: MoveTable<tt::FullSymmetry, tt_outer::ThreeTrapezoidsOuterIndex, tt::Turns> = MoveTable::new_on_pattern::<tt::ThreeTrapezoids, tt_outer::ThreeTrapezoidsOuter>(outer_rep_table.clone());
        let outer_pruning_table: PruningTable<tt::FullSymmetry, tt_outer::ThreeTrapezoidsOuterIndex, _, tt::Turns, _> = PruningTable::new(outer_move_table, std::iter::once(tt_outer::ThreeTrapezoidsOuter::from(tt::ThreeTrapezoids::identity()).into()));
        let pruning_table = CompositePruningTable::new(inner_pruning_table, outer_pruning_table);

        // Our simple implementation (small enough to solve naively) matches our more complex one
        // NOTE: we can still expect the optimal solve here!
        let tt_table = tt::moves_to_solve();
        for pi in all::<tt::ThreeTrapezoidsIndex>() {
            let p: tt::ThreeTrapezoids = pi.into();
            let inner_pi: tt_inner::ThreeTrapezoidsInner = p.into();
            let outer_pi: tt_outer::ThreeTrapezoidsOuter = p.into();
            let cpi = (inner_pi.into(), outer_pi.into());
            assert_eq!(*tt_table.get(&pi).unwrap(), pruning_table.solve(cpi).len());
        }
    }
}
