use permutation_group::PermutationGroup as PG;
use invertable::Invertable;
use equivalence_class::EquivalenceClass;
use table_traits::{ TableTurn, TableRawIndexToSymIndex, TableRepCount };

use std::sync::Arc;
use std::collections::BTreeSet;
use std::collections::VecDeque;
use enum_iterator::{all, Sequence};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
// Opaque type to prevent accidental misuse
// NOTE: I don't think we need PermIndex to prevent possible confusions?
pub struct LowerBoundToken<Index, Turn>(u8, Index, std::marker::PhantomData<Turn>);

impl<Index: Copy, Turn> LowerBoundToken<Index, Turn> {
    pub fn new(i: Index, n: u8) -> Self {
        Self(n, i, std::marker::PhantomData)
    }

    pub fn get_index(&self) -> Index {
        self.1
    }

    pub fn get_lower_bound(&self) -> u8 {
        self.0
    }
}

#[derive(Debug)]
pub struct PruningTable<Sym, PermIndex, RepIndex, Turn, MoveTable> {
    table: Vec<u8>,
    move_table: MoveTable,
    goals: BTreeSet<RepIndex>,
    turns: std::marker::PhantomData<Turn>,
    syms: std::marker::PhantomData<Sym>,
    perm_index: std::marker::PhantomData<PermIndex>,
}

impl<MoveTable: TableTurn<Sym, RepIndex, Turn> + TableRawIndexToSymIndex<Sym, PermIndex, RepIndex> + TableRepCount, Turn: Sequence + Copy + Invertable + EquivalenceClass<Sym>, Sym: Sequence + Copy + Clone + PG, PermIndex: Sequence + Copy + Ord, RepIndex: Copy + Ord + Into<usize>> PruningTable<Sym, PermIndex, RepIndex, Turn, MoveTable> {
    // TODO: Hypothetically our pruning table could use a different Turn set
    // than our MoveTable.  We'd need the MoveTableTurn to be Invertable, but
    // not the PruningTableTurn.  PruningTableTurn must be From<MoveTableTurn>.
    // If we separate these out, when doing the discovery we would convert the
    // PruningTableTurn into a MoveTable turn _and then invert it_ because
    // we're walking away from the solved state.  Honestly, there still may be
    // problems with this, and I don't know if non-invertable turn twisty
    // puzzles even exist.
    pub fn new<I: Iterator<Item=PermIndex>>(move_table: MoveTable, goal_states: I) -> Self {
        let table_size = move_table.table_rep_count() / 4 + if move_table.table_rep_count() % 4 == 0 { 0 } else { 1 };
        let mut table = Vec::with_capacity(table_size);
        // 0b00 means (turns left `mod` 3 = 0)
        // 0b01 means (turns left `mod` 3 = 1)
        // 0b10 means (turns left `mod` 3 = 2)
        // 0b11 means uninitialized
        table.resize(table_size, 255);
        let mut queue = VecDeque::new();
        let mut goals = BTreeSet::new();
        for pi in goal_states {
            let (ri, _) = move_table.table_raw_index_to_sym_index(pi);
            let i: usize = ri.into();
            let was_new = set_if_new(&mut table, i, 0);
            if was_new {
                queue.push_back((ri, 1));
                goals.insert(ri);
            }
        }

        // TODO: Apparently switching to applying moves on empty entries once
        // the table is nearly full is a decent speed boost.
        loop {
            match queue.pop_front() {
                None => break,
                Some((ri, count)) => {
                    for t in all::<Turn>() {
                        let (ri, _) = move_table.table_turn(ri, t);
                        let i: usize = ri.into();
                        let was_new = set_if_new(&mut table, i, count % 3);
                        if was_new {
                            queue.push_back((ri, count + 1));
                        }
                    }
                },
            }
        }

        PruningTable {
            table,
            move_table,
            goals,
            turns: std::marker::PhantomData,
            syms: std::marker::PhantomData,
            perm_index: std::marker::PhantomData,
        }
    }

    // For perfect pruning tables, the lower_bound _is_ the remaining turn count.
    pub fn start_search(&self, pi: PermIndex) -> LowerBoundToken<(RepIndex, Sym), Turn> {
        let mut count = 0;
        let (ri0, s0) = self.move_table.table_raw_index_to_sym_index(pi);
        let mut ri = ri0;

        loop {
            if self.goals.contains(&ri) {
                break;
            } else {
                let target = (lookup(&self.table, ri.into()) + 2) % 3;
                let mut found = false;
                for t in all::<Turn>() {
                    let (candidate, _) = self.move_table.table_turn(ri, t);
                    if target == lookup(&self.table, candidate.into()) {
                        count += 1;
                        ri = candidate;
                        found = true;
                        break;
                    }
                }
                if found == false {
                    unreachable!("Pruning table should always make progress, but did not find a turn that got it closer to the goal.");
                }
            }
        }
        LowerBoundToken::new((ri0, s0), count)
    }

    pub fn continue_search(&self, lbt: LowerBoundToken<(RepIndex, Sym), Turn>, t: Turn) -> LowerBoundToken<(RepIndex, Sym), Turn> {
        let prev_lower_bound = lbt.get_lower_bound();
        let prev_m3 = prev_lower_bound % 3;
        let (ri0, s0) = lbt.get_index();
        let (ri1, s1) = self.move_table.table_turn(ri0, t.get_equivalent(&s0));
        let next_m3 = lookup(&self.table, ri1.into());

        let next_lower_bound = match (prev_m3, next_m3) {
            // Repetition avoids casting to i8
            (0, 0) => prev_lower_bound,
            (0, 1) => prev_lower_bound + 1,
            (0, 2) => prev_lower_bound - 1,
            (1, 0) => prev_lower_bound - 1,
            (1, 1) => prev_lower_bound,
            (1, 2) => prev_lower_bound + 1,
            (2, 0) => prev_lower_bound + 1,
            (2, 1) => prev_lower_bound - 1,
            (2, 2) => prev_lower_bound,
            (_, _) => unreachable!("Invariant violation: Pruning table values were not mod 3."),
        };

        LowerBoundToken::new((ri1, s0.permute(s1)), next_lower_bound)
    }

    /*
     * s' * pi * s = ri
     * pi = s * ri * s'
     * pi * t = s * ri * s' * t
     *
     * s * tx * s' = t
     * tx = s' * t * s
     *
     * pi * t = s * ri * s' * s * tx * s'
     * pi * t = s * ri * tx * s'
     *
     * s2' * ri * tx * s2 = ri2
     * ri * tx = s2 * ri2 * s2'
     *
     * pi * t = s * s2 * ri2 * s2' * s'
     * s2' * s' * pi * t * s * s2 = ri2
     */
    // TODO: This only works for perfect pruning tables.
    pub fn solve(&self, pi: PermIndex) -> Vec<Turn> {
        let mut turns: Vec<Turn> = Vec::new();
        let mut lbt = self.start_search(pi);

        loop {
            if lbt.get_lower_bound() == 0 {
                break turns;
            }

            for turn in all::<Turn>() {
                let lbt2 = self.continue_search(lbt, turn);
                if lbt2.get_lower_bound() < lbt.get_lower_bound() {
                    turns.push(turn);
                    lbt = lbt2;
                    break;
                }
            }
        }
    }
}

fn lookup(v: &Vec<u8>, i: usize) -> u8 {
    (v[i >> 2] >> (2 * (i & 3))) & 3
}

fn set_if_new(v: &mut Vec<u8>, i: usize, x: u8) -> bool {
    let ti = i >> 2;
    let offset = 2 * (i & 3);
    let initial_frame = v[ti];
    let initial_value = (initial_frame >> offset) & 3;
    let was_new = initial_value == 3;
    if was_new {
        v[ti] = initial_frame & (!(3 << offset) | (x << offset));
    }
    was_new
}

#[cfg(test)]
mod tests {
    use super::*;
    use two_triangles::*;
    use representative_table::*;
    use enum_iterator::all;
    use flat_move_table::MoveTable;
    use algebraic_actions::MagmaAction;

    #[test]
    fn pruning_table_is_correct_for_two_triangles_even_parity_without_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new::<TwoTriangles>());
        let move_table = Arc::new(MoveTable::new::<TwoTriangles>(rep_table));
        let pruning_table: PruningTable<NoSymmetry, TwoTrianglesEvenIndex, _, Turns, _> = PruningTable::new(move_table.clone(), std::iter::once(TwoTriangles::identity().into()));

        // Our simple implementation (TwoTriangles is small enough to solve naively) matches our more complex one
        let tt_table = moves_to_solve();
        for pi in all::<TwoTrianglesEvenIndex>() {
            let lbt = pruning_table.start_search(pi);
            assert_eq!(*tt_table.get(&pi).unwrap(), lbt.get_lower_bound() as usize);
            for t in all::<Turns>() {
                let p: TwoTriangles = pi.into();
                let turned = p.permute(t.into()).into();
                assert_eq!(*tt_table.get(&turned).unwrap(), pruning_table.continue_search(lbt, t).get_lower_bound() as usize);
            }
        }

        // Solves the puzzle
        for pi in all::<TwoTrianglesEvenIndex>() {
            let mut turns = pruning_table.solve(pi);
            turns.reverse();
            let mut p = TwoTriangles::identity();
            for t in turns {
                p = p.permute(t.invert().into());
            }
            assert_eq!(pi, p.into());
        }
    }

    #[test]
    fn pruning_table_is_correct_for_two_triangles_even_parity_with_rotational_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new::<TwoTriangles>());
        let move_table = Arc::new(MoveTable::new::<TwoTriangles>(rep_table));
        let pruning_table: PruningTable<RotationalSymmetry, TwoTrianglesEvenIndex, _, Turns, _> = PruningTable::new(move_table.clone(), std::iter::once(TwoTriangles::identity().into()));

        // Our simple implementation (TwoTriangles is small enough to solve naively) matches our more complex one
        let tt_table = moves_to_solve();
        for pi in all::<TwoTrianglesEvenIndex>() {
            let lbt = pruning_table.start_search(pi);
            assert_eq!(*tt_table.get(&pi).unwrap(), lbt.get_lower_bound() as usize);
            for t in all::<Turns>() {
                let p: TwoTriangles = pi.into();
                let turned = p.permute(t.into()).into();
                assert_eq!(*tt_table.get(&turned).unwrap(), pruning_table.continue_search(lbt, t).get_lower_bound() as usize);
            }
        }

        // Solves the puzzle
        for pi in all::<TwoTrianglesEvenIndex>() {
            let mut turns = pruning_table.solve(pi);
            turns.reverse();
            let mut p = TwoTriangles::identity();
            for t in turns {
                p = p.permute(t.invert().into());
            }
            assert_eq!(pi, p.into());
        }
    }

    #[test]
    fn pruning_table_is_correct_for_two_triangles_even_parity_with_full_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new::<TwoTriangles>());
        let move_table = Arc::new(MoveTable::new::<TwoTriangles>(rep_table));
        let pruning_table: PruningTable<FullSymmetry, TwoTrianglesEvenIndex, _, Turns, _> = PruningTable::new(move_table.clone(), std::iter::once(TwoTriangles::identity().into()));

        // Our simple implementation (TwoTriangles is small enough to solve naively) matches our more complex one
        let tt_table = moves_to_solve();
        for pi in all::<TwoTrianglesEvenIndex>() {
            let lbt = pruning_table.start_search(pi);
            assert_eq!(*tt_table.get(&pi).unwrap(), lbt.get_lower_bound() as usize);
            for t in all::<Turns>() {
                let p: TwoTriangles = pi.into();
                let turned = p.permute(t.into()).into();
                assert_eq!(*tt_table.get(&turned).unwrap(), pruning_table.continue_search(lbt, t).get_lower_bound() as usize);
            }
        }

        // Solves the puzzle
        for pi in all::<TwoTrianglesEvenIndex>() {
            let mut turns = pruning_table.solve(pi);
            turns.reverse();
            let mut p = TwoTriangles::identity();
            for t in turns {
                p = p.permute(t.invert().into());
            }
            assert_eq!(pi, p.into());
        }
    }

    use three_triangles;

    #[test]
    fn pruning_table_is_correct_for_three_triangles_even_parity_without_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new::<three_triangles::ThreeTriangles>());
        let move_table = Arc::new(MoveTable::new::<three_triangles::ThreeTriangles>(rep_table));
        let pruning_table: PruningTable<three_triangles::NoSymmetry, three_triangles::ThreeTrianglesEvenIndex, _, three_triangles::Turns, _> = PruningTable::new(move_table.clone(), std::iter::once(three_triangles::ThreeTriangles::identity().into()));

        // Our simple implementation (three_triangles::ThreeTriangles is small enough to solve naively) matches our more complex one
        let tt_table = three_triangles::moves_to_solve();
        for pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            let lbt = pruning_table.start_search(pi);
            assert_eq!(*tt_table.get(&pi).unwrap(), lbt.get_lower_bound() as usize);
            for t in all::<three_triangles::Turns>() {
                let p: three_triangles::ThreeTriangles = pi.into();
                let turned = p.permute(t.into()).into();
                assert_eq!(*tt_table.get(&turned).unwrap(), pruning_table.continue_search(lbt, t).get_lower_bound() as usize);
            }
        }
    }

    #[test]
    fn pruning_table_is_correct_for_three_triangles_even_parity_with_rotational_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new::<three_triangles::ThreeTriangles>());
        let move_table = Arc::new(MoveTable::new::<three_triangles::ThreeTriangles>(rep_table));
        let pruning_table: PruningTable<three_triangles::RotationalSymmetry, three_triangles::ThreeTrianglesEvenIndex, _, three_triangles::Turns, _> = PruningTable::new(move_table.clone(), std::iter::once(three_triangles::ThreeTriangles::identity().into()));

        // Our simple implementation (three_triangles::ThreeTriangles is small enough to solve naively) matches our more complex one
        let tt_table = three_triangles::moves_to_solve();
        for pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            let lbt = pruning_table.start_search(pi);
            assert_eq!(*tt_table.get(&pi).unwrap(), lbt.get_lower_bound() as usize);
            for t in all::<three_triangles::Turns>() {
                let p: three_triangles::ThreeTriangles = pi.into();
                let turned = p.permute(t.into()).into();
                assert_eq!(*tt_table.get(&turned).unwrap(), pruning_table.continue_search(lbt, t).get_lower_bound() as usize);
            }
        }
    }

    #[test]
    fn pruning_table_is_correct_for_three_triangles_even_parity_with_full_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new::<three_triangles::ThreeTriangles>());
        let move_table = Arc::new(MoveTable::new::<three_triangles::ThreeTriangles>(rep_table));
        let pruning_table: PruningTable<three_triangles::FullSymmetry, three_triangles::ThreeTrianglesEvenIndex, _, three_triangles::Turns, _> = PruningTable::new(move_table.clone(), std::iter::once(three_triangles::ThreeTriangles::identity().into()));

        // Our simple implementation (three_triangles::ThreeTriangles is small enough to solve naively) matches our more complex one
        let tt_table = three_triangles::moves_to_solve();
        for pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            let lbt = pruning_table.start_search(pi);
            assert_eq!(*tt_table.get(&pi).unwrap(), lbt.get_lower_bound() as usize);
            for t in all::<three_triangles::Turns>() {
                let p: three_triangles::ThreeTriangles = pi.into();
                let turned = p.permute(t.into()).into();
                assert_eq!(*tt_table.get(&turned).unwrap(), pruning_table.continue_search(lbt, t).get_lower_bound() as usize);
            }
        }

        // Solves the puzzle
        for pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            let mut turns = pruning_table.solve(pi);
            turns.reverse();
            let mut p = three_triangles::ThreeTriangles::identity();
            for t in turns {
                p = p.permute(t.invert().into());
            }
            assert_eq!(pi, p.into());
        }
    }

    use three_trapezoids;

    #[test]
    fn pruning_table_is_correct_for_three_trapezoids_with_no_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new::<three_trapezoids::ThreeTrapezoids>());
        let move_table = Arc::new(MoveTable::new::<three_trapezoids::ThreeTrapezoids>(rep_table));
        let pruning_table: PruningTable<three_trapezoids::NoSymmetry, three_trapezoids::ThreeTrapezoidsIndex, _, three_trapezoids::Turns, _> = PruningTable::new(move_table.clone(), std::iter::once(three_trapezoids::ThreeTrapezoids::identity().into()));

        // Our simple implementation (three_trapezoids::ThreeTrapezoids is small enough to solve naively) matches our more complex one
        let tt_table = three_trapezoids::moves_to_solve();
        for pi in all::<three_trapezoids::ThreeTrapezoidsIndex>() {
            let lbt = pruning_table.start_search(pi);
            assert_eq!(*tt_table.get(&pi).unwrap(), lbt.get_lower_bound() as usize);
            for t in all::<three_trapezoids::Turns>() {
                let p: three_trapezoids::ThreeTrapezoids = pi.into();
                let turned = p.permute(t.into()).into();
                assert_eq!(*tt_table.get(&turned).unwrap(), pruning_table.continue_search(lbt, t).get_lower_bound() as usize);
            }
        }

        // Solves the puzzle
        for pi in all::<three_trapezoids::ThreeTrapezoidsIndex>() {
            let mut turns = pruning_table.solve(pi);
            turns.reverse();
            let mut p = three_trapezoids::ThreeTrapezoids::identity();
            for t in turns {
                p = p.permute(t.invert().into());
            }
            assert_eq!(pi, p.into());
        }
    }

    #[test]
    fn pruning_table_is_correct_for_three_trapezoids_with_mirror_ud_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new::<three_trapezoids::ThreeTrapezoids>());
        let move_table = Arc::new(MoveTable::new::<three_trapezoids::ThreeTrapezoids>(rep_table));
        let pruning_table: PruningTable<three_trapezoids::MirrorUDSymmetry, three_trapezoids::ThreeTrapezoidsIndex, _, three_trapezoids::Turns, _> = PruningTable::new(move_table.clone(), std::iter::once(three_trapezoids::ThreeTrapezoids::identity().into()));

        // Our simple implementation (three_trapezoids::ThreeTrapezoids is small enough to solve naively) matches our more complex one
        let tt_table = three_trapezoids::moves_to_solve();
        for pi in all::<three_trapezoids::ThreeTrapezoidsIndex>() {
            let lbt = pruning_table.start_search(pi);
            assert_eq!(*tt_table.get(&pi).unwrap(), lbt.get_lower_bound() as usize);
            for t in all::<three_trapezoids::Turns>() {
                let p: three_trapezoids::ThreeTrapezoids = pi.into();
                let turned = p.permute(t.into()).into();
                assert_eq!(*tt_table.get(&turned).unwrap(), pruning_table.continue_search(lbt, t).get_lower_bound() as usize);
            }
        }

        // Solves the puzzle
        for pi in all::<three_trapezoids::ThreeTrapezoidsIndex>() {
            let mut turns = pruning_table.solve(pi);
            turns.reverse();
            let mut p = three_trapezoids::ThreeTrapezoids::identity();
            for t in turns {
                p = p.permute(t.invert().into());
            }
            assert_eq!(pi, p.into());
        }
    }

    #[test]
    fn pruning_table_is_correct_for_three_trapezoids_with_rotational_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new::<three_trapezoids::ThreeTrapezoids>());
        let move_table = Arc::new(MoveTable::new::<three_trapezoids::ThreeTrapezoids>(rep_table));
        let pruning_table: PruningTable<three_trapezoids::RotationalSymmetry, three_trapezoids::ThreeTrapezoidsIndex, _, three_trapezoids::Turns, _> = PruningTable::new(move_table.clone(), std::iter::once(three_trapezoids::ThreeTrapezoids::identity().into()));

        // Our simple implementation (three_trapezoids::ThreeTrapezoids is small enough to solve naively) matches our more complex one
        let tt_table = three_trapezoids::moves_to_solve();
        for pi in all::<three_trapezoids::ThreeTrapezoidsIndex>() {
            let lbt = pruning_table.start_search(pi);
            assert_eq!(*tt_table.get(&pi).unwrap(), lbt.get_lower_bound() as usize);
            for t in all::<three_trapezoids::Turns>() {
                let p: three_trapezoids::ThreeTrapezoids = pi.into();
                let turned = p.permute(t.into()).into();
                assert_eq!(*tt_table.get(&turned).unwrap(), pruning_table.continue_search(lbt, t).get_lower_bound() as usize);
            }
        }

        // Solves the puzzle
        for pi in all::<three_trapezoids::ThreeTrapezoidsIndex>() {
            let mut turns = pruning_table.solve(pi);
            turns.reverse();
            let mut p = three_trapezoids::ThreeTrapezoids::identity();
            for t in turns {
                p = p.permute(t.invert().into());
            }
            assert_eq!(pi, p.into());
        }
    }

    #[test]
    fn pruning_table_is_correct_for_three_trapezoids_with_full_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new::<three_trapezoids::ThreeTrapezoids>());
        let move_table = Arc::new(MoveTable::new::<three_trapezoids::ThreeTrapezoids>(rep_table));
        let pruning_table: PruningTable<three_trapezoids::FullSymmetry, three_trapezoids::ThreeTrapezoidsIndex, _, three_trapezoids::Turns, _> = PruningTable::new(move_table.clone(), std::iter::once(three_trapezoids::ThreeTrapezoids::identity().into()));

        // Our simple implementation (three_trapezoids::ThreeTrapezoids is small enough to solve naively) matches our more complex one
        let tt_table = three_trapezoids::moves_to_solve();
        for pi in all::<three_trapezoids::ThreeTrapezoidsIndex>() {
            let lbt = pruning_table.start_search(pi);
            assert_eq!(*tt_table.get(&pi).unwrap(), lbt.get_lower_bound() as usize);
            for t in all::<three_trapezoids::Turns>() {
                let p: three_trapezoids::ThreeTrapezoids = pi.into();
                let turned = p.permute(t.into()).into();
                assert_eq!(*tt_table.get(&turned).unwrap(), pruning_table.continue_search(lbt, t).get_lower_bound() as usize);
            }
        }

        // Solves the puzzle
        for pi in all::<three_trapezoids::ThreeTrapezoidsIndex>() {
            let mut turns = pruning_table.solve(pi);
            turns.reverse();
            let mut p = three_trapezoids::ThreeTrapezoids::identity();
            for t in turns {
                p = p.permute(t.invert().into());
            }
            assert_eq!(pi, p.into());
        }
    }

    #[test]
    fn pruning_table_is_correct_for_three_trapezoids_inner_with_no_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new::<three_trapezoids::inner::ThreeTrapezoidsInner>());
        let move_table = Arc::new(MoveTable::new_on_pattern::<three_trapezoids::ThreeTrapezoids, three_trapezoids::inner::ThreeTrapezoidsInner>(rep_table));
        let pruning_table: PruningTable<three_trapezoids::NoSymmetry, three_trapezoids::inner::ThreeTrapezoidsInnerIndex, _, three_trapezoids::Turns, _> = PruningTable::new(move_table.clone(), std::iter::once(three_trapezoids::inner::ThreeTrapezoidsInner::from(three_trapezoids::ThreeTrapezoids::identity()).into()));

        // Our simple implementation (three_trapezoids::ThreeTrapezoids is small enough to solve naively) matches our more complex one
        let tt_table = three_trapezoids::inner::moves_to_solve();
        for pi in all::<three_trapezoids::inner::ThreeTrapezoidsInnerIndex>() {
            let lbt = pruning_table.start_search(pi);
            assert_eq!(*tt_table.get(&pi).unwrap(), lbt.get_lower_bound() as usize);
            for t in all::<three_trapezoids::Turns>() {
                let p: three_trapezoids::inner::ThreeTrapezoidsInner = pi.into();
                let turn: three_trapezoids::ThreeTrapezoids = t.into();
                let turned = p.act(turn).into();
                assert_eq!(*tt_table.get(&turned).unwrap(), pruning_table.continue_search(lbt, t).get_lower_bound() as usize);
            }
        }

        // Solves the puzzle
        for pi in all::<three_trapezoids::inner::ThreeTrapezoidsInnerIndex>() {
            let mut turns = pruning_table.solve(pi);
            turns.reverse();
            let mut p: three_trapezoids::inner::ThreeTrapezoidsInner = three_trapezoids::ThreeTrapezoids::identity().into();
            for t in turns {
                let t: three_trapezoids::ThreeTrapezoids = t.into();
                p = p.act(t.invert());
            }
            assert_eq!(pi, p.into());
        }
    }

    #[test]
    fn pruning_table_is_correct_for_three_trapezoids_inner_with_mirror_ud_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new::<three_trapezoids::inner::ThreeTrapezoidsInner>());
        let move_table = Arc::new(MoveTable::new_on_pattern::<three_trapezoids::ThreeTrapezoids, three_trapezoids::inner::ThreeTrapezoidsInner>(rep_table));
        let pruning_table: PruningTable<three_trapezoids::MirrorUDSymmetry, three_trapezoids::inner::ThreeTrapezoidsInnerIndex, _, three_trapezoids::Turns, _> = PruningTable::new(move_table.clone(), std::iter::once(three_trapezoids::inner::ThreeTrapezoidsInner::from(three_trapezoids::ThreeTrapezoids::identity()).into()));

        // Our simple implementation (three_trapezoids::ThreeTrapezoids is small enough to solve naively) matches our more complex one
        let tt_table = three_trapezoids::inner::moves_to_solve();
        for pi in all::<three_trapezoids::inner::ThreeTrapezoidsInnerIndex>() {
            let lbt = pruning_table.start_search(pi);
            assert_eq!(*tt_table.get(&pi).unwrap(), lbt.get_lower_bound() as usize);
            for t in all::<three_trapezoids::Turns>() {
                let p: three_trapezoids::inner::ThreeTrapezoidsInner = pi.into();
                let turn: three_trapezoids::ThreeTrapezoids = t.into();
                let turned = p.act(turn).into();
                assert_eq!(*tt_table.get(&turned).unwrap(), pruning_table.continue_search(lbt, t).get_lower_bound() as usize);
            }
        }

        // Solves the puzzle
        for pi in all::<three_trapezoids::inner::ThreeTrapezoidsInnerIndex>() {
            let mut turns = pruning_table.solve(pi);
            turns.reverse();
            let mut p: three_trapezoids::inner::ThreeTrapezoidsInner = three_trapezoids::ThreeTrapezoids::identity().into();
            for t in turns {
                let t: three_trapezoids::ThreeTrapezoids = t.into();
                p = p.act(t.invert());
            }
            assert_eq!(pi, p.into());
        }
    }

    #[test]
    fn pruning_table_is_correct_for_three_trapezoids_inner_with_rotational_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new::<three_trapezoids::inner::ThreeTrapezoidsInner>());
        let move_table = Arc::new(MoveTable::new_on_pattern::<three_trapezoids::ThreeTrapezoids, three_trapezoids::inner::ThreeTrapezoidsInner>(rep_table));
        let pruning_table: PruningTable<three_trapezoids::RotationalSymmetry, three_trapezoids::inner::ThreeTrapezoidsInnerIndex, _, three_trapezoids::Turns, _> = PruningTable::new(move_table.clone(), std::iter::once(three_trapezoids::inner::ThreeTrapezoidsInner::from(three_trapezoids::ThreeTrapezoids::identity()).into()));

        // Our simple implementation (three_trapezoids::ThreeTrapezoids is small enough to solve naively) matches our more complex one
        let tt_table = three_trapezoids::inner::moves_to_solve();
        for pi in all::<three_trapezoids::inner::ThreeTrapezoidsInnerIndex>() {
            let lbt = pruning_table.start_search(pi);
            assert_eq!(*tt_table.get(&pi).unwrap(), lbt.get_lower_bound() as usize);
            for t in all::<three_trapezoids::Turns>() {
                let p: three_trapezoids::inner::ThreeTrapezoidsInner = pi.into();
                let turn: three_trapezoids::ThreeTrapezoids = t.into();
                let turned = p.act(turn).into();
                assert_eq!(*tt_table.get(&turned).unwrap(), pruning_table.continue_search(lbt, t).get_lower_bound() as usize);
            }
        }

        // Solves the puzzle
        for pi in all::<three_trapezoids::inner::ThreeTrapezoidsInnerIndex>() {
            let mut turns = pruning_table.solve(pi);
            turns.reverse();
            let mut p: three_trapezoids::inner::ThreeTrapezoidsInner = three_trapezoids::ThreeTrapezoids::identity().into();
            for t in turns {
                let t: three_trapezoids::ThreeTrapezoids = t.into();
                p = p.act(t.invert());
            }
            assert_eq!(pi, p.into());
        }
    }

    #[test]
    fn pruning_table_is_correct_for_three_trapezoids_inner_with_full_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new::<three_trapezoids::inner::ThreeTrapezoidsInner>());
        let move_table = Arc::new(MoveTable::new_on_pattern::<three_trapezoids::ThreeTrapezoids, three_trapezoids::inner::ThreeTrapezoidsInner>(rep_table));
        let pruning_table: PruningTable<three_trapezoids::FullSymmetry, three_trapezoids::inner::ThreeTrapezoidsInnerIndex, _, three_trapezoids::Turns, _> = PruningTable::new(move_table.clone(), std::iter::once(three_trapezoids::inner::ThreeTrapezoidsInner::from(three_trapezoids::ThreeTrapezoids::identity()).into()));

        // Our simple implementation (three_trapezoids::ThreeTrapezoids is small enough to solve naively) matches our more complex one
        let tt_table = three_trapezoids::inner::moves_to_solve();
        for pi in all::<three_trapezoids::inner::ThreeTrapezoidsInnerIndex>() {
            let lbt = pruning_table.start_search(pi);
            assert_eq!(*tt_table.get(&pi).unwrap(), lbt.get_lower_bound() as usize);
            for t in all::<three_trapezoids::Turns>() {
                let p: three_trapezoids::inner::ThreeTrapezoidsInner = pi.into();
                let turn: three_trapezoids::ThreeTrapezoids = t.into();
                let turned = p.act(turn).into();
                assert_eq!(*tt_table.get(&turned).unwrap(), pruning_table.continue_search(lbt, t).get_lower_bound() as usize);
            }
        }

        // Solves the puzzle
        for pi in all::<three_trapezoids::inner::ThreeTrapezoidsInnerIndex>() {
            let mut turns = pruning_table.solve(pi);
            turns.reverse();
            let mut p: three_trapezoids::inner::ThreeTrapezoidsInner = three_trapezoids::ThreeTrapezoids::identity().into();
            for t in turns {
                let t: three_trapezoids::ThreeTrapezoids = t.into();
                p = p.act(t.invert());
            }
            assert_eq!(pi, p.into());
        }
    }

    #[test]
    fn pruning_table_is_correct_for_three_trapezoids_outer_with_no_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new::<three_trapezoids::outer::ThreeTrapezoidsOuter>());
        let move_table = Arc::new(MoveTable::new_on_pattern::<three_trapezoids::ThreeTrapezoids, three_trapezoids::outer::ThreeTrapezoidsOuter>(rep_table));
        let pruning_table: PruningTable<three_trapezoids::NoSymmetry, three_trapezoids::outer::ThreeTrapezoidsOuterIndex, _, three_trapezoids::Turns, _> = PruningTable::new(move_table.clone(), std::iter::once(three_trapezoids::outer::ThreeTrapezoidsOuter::from(three_trapezoids::ThreeTrapezoids::identity()).into()));

        // Our simple implementation (three_trapezoids::ThreeTrapezoids is small enough to solve naively) matches our more complex one
        let tt_table = three_trapezoids::outer::moves_to_solve();
        for pi in all::<three_trapezoids::outer::ThreeTrapezoidsOuterIndex>() {
            let lbt = pruning_table.start_search(pi);
            assert_eq!(*tt_table.get(&pi).unwrap(), lbt.get_lower_bound() as usize);
            for t in all::<three_trapezoids::Turns>() {
                let p: three_trapezoids::outer::ThreeTrapezoidsOuter = pi.into();
                let turn: three_trapezoids::ThreeTrapezoids = t.into();
                let turned = p.act(turn).into();
                assert_eq!(*tt_table.get(&turned).unwrap(), pruning_table.continue_search(lbt, t).get_lower_bound() as usize);
            }
        }

        // Solves the puzzle
        for pi in all::<three_trapezoids::outer::ThreeTrapezoidsOuterIndex>() {
            let mut turns = pruning_table.solve(pi);
            turns.reverse();
            let mut p: three_trapezoids::outer::ThreeTrapezoidsOuter = three_trapezoids::ThreeTrapezoids::identity().into();
            for t in turns {
                let t: three_trapezoids::ThreeTrapezoids = t.into();
                p = p.act(t.invert());
            }
            assert_eq!(pi, p.into());
        }
    }

    #[test]
    fn pruning_table_is_correct_for_three_trapezoids_outer_with_mirror_ud_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new::<three_trapezoids::outer::ThreeTrapezoidsOuter>());
        let move_table = Arc::new(MoveTable::new_on_pattern::<three_trapezoids::ThreeTrapezoids, three_trapezoids::outer::ThreeTrapezoidsOuter>(rep_table));
        let pruning_table: PruningTable<three_trapezoids::MirrorUDSymmetry, three_trapezoids::outer::ThreeTrapezoidsOuterIndex, _, three_trapezoids::Turns, _> = PruningTable::new(move_table.clone(), std::iter::once(three_trapezoids::outer::ThreeTrapezoidsOuter::from(three_trapezoids::ThreeTrapezoids::identity()).into()));

        // Our simple implementation (three_trapezoids::ThreeTrapezoids is small enough to solve naively) matches our more complex one
        let tt_table = three_trapezoids::outer::moves_to_solve();
        for pi in all::<three_trapezoids::outer::ThreeTrapezoidsOuterIndex>() {
            let lbt = pruning_table.start_search(pi);
            assert_eq!(*tt_table.get(&pi).unwrap(), lbt.get_lower_bound() as usize);
            for t in all::<three_trapezoids::Turns>() {
                let p: three_trapezoids::outer::ThreeTrapezoidsOuter = pi.into();
                let turn: three_trapezoids::ThreeTrapezoids = t.into();
                let turned = p.act(turn).into();
                assert_eq!(*tt_table.get(&turned).unwrap(), pruning_table.continue_search(lbt, t).get_lower_bound() as usize);
            }
        }

        // Solves the puzzle
        for pi in all::<three_trapezoids::outer::ThreeTrapezoidsOuterIndex>() {
            let mut turns = pruning_table.solve(pi);
            turns.reverse();
            let mut p: three_trapezoids::outer::ThreeTrapezoidsOuter = three_trapezoids::ThreeTrapezoids::identity().into();
            for t in turns {
                let t: three_trapezoids::ThreeTrapezoids = t.into();
                p = p.act(t.invert());
            }
            assert_eq!(pi, p.into());
        }
    }

    #[test]
    fn pruning_table_is_correct_for_three_trapezoids_outer_with_rotational_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new::<three_trapezoids::outer::ThreeTrapezoidsOuter>());
        let move_table = Arc::new(MoveTable::new_on_pattern::<three_trapezoids::ThreeTrapezoids, three_trapezoids::outer::ThreeTrapezoidsOuter>(rep_table));
        let pruning_table: PruningTable<three_trapezoids::RotationalSymmetry, three_trapezoids::outer::ThreeTrapezoidsOuterIndex, _, three_trapezoids::Turns, _> = PruningTable::new(move_table.clone(), std::iter::once(three_trapezoids::outer::ThreeTrapezoidsOuter::from(three_trapezoids::ThreeTrapezoids::identity()).into()));

        // Our simple implementation (three_trapezoids::ThreeTrapezoids is small enough to solve naively) matches our more complex one
        let tt_table = three_trapezoids::outer::moves_to_solve();
        for pi in all::<three_trapezoids::outer::ThreeTrapezoidsOuterIndex>() {
            let lbt = pruning_table.start_search(pi);
            assert_eq!(*tt_table.get(&pi).unwrap(), lbt.get_lower_bound() as usize);
            for t in all::<three_trapezoids::Turns>() {
                let p: three_trapezoids::outer::ThreeTrapezoidsOuter = pi.into();
                let turn: three_trapezoids::ThreeTrapezoids = t.into();
                let turned = p.act(turn).into();
                assert_eq!(*tt_table.get(&turned).unwrap(), pruning_table.continue_search(lbt, t).get_lower_bound() as usize);
            }
        }

        // Solves the puzzle
        for pi in all::<three_trapezoids::outer::ThreeTrapezoidsOuterIndex>() {
            let mut turns = pruning_table.solve(pi);
            turns.reverse();
            let mut p: three_trapezoids::outer::ThreeTrapezoidsOuter = three_trapezoids::ThreeTrapezoids::identity().into();
            for t in turns {
                let t: three_trapezoids::ThreeTrapezoids = t.into();
                p = p.act(t.invert());
            }
            assert_eq!(pi, p.into());
        }
    }

    #[test]
    fn pruning_table_is_correct_for_three_trapezoids_outer_with_full_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new::<three_trapezoids::outer::ThreeTrapezoidsOuter>());
        let move_table = Arc::new(MoveTable::new_on_pattern::<three_trapezoids::ThreeTrapezoids, three_trapezoids::outer::ThreeTrapezoidsOuter>(rep_table));
        let pruning_table: PruningTable<three_trapezoids::FullSymmetry, three_trapezoids::outer::ThreeTrapezoidsOuterIndex, _, three_trapezoids::Turns, _> = PruningTable::new(move_table.clone(), std::iter::once(three_trapezoids::outer::ThreeTrapezoidsOuter::from(three_trapezoids::ThreeTrapezoids::identity()).into()));

        // Our simple implementation (three_trapezoids::ThreeTrapezoids is small enough to solve naively) matches our more complex one
        let tt_table = three_trapezoids::outer::moves_to_solve();
        for pi in all::<three_trapezoids::outer::ThreeTrapezoidsOuterIndex>() {
            let lbt = pruning_table.start_search(pi);
            assert_eq!(*tt_table.get(&pi).unwrap(), lbt.get_lower_bound() as usize);
            for t in all::<three_trapezoids::Turns>() {
                let p: three_trapezoids::outer::ThreeTrapezoidsOuter = pi.into();
                let turn: three_trapezoids::ThreeTrapezoids = t.into();
                let turned = p.act(turn).into();
                assert_eq!(*tt_table.get(&turned).unwrap(), pruning_table.continue_search(lbt, t).get_lower_bound() as usize);
            }
        }

        // Solves the puzzle
        for pi in all::<three_trapezoids::outer::ThreeTrapezoidsOuterIndex>() {
            let mut turns = pruning_table.solve(pi);
            turns.reverse();
            let mut p: three_trapezoids::outer::ThreeTrapezoidsOuter = three_trapezoids::ThreeTrapezoids::identity().into();
            for t in turns {
                let t: three_trapezoids::ThreeTrapezoids = t.into();
                p = p.act(t.invert());
            }
            assert_eq!(pi, p.into());
        }
    }

    use three_triangles_stack;
    use composite_move_table::CompositeMoveTable;

    #[test]
    fn pruning_table_is_correct_for_three_triangles_stack_even_parity_with_no_symmetry_via_composite() {
        // TODO: Ideally these use the same move table!
        let top_rep_table = Arc::new(RepresentativeTable::new::<three_triangles_stack::TopThreeTriangles>());
        let top_move_table: MoveTable<three_triangles_stack::NoSymmetry, three_triangles::ThreeTrianglesEvenIndex, three_triangles_stack::Turns> = MoveTable::new::<three_triangles_stack::TopThreeTriangles>(top_rep_table.clone());

        let bottom_rep_table = Arc::new(RepresentativeTable::new::<three_triangles_stack::BottomThreeTriangles>());
        let bottom_move_table: MoveTable<three_triangles_stack::NoSymmetry, three_triangles::ThreeTrianglesEvenIndex, three_triangles_stack::Turns> = MoveTable::new::<three_triangles_stack::BottomThreeTriangles>(bottom_rep_table.clone());
        let move_table = Arc::new(CompositeMoveTable::new(Arc::new(top_move_table), Arc::new(bottom_move_table)));
        let pruning_table: PruningTable<three_triangles_stack::NoSymmetry, (three_triangles::ThreeTrianglesEvenIndex, three_triangles::ThreeTrianglesEvenIndex), _, three_triangles_stack::Turns, _> = PruningTable::new(move_table.clone(), std::iter::once((three_triangles::ThreeTriangles::identity().into(), three_triangles::ThreeTriangles::identity().into())));

        // Our simple implementation (three_triangles_stack is small enough to solve naively) matches our more complex one
        let tt_table = three_triangles_stack::moves_to_solve();
        for top_pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            for bottom_pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
                let pi = (top_pi, bottom_pi);
                let p = (top_pi.into(), bottom_pi.into());
                let lbt = pruning_table.start_search(pi);
                assert_eq!(tt_table.get(&p).unwrap().len(), lbt.get_lower_bound() as usize);
                for t in all::<three_triangles_stack::Turns>() {
                    let top_turned = p.0.permute(t.into());
                    let bottom_turned = p.1.permute(t.into());
                    let turned = (top_turned.into(), bottom_turned.into());
                    assert_eq!(tt_table.get(&turned).unwrap().len(), pruning_table.continue_search(lbt, t).get_lower_bound() as usize);
                }
            }
        }

        // Solves the puzzle
        for top_pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            for bottom_pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
                let pi = (top_pi, bottom_pi);
                let mut turns = pruning_table.solve(pi);
                turns.reverse();
                let mut top_p = three_triangles_stack::TopThreeTriangles::identity();
                let mut bottom_p = three_triangles_stack::BottomThreeTriangles::identity();
                for t in turns {
                    top_p = top_p.permute(t.invert().into());
                    bottom_p = bottom_p.permute(t.invert().into());
                }
                assert_eq!(pi, (top_p.into(), bottom_p.into()));
            }
        }
    }

    #[test]
    fn pruning_table_is_correct_for_three_triangles_stack_even_parity_with_mirror_ud_symmetry_via_composite() {
        // TODO: Ideally these use the same move table!
        let top_rep_table = Arc::new(RepresentativeTable::new::<three_triangles_stack::TopThreeTriangles>());
        let top_move_table: MoveTable<three_triangles_stack::MirrorUDSymmetry, three_triangles::ThreeTrianglesEvenIndex, three_triangles_stack::Turns> = MoveTable::new::<three_triangles_stack::TopThreeTriangles>(top_rep_table.clone());

        let bottom_rep_table = Arc::new(RepresentativeTable::new::<three_triangles_stack::BottomThreeTriangles>());
        let bottom_move_table: MoveTable<three_triangles_stack::MirrorUDSymmetry, three_triangles::ThreeTrianglesEvenIndex, three_triangles_stack::Turns> = MoveTable::new::<three_triangles_stack::BottomThreeTriangles>(bottom_rep_table.clone());
        let move_table = Arc::new(CompositeMoveTable::new(Arc::new(top_move_table), Arc::new(bottom_move_table)));
        let pruning_table: PruningTable<three_triangles_stack::MirrorUDSymmetry, (three_triangles::ThreeTrianglesEvenIndex, three_triangles::ThreeTrianglesEvenIndex), _, three_triangles_stack::Turns, _> = PruningTable::new(move_table.clone(), std::iter::once((three_triangles::ThreeTriangles::identity().into(), three_triangles::ThreeTriangles::identity().into())));

        // Our simple implementation (three_triangles_stack is small enough to solve naively) matches our more complex one
        let tt_table = three_triangles_stack::moves_to_solve();
        for top_pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            for bottom_pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
                let pi = (top_pi, bottom_pi);
                let p = (top_pi.into(), bottom_pi.into());
                let lbt = pruning_table.start_search(pi);
                assert_eq!(tt_table.get(&p).unwrap().len(), lbt.get_lower_bound() as usize);
                for t in all::<three_triangles_stack::Turns>() {
                    let top_turned = p.0.permute(t.into());
                    let bottom_turned = p.1.permute(t.into());
                    let turned = (top_turned.into(), bottom_turned.into());
                    assert_eq!(tt_table.get(&turned).unwrap().len(), pruning_table.continue_search(lbt, t).get_lower_bound() as usize);
                }
            }
        }

        // Solves the puzzle
        for top_pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            for bottom_pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
                let pi = (top_pi, bottom_pi);
                let mut turns = pruning_table.solve(pi);
                turns.reverse();
                let mut top_p = three_triangles_stack::TopThreeTriangles::identity();
                let mut bottom_p = three_triangles_stack::BottomThreeTriangles::identity();
                for t in turns {
                    top_p = top_p.permute(t.invert().into());
                    bottom_p = bottom_p.permute(t.invert().into());
                }
                assert_eq!(pi, (top_p.into(), bottom_p.into()));
            }
        }
    }

    #[test]
    fn pruning_table_is_correct_for_three_triangles_stack_even_parity_with_rotational_symmetry_via_composite() {
      // TODO: Ideally these use the same move table!
      let top_rep_table = Arc::new(RepresentativeTable::new::<three_triangles_stack::TopThreeTriangles>());
      let top_move_table: MoveTable<three_triangles_stack::RotationalSymmetry, three_triangles::ThreeTrianglesEvenIndex, three_triangles_stack::Turns> = MoveTable::new::<three_triangles_stack::TopThreeTriangles>(top_rep_table.clone());

        let bottom_rep_table = Arc::new(RepresentativeTable::new::<three_triangles_stack::BottomThreeTriangles>());
        let bottom_move_table: MoveTable<three_triangles_stack::RotationalSymmetry, three_triangles::ThreeTrianglesEvenIndex, three_triangles_stack::Turns> = MoveTable::new::<three_triangles_stack::BottomThreeTriangles>(bottom_rep_table.clone());
        let move_table = Arc::new(CompositeMoveTable::new(Arc::new(top_move_table), Arc::new(bottom_move_table)));
        let pruning_table: PruningTable<three_triangles_stack::RotationalSymmetry, (three_triangles::ThreeTrianglesEvenIndex, three_triangles::ThreeTrianglesEvenIndex), _, three_triangles_stack::Turns, _> = PruningTable::new(move_table.clone(), std::iter::once((three_triangles::ThreeTriangles::identity().into(), three_triangles::ThreeTriangles::identity().into())));

        // Our simple implementation (three_triangles_stack is small enough to solve naively) matches our more complex one
        let tt_table = three_triangles_stack::moves_to_solve();
        for top_pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            for bottom_pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
                let pi = (top_pi, bottom_pi);
                let p = (top_pi.into(), bottom_pi.into());
                let lbt = pruning_table.start_search(pi);
                assert_eq!(tt_table.get(&p).unwrap().len(), lbt.get_lower_bound() as usize);
                for t in all::<three_triangles_stack::Turns>() {
                    let top_turned = p.0.permute(t.into());
                    let bottom_turned = p.1.permute(t.into());
                    let turned = (top_turned.into(), bottom_turned.into());
                    assert_eq!(tt_table.get(&turned).unwrap().len(), pruning_table.continue_search(lbt, t).get_lower_bound() as usize);
                }
            }
        }

        // Solves the puzzle
        for top_pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            for bottom_pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
                let pi = (top_pi, bottom_pi);
                let mut turns = pruning_table.solve(pi);
                turns.reverse();
                let mut top_p = three_triangles_stack::TopThreeTriangles::identity();
                let mut bottom_p = three_triangles_stack::BottomThreeTriangles::identity();
                for t in turns {
                    top_p = top_p.permute(t.invert().into());
                    bottom_p = bottom_p.permute(t.invert().into());
                }
                assert_eq!(pi, (top_p.into(), bottom_p.into()));
            }
        }
    }

    #[test]
    fn pruning_table_is_correct_for_three_triangles_stack_even_parity_with_full_symmetry_via_composite() {
        // TODO: Ideally these use the same move table!
        let top_rep_table = Arc::new(RepresentativeTable::new::<three_triangles_stack::TopThreeTriangles>());
        let top_move_table: MoveTable<three_triangles_stack::FullSymmetry, three_triangles::ThreeTrianglesEvenIndex, three_triangles_stack::Turns> = MoveTable::new::<three_triangles_stack::TopThreeTriangles>(top_rep_table.clone());

        let bottom_rep_table = Arc::new(RepresentativeTable::new::<three_triangles_stack::BottomThreeTriangles>());
        let bottom_move_table: MoveTable<three_triangles_stack::FullSymmetry, three_triangles::ThreeTrianglesEvenIndex, three_triangles_stack::Turns> = MoveTable::new::<three_triangles_stack::BottomThreeTriangles>(bottom_rep_table.clone());
        let move_table = Arc::new(CompositeMoveTable::new(Arc::new(top_move_table), Arc::new(bottom_move_table)));
        let pruning_table: PruningTable<three_triangles_stack::FullSymmetry, (three_triangles::ThreeTrianglesEvenIndex, three_triangles::ThreeTrianglesEvenIndex), _, three_triangles_stack::Turns, _> = PruningTable::new(move_table.clone(), std::iter::once((three_triangles::ThreeTriangles::identity().into(), three_triangles::ThreeTriangles::identity().into())));

        // Our simple implementation (three_triangles_stack is small enough to solve naively) matches our more complex one
        let tt_table = three_triangles_stack::moves_to_solve();
        for top_pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            for bottom_pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
                let pi = (top_pi, bottom_pi);
                let p = (top_pi.into(), bottom_pi.into());
                let lbt = pruning_table.start_search(pi);
                assert_eq!(tt_table.get(&p).unwrap().len(), lbt.get_lower_bound() as usize);
                for t in all::<three_triangles_stack::Turns>() {
                    let top_turned = p.0.permute(t.into());
                    let bottom_turned = p.1.permute(t.into());
                    let turned = (top_turned.into(), bottom_turned.into());
                    assert_eq!(tt_table.get(&turned).unwrap().len(), pruning_table.continue_search(lbt, t).get_lower_bound() as usize);
                }
            }
        }

        // Solves the puzzle
        for top_pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            for bottom_pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
                let pi = (top_pi, bottom_pi);
                let mut turns = pruning_table.solve(pi);
                turns.reverse();
                let mut top_p = three_triangles_stack::TopThreeTriangles::identity();
                let mut bottom_p = three_triangles_stack::BottomThreeTriangles::identity();
                for t in turns {
                    top_p = top_p.permute(t.invert().into());
                    bottom_p = bottom_p.permute(t.invert().into());
                }
                assert_eq!(pi, (top_p.into(), bottom_p.into()));
            }
        }
    }
}
