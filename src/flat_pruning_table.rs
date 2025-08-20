use permutation_group::PermutationGroup as PG;
use invertable::Invertable;
use equivalence_class::EquivalenceClass;
use representative_table::RepIndex;
use table_traits::{ TableTurn, TableRawIndexToSymIndex, TableSymIndexToRawIndex, TableRepCount };

use std::sync::Arc;
use std::convert::TryFrom;
use std::collections::BTreeSet;
use std::collections::VecDeque;
use enum_iterator::{all, Sequence};

#[derive(Debug)]
pub struct PruningTable<Sym, PermIndex, Turn, MoveTable> {
    table: Vec<u8>,
    // TODO: We almost certainly want/need to just use some abstract T here,
    // where it will implement some sort of Turner trait.  The reason is, that
    // Pruning Tables work rather broadly, and MoveTables are totally optional,
    // symmetry management is totally optional, and in practice we're going to
    // have tables that require composite MoveTables.  Rather than have a type
    // that captures all of this, we're going to be better off having a trait
    // for what a pruning table needs specifically from a stateful (or
    // stateless) Turner.
    move_table: MoveTable,
    goals: BTreeSet<RepIndex<Sym, PermIndex>>,
    turns: std::marker::PhantomData<Turn>,
}

impl<MoveTable: TableTurn<Sym, RepIndex<Sym, PermIndex>, Turn> + TableRawIndexToSymIndex<Sym, PermIndex, RepIndex<Sym, PermIndex>> + TableSymIndexToRawIndex<Sym, PermIndex, RepIndex<Sym, PermIndex>> + TableRepCount, Turn: Sequence + Copy + PartialEq + Into<usize> + Invertable + EquivalenceClass<Sym>, Sym: Sequence + Copy + Clone + Into<usize> + Invertable + PG + Ord, PermIndex: Sequence + Copy + Ord + TryFrom<usize> + Into<usize> + std::fmt::Debug> PruningTable<Sym, PermIndex, Turn, MoveTable> where <PermIndex as TryFrom<usize>>::Error: std::fmt::Debug {
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
        }
    }

    // For perfect pruning tables, the lower_bound _is_ the remaining turn count.
    pub fn remaining_turns_lower_bound(&self, pi: PermIndex) -> u8 {
        let mut count = 0;
        let (mut ri, _) = self.move_table.table_raw_index_to_sym_index(pi);

        loop {
            if self.goals.contains(&ri) {
                break count;
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
        let (mut ri, mut s) = self.move_table.table_raw_index_to_sym_index(pi);

        loop {
            if self.goals.contains(&ri) {
                break turns;
            }

            let target = (lookup(&self.table, ri.into()) + 2) % 3;

            for turn in all::<Turn>() {
                let (ri2, s2) = self.move_table.table_turn(ri, turn.get_equivalent(&s));
                if target == lookup(&self.table, ri2.into()) {
                    turns.push(turn);
                    ri = ri2;
                    // TODO: We should probably wait to correct our turns until
                    // the end, so we only permute the symmetries that actually
                    // solve the cube, but this is just so much simpler.
                    s = s.permute(s2);
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

    #[test]
    fn pruning_table_is_correct_for_two_triangles_even_parity_without_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new::<TwoTriangles>());
        let move_table = Arc::new(MoveTable::new::<TwoTriangles>(rep_table));
        let pruning_table: PruningTable<NoSymmetry, TwoTrianglesEvenIndex, Turns, _> = PruningTable::new(move_table, std::iter::once(TwoTriangles::identity().into()));

        // Our simple implementation (TwoTriangles is small enough to solve naively) matches our more complex one
        let tt_table = moves_to_solve();
        for pi in all::<TwoTrianglesEvenIndex>() {
            assert_eq!(*tt_table.get(&pi).unwrap(), pruning_table.remaining_turns_lower_bound(pi) as usize);
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
        let pruning_table: PruningTable<RotationalSymmetry, TwoTrianglesEvenIndex, Turns, _> = PruningTable::new(move_table.clone(), std::iter::once(TwoTriangles::identity().into()));

        // Our simple implementation (TwoTriangles is small enough to solve naively) matches our more complex one
        let tt_table = moves_to_solve();
        for pi in all::<TwoTrianglesEvenIndex>() {
            assert_eq!(*tt_table.get(&pi).unwrap(), pruning_table.remaining_turns_lower_bound(pi) as usize);
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
        let pruning_table: PruningTable<FullSymmetry, TwoTrianglesEvenIndex, Turns, _> = PruningTable::new(move_table.clone(), std::iter::once(TwoTriangles::identity().into()));

        // Our simple implementation (TwoTriangles is small enough to solve naively) matches our more complex one
        let tt_table = moves_to_solve();
        for pi in all::<TwoTrianglesEvenIndex>() {
            assert_eq!(*tt_table.get(&pi).unwrap(), pruning_table.remaining_turns_lower_bound(pi) as usize);
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
        let pruning_table: PruningTable<three_triangles::NoSymmetry, three_triangles::ThreeTrianglesEvenIndex, three_triangles::Turns, _> = PruningTable::new(move_table, std::iter::once(three_triangles::ThreeTriangles::identity().into()));

        // Our simple implementation (three_triangles::ThreeTriangles is small enough to solve naively) matches our more complex one
        let tt_table = three_triangles::moves_to_solve();
        for pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            assert_eq!(*tt_table.get(&pi).unwrap(), pruning_table.remaining_turns_lower_bound(pi) as usize);
        }
    }

    #[test]
    fn pruning_table_is_correct_for_three_triangles_even_parity_with_rotational_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new::<three_triangles::ThreeTriangles>());
        let move_table = Arc::new(MoveTable::new::<three_triangles::ThreeTriangles>(rep_table));
        let pruning_table: PruningTable<three_triangles::RotationalSymmetry, three_triangles::ThreeTrianglesEvenIndex, three_triangles::Turns, _> = PruningTable::new(move_table.clone(), std::iter::once(three_triangles::ThreeTriangles::identity().into()));

        // Our simple implementation (three_triangles::ThreeTriangles is small enough to solve naively) matches our more complex one
        let tt_table = three_triangles::moves_to_solve();
        for pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            assert_eq!(*tt_table.get(&pi).unwrap(), pruning_table.remaining_turns_lower_bound(pi) as usize);
        }
    }

    #[test]
    fn pruning_table_is_correct_for_three_triangles_even_parity_with_full_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new::<three_triangles::ThreeTriangles>());
        let move_table = Arc::new(MoveTable::new::<three_triangles::ThreeTriangles>(rep_table));
        let pruning_table: PruningTable<three_triangles::FullSymmetry, three_triangles::ThreeTrianglesEvenIndex, three_triangles::Turns, _> = PruningTable::new(move_table.clone(), std::iter::once(three_triangles::ThreeTriangles::identity().into()));

        // Our simple implementation (three_triangles::ThreeTriangles is small enough to solve naively) matches our more complex one
        let tt_table = three_triangles::moves_to_solve();
        for pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            assert_eq!(*tt_table.get(&pi).unwrap(), pruning_table.remaining_turns_lower_bound(pi) as usize);
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
}
