use permutation_group::PermutationGroup as PG;
use equivalence_class::EquivalenceClass;
use super::util::{n_scoped_workers, while_iter_in_mutex_has_next };
use flat_move_table::MoveTable;
use representative_table::RepIndex;

use std::sync::Arc;
use std::convert::TryFrom;
use std::collections::BTreeSet;
use std::collections::VecDeque;
use enum_iterator::Sequence;

#[derive(Debug)]
pub struct PruningTable<Perm, Sym, PermIndex, Turn> {
    table: Vec<u8>,
    // TODO: We almost certainly want/need to just use some abstract T here,
    // where it will implement some sort of Turner trait.  The reason is, that
    // Pruning Tables work rather broadly, and MoveTables are totally optional,
    // symmetry management is totally optional, and in practice we're going to
    // have tables that require composite MoveTables.  Rather than have a type
    // that captures all of this, we're going to be better off having a trait
    // for what a pruning table needs specifically from a stateful (or
    // stateless) Turner.
    move_table: Arc<MoveTable<Perm, Sym, PermIndex, Turn>>,
    goals: BTreeSet<RepIndex<PermIndex>>,
}

impl<Perm: PG + Clone + EquivalenceClass<Sym> + Into<PermIndex>, Turn: Copy + Into<Perm> + PartialEq, Sym: Sequence + Copy + Clone, PermIndex: Sequence + Copy + Ord + TryFrom<usize> + Into<usize> + Into<Perm> + std::fmt::Debug> PruningTable<Perm, Sym, PermIndex, Turn> where <PermIndex as TryFrom<usize>>::Error: std::fmt::Debug {
    // NOTE: For practical reasons this only supports puzzles we can apply the
    // inverse of any turn as a turn.  But hypothetically, we could have
    // different turns between our PruningTable and our MoveTable.  However,
    // the MoveTable would still need to hold inverses so that we could explore
    // in either direction, but the pruning table would only be generated with
    // the inverted subset (how to solve it), or with the forward set (how to
    // get to the scrambled position).
    pub fn new<I: Iterator<Item=Perm>>(move_table: Arc<MoveTable<Perm, Sym, PermIndex, Turn>>, goal_states: I) -> Self {
        let table_size = move_table.len() / 4 + if move_table.len() % 4 == 0 { 0 } else { 1 };
        let mut table = Vec::with_capacity(table_size);
        // 0b00 means (turns left `mod` 3 = 0)
        // 0b01 means (turns left `mod` 3 = 1)
        // 0b10 means (turns left `mod` 3 = 2)
        // 0b11 means uninitialized
        table.resize(table_size, 255);
        let mut queue = VecDeque::new();
        let mut goals = BTreeSet::new();
        for p in goal_states {
            let (ri, _) = move_table.perm_to_indexes(&p);
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
                    for (ri, _) in move_table.apply_turns(ri) {
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
        }
    }

    // For perfect pruning tables, the lower_bound _is_ the remaining turn count.
    pub fn remaining_turns_lower_bound(&self, p: &Perm) -> u8 {
        let mut count = 0;
        let (mut ri, _) = self.move_table.perm_to_indexes(p);

        loop {
            if self.goals.contains(&ri) {
                break count;
            } else {
                let target = (lookup(&self.table, ri.into()) + 2) % 3;
                let mut found = false;
                for (candidate, _) in self.move_table.apply_turns(ri) {
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

    #[test]
    fn pruning_table_is_correct_for_two_triangles_even_parity_without_symmetry() {
        let turns = vec![Turns::Left, Turns::LeftPrime, Turns::Right, Turns::RightPrime];
        let all_perms = all::<TwoTrianglesIndex>().map(|i| i.into()).filter(|t: &TwoTriangles| t.is_even_parity());

        let rep_table: RepresentativeTable<TwoTriangles, NoSymmetry, TwoTrianglesEvenIndex> = RepresentativeTable::new();
        let rep_table = Arc::new(rep_table);
        let move_table: MoveTable<TwoTriangles, NoSymmetry, TwoTrianglesEvenIndex, Turns> = MoveTable::new(turns.clone(), rep_table.clone());
        let move_table = Arc::new(move_table);
        let pruning_table: PruningTable<TwoTriangles, NoSymmetry, TwoTrianglesEvenIndex, Turns> = PruningTable::new(move_table.clone(), std::iter::once(TwoTriangles::identity()));

        // Our simple implementation (TwoTriangles is small enough to solve naively) matches our more complex one
        let tt_table = moves_to_solve(&turns);
        for p in all_perms {
            assert_eq!(*tt_table.get(&p).unwrap(), pruning_table.remaining_turns_lower_bound(&p) as usize);
        }
    }

    #[test]
    fn pruning_table_is_correct_for_two_triangles_even_parity_with_rotational_symmetry() {
        let turns = vec![Turns::Left, Turns::LeftPrime, Turns::Right, Turns::RightPrime];
        let all_perms = all::<TwoTrianglesIndex>().map(|i| i.into()).filter(|t: &TwoTriangles| t.is_even_parity());

        let rep_table: RepresentativeTable<TwoTriangles, RotationalSymmetry, TwoTrianglesEvenIndex> = RepresentativeTable::new();
        let rep_table = Arc::new(rep_table);
        let move_table: MoveTable<TwoTriangles, RotationalSymmetry, TwoTrianglesEvenIndex, Turns> = MoveTable::new(turns.clone(), rep_table.clone());
        let move_table = Arc::new(move_table);
        let pruning_table: PruningTable<TwoTriangles, RotationalSymmetry, TwoTrianglesEvenIndex, Turns> = PruningTable::new(move_table.clone(), std::iter::once(TwoTriangles::identity()));

        // Our simple implementation (TwoTriangles is small enough to solve naively) matches our more complex one
        let tt_table = moves_to_solve(&turns);
        for p in all_perms {
            assert_eq!(*tt_table.get(&p).unwrap(), pruning_table.remaining_turns_lower_bound(&p) as usize);
        }
    }

    #[test]
    fn pruning_table_is_correct_for_two_triangles_even_parity_with_full_symmetry() {
        let turns = vec![Turns::Left, Turns::LeftPrime, Turns::Right, Turns::RightPrime];
        let all_perms = all::<TwoTrianglesIndex>().map(|i| i.into()).filter(|t: &TwoTriangles| t.is_even_parity());

        let rep_table: RepresentativeTable<TwoTriangles, FullSymmetry, TwoTrianglesEvenIndex> = RepresentativeTable::new();
        let rep_table = Arc::new(rep_table);
        let move_table: MoveTable<TwoTriangles, FullSymmetry, TwoTrianglesEvenIndex, Turns> = MoveTable::new(turns.clone(), rep_table.clone());
        let move_table = Arc::new(move_table);
        let pruning_table: PruningTable<TwoTriangles, FullSymmetry, TwoTrianglesEvenIndex, Turns> = PruningTable::new(move_table.clone(), std::iter::once(TwoTriangles::identity()));

        // Our simple implementation (TwoTriangles is small enough to solve naively) matches our more complex one
        let tt_table = moves_to_solve(&turns);
        for p in all_perms {
            assert_eq!(*tt_table.get(&p).unwrap(), pruning_table.remaining_turns_lower_bound(&p) as usize);
        }
    }
}
