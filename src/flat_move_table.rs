use permutation_group::PermutationGroup as PG;
use invertable::Invertable;
use equivalence_class::EquivalenceClass;

use std::collections::HashMap;
use std::hash::Hash;

#[derive(Debug, Clone)]
pub struct MoveTable<Perm, Sym, Turn> {
    representants: Vec<Perm>,
    table: Vec<(usize, Option<usize>)>,
    syms: Vec<Sym>,
    turns: Vec<Turn>,
}

impl<Perm: Hash + Ord + PG + Clone + EquivalenceClass<Sym>, Turn: Copy + Into<Perm>, Sym: Clone> MoveTable<Perm, Sym, Turn> {
    // TODO: Should we just accept an iteratable type for both Turn and Sym?
    pub fn new(turns: Vec<Turn>, syms: Vec<Sym>, initial: &Perm) -> Self {
        let mut representants_hm = HashMap::new();
        let mut queue = Vec::new();
        let mut representants_table = Vec::new();
        // TODO: With capacity hint?
        let mut table = Vec::new();

        // TODO: Can't we just generate the identity?  Or do we need to accept a list of goal states?
        representants_hm.insert(initial.clone(), 0);
        queue.push((initial.clone(), 0));
        representants_table.push(initial.clone());

        loop {
            let next = queue.pop();

            match next {
                None => {
                  break;
                }
                Some((c, rj)) => {
                    for ti in 0..turns.len() {
                        let t = turns[ti];
                        let turned: Perm = c.clone().permute(<Turn as Into<Perm>>::into(t));
                        let (r, si) = greatest_equivalence(&syms, &turned);
                        let ri = match representants_hm.get(&r) {
                            None => {
                                let current_count = representants_hm.len();
                                representants_hm.insert(r.clone(), current_count);
                                queue.push((r.clone(), current_count));
                                representants_table.push(r);
                                current_count
                            },
                            Some(ri) => *ri,
                        };

                        let i: usize = rj * turns.len() + ti;
                        if i >= table.len() {
                            table.resize(i + 1, (0, None));
                        }
                        table[i] = (ri, si)
                    }
                }
            }
        }

        MoveTable {
            representants: representants_table,
            table,
            syms,
            turns,
        }
    }
}

fn greatest_equivalence<Perm: Ord + PG + Clone + EquivalenceClass<Sym>, Sym: Clone>(syms: &Vec<Sym>, perm: &Perm) -> (Perm, Option<usize>) {
    // Identity Sym should not be included in the Sym list (but hypothetically won't hurt anything?)
    let mut sym = None;
    let mut greatest = perm.clone();
    for i in 0..syms.len() {
        // TODO: This clone is pretty wasteful, because we really only need a
        // reference, but get_equivalent takes self not &self, because so do
        // permute and apply.  We can't implement apply on &T, because then
        // we'd have &T -> &T -> &T, and it's not possible to return a
        // reference to nothing.  We also can't do something like BorrowMut<T>
        // -> BorrowMut<T> -> BorrowMut<T>, because we can't return the
        // original mutable reference, because that would make for two of them.
        // It seems to me that the functional crate maybe gets it wrong, for
        // Rust, it should still be &T -> &T -> T, because in functional
        // programming we never mutate anything!  We should only ever need a
        // reference to the first two parameters, but then create something new
        // at the end.
        let e = perm.clone().get_equivalent(&syms[i]);
        if e > greatest {
            greatest = e;
            sym = Some(i);
        }
    }
    ( greatest, sym )
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;
    use two_triangles::*;

    #[test]
    fn move_table_is_correct_for_two_triangles_without_symmetry() {
        let turns = vec![Turns::Left, Turns::Right];
        let syms = vec![];

        let move_table: MoveTable<TwoTriangles, Sym, Turns> = MoveTable::new(turns, syms, &TwoTriangles::identity());
        let unique_representants = move_table.representants.iter().collect::<HashSet<_>>();

        // Half of the maximum possible 120 permutations, due to parity
        assert_eq!(move_table.representants.len(), 60);
        assert_eq!(move_table.representants.len(), unique_representants.len());
        // Two moves per representant
        assert_eq!(move_table.table.len(), 120);
    }

    #[test]
    fn move_table_is_correct_for_two_triangles_with_rotational_symmetry() {
        let turns = vec![Turns::Left, Turns::Right];
        let syms = vec![Sym::MirrorBoth];

        let move_table: MoveTable<TwoTriangles, Sym, Turns> = MoveTable::new(turns, syms.clone(), &TwoTriangles::identity());
        let unique_representants = move_table.representants.iter().collect::<HashSet<_>>();

        // 32 equivalence classes, based on manual inspection
        assert_eq!(move_table.representants.len(), 32);
        assert_eq!(move_table.representants.len(), unique_representants.len());
        // Two moves per representant
        assert_eq!(move_table.table.len(), 64);

        for representant in &move_table.representants {
            for sym in &syms {
                let equivalent = representant.get_equivalent(sym);
                assert_eq!(representant == &equivalent || !unique_representants.contains(&equivalent), true);
            }
        }
    }

    #[test]
    fn move_table_is_correct_for_two_triangles_with_full_symmetry() {
        let turns = vec![Turns::Left, Turns::LeftPrime, Turns::Right, Turns::RightPrime];
        let syms = vec![Sym::MirrorLR, Sym::MirrorTD, Sym::MirrorBoth];

        let move_table: MoveTable<TwoTriangles, Sym, Turns> = MoveTable::new(turns, syms.clone(), &TwoTriangles::identity());
        let unique_representants = move_table.representants.iter().collect::<HashSet<_>>();

        // 18 equivalence classes, based on manual inspection
        assert_eq!(move_table.representants.len(), 18);
        assert_eq!(move_table.representants.len(), unique_representants.len());
        // Two moves per representant
        assert_eq!(move_table.table.len(), 72);

        for representant in &move_table.representants {
            for sym in &syms {
                let equivalent = representant.get_equivalent(sym);
                assert_eq!(representant == &equivalent || !unique_representants.contains(&equivalent), true);
            }
        }
    }
}
