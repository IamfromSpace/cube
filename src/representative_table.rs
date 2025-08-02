use permutation_group::PermutationGroup as PG;
use invertable::Invertable;
use equivalence_class::EquivalenceClass;

use std::collections::BTreeSet;

// Index should be small to make the final array compact, like a u8 or u16 if possible.
#[derive(Debug, Clone)]
pub struct RepresentativeTable<Perm, Sym, Index> {
    table: Vec<Index>,
    syms: Vec<Sym>,
    perm: std::marker::PhantomData<Perm>,
}

impl<Perm: PG + Clone + EquivalenceClass<Sym> + Into<Index>, Sym: Clone, Index: Ord> RepresentativeTable<Perm, Sym, Index> {
    pub fn new<I: Iterator<Item=Perm>>(syms: Vec<Sym>, it: I) -> Self {
        // TODO: With capacity hint?  Could be an Option<usize> argument?
        let mut discovered = BTreeSet::new();

        for perm in it {
            discovered.insert(smallest_equivalence(&syms, &perm));
        }

        // Not sure why BTreeSet<T> isn't Into<Vec<T>>
        let mut table = Vec::with_capacity(discovered.len());
        for x in discovered {
           table.push(x);
        }

        RepresentativeTable {
            table,
            syms,
            perm: std::marker::PhantomData,
        }
    }
}

fn smallest_equivalence<Perm: PG + Clone + EquivalenceClass<Sym> + Into<Index>, Sym, Index: Ord>(syms: &Vec<Sym>, perm: &Perm) -> Index {
    // Identity Sym should not be included in the Sym list (but hypothetically won't hurt anything?)
    let mut smallest = perm.clone().into(); // Not sure why From<&Perm> is such a pain
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
        let ei = perm.clone().get_equivalent(&syms[i]).into();
        if ei < smallest {
            smallest = ei;
        }
    }
    smallest
}

#[cfg(test)]
mod tests {
    use super::*;
    use two_triangles::*;

    #[test]
    fn representative_table_is_correct_for_two_triangles_without_symmetry() {
        let syms = vec![];
        let all_perms = (0..120u8).map(|i| i.into());
        let rep_table: RepresentativeTable<TwoTriangles, Sym, u8> = RepresentativeTable::new(syms, all_perms);
        assert_eq!(rep_table.table.len(), 120);
        for i in 1..rep_table.table.len() {
            assert_eq!(rep_table.table[i - 1] < rep_table.table[i], true);
        }
    }

    #[test]
    fn representative_table_is_correct_for_two_triangles_with_rotational_symmetry() {
        let syms = vec![Sym::MirrorBoth];
        let all_perms = (0..120u8).map(|i| i.into());
        let rep_table: RepresentativeTable<TwoTriangles, Sym, u8> = RepresentativeTable::new(syms, all_perms);
        assert_eq!(rep_table.table.len(), 64);
        for i in 1..rep_table.table.len() {
            assert_eq!(rep_table.table[i - 1] < rep_table.table[i], true);
        }
    }

    #[test]
    fn representative_table_is_correct_for_two_triangles_with_full_symmetry() {
        let syms = vec![Sym::MirrorLR, Sym::MirrorTD, Sym::MirrorBoth];
        let all_perms = (0..120u8).map(|i| i.into());
        let rep_table: RepresentativeTable<TwoTriangles, Sym, u8> = RepresentativeTable::new(syms, all_perms);
        assert_eq!(rep_table.table.len(), 36);
        for i in 1..rep_table.table.len() {
            assert_eq!(rep_table.table[i - 1] < rep_table.table[i], true);
        }
    }

    #[test]
    fn representative_table_is_correct_for_two_triangles_even_perms_without_symmetry() {
        let syms = vec![];
        let all_perms = (0..120u8).map(|i| i.into()).filter(|t: &TwoTriangles| t.is_even_parity());
        let rep_table: RepresentativeTable<TwoTriangles, Sym, u8> = RepresentativeTable::new(syms, all_perms);
        assert_eq!(rep_table.table.len(), 60);
        for i in 1..rep_table.table.len() {
            assert_eq!(rep_table.table[i - 1] < rep_table.table[i], true);
        }
    }

    #[test]
    fn representative_table_is_correct_for_two_triangles_even_perms_with_rotational_symmetry() {
        let syms = vec![Sym::MirrorBoth];
        let all_perms = (0..120u8).map(|i| i.into()).filter(|t: &TwoTriangles| t.is_even_parity());
        let rep_table: RepresentativeTable<TwoTriangles, Sym, u8> = RepresentativeTable::new(syms, all_perms);
        assert_eq!(rep_table.table.len(), 32);
        for i in 1..rep_table.table.len() {
            assert_eq!(rep_table.table[i - 1] < rep_table.table[i], true);
        }
    }

    #[test]
    fn representative_table_is_correct_for_two_triangles_even_perms_with_full_symmetry() {
        let syms = vec![Sym::MirrorLR, Sym::MirrorTD, Sym::MirrorBoth];
        let all_perms = (0..120u8).map(|i| i.into()).filter(|t: &TwoTriangles| t.is_even_parity());
        let rep_table: RepresentativeTable<TwoTriangles, Sym, u8> = RepresentativeTable::new(syms, all_perms);
        assert_eq!(rep_table.table.len(), 18);
        for i in 1..rep_table.table.len() {
            assert_eq!(rep_table.table[i - 1] < rep_table.table[i], true);
        }
    }
}
