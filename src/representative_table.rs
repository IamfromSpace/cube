use permutation_group::PermutationGroup as PG;
use invertable::Invertable;
use equivalence_class::EquivalenceClass;

use std::collections::BTreeSet;
use std::convert::{TryInto, TryFrom};

// Opaque type to prevent accidental misuse
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct SymIndex(u8);

// Opaque type to prevent accidental misuse
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct RepIndex<PermIndex>(PermIndex);

impl<PermIndex: Into<usize>> Into<usize> for RepIndex<PermIndex> {
    fn into(self) -> usize {
        self.0.into()
    }
}

// PermIndex should be small to make the final array compact, like a u8 or u16 if possible.
#[derive(Debug, Clone)]
pub struct RepresentativeTable<Perm, Sym, PermIndex> {
    table: Vec<PermIndex>,
    syms: Vec<Sym>,
    perm: std::marker::PhantomData<Perm>,
}

impl<Perm: PG + Clone + EquivalenceClass<Sym> + Into<PermIndex>, Sym: Clone, PermIndex: Copy + Ord + TryFrom<usize> + Into<usize> + Into<Perm>> RepresentativeTable<Perm, Sym, PermIndex> where <PermIndex as TryFrom<usize>>::Error: std::fmt::Debug {
    pub fn new<I: Iterator<Item=Perm>>(syms: Vec<Sym>, it: I) -> Self {
        // TODO: With capacity hint?  Could be an Option<usize> argument?
        let mut discovered = BTreeSet::new();

        for perm in it {
            let (ri, _) = smallest_equivalence(&syms, &perm);
            discovered.insert(ri);
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

    // NOTE: This could be optimized further, but this should be surprisingly
    // effective as is.  First off, a PermIndex very likely is large enough to
    // fit a compact sym-coordinate in, but not guaranteed.  A good example
    // would be indexing an orbit of 8 edge orientations, assuming that odd
    // parity was allowed.  This fits exactly into a u8.  But lets say this has
    // four symmetries.  Unfortunately, this means that we'll have _slightly_
    // more than 64 representatives, because self-symmetric positions don't
    // find a unique state per symmetry.  The identity, for example, finds no
    // new positions across symmetries.  So even if all others did, 64 reps
    // could only represent 253 states, and we know there are more--therefore
    // more reps _must_ exist.  The sym-coordinate of the 65th representative
    // reduced by symmetry index 3 is 259, which exceeds the u8 bound.
    // Requiring that PermIndex be big enough for a sym-coordinate could make
    // some sense, but adds some overhead.
    //
    // Next, perfectly compacting a sym-coordinate isn't necessarily a good
    // idea.  Splitting a sym-coordinate will actually be a very common
    // operation, so we can look up the next possible moves on the rep index
    // alone.  It may take more memory to hold them in separate bytes (and
    // therefore less effective in our caches), but maybe be more efficient for
    // a repeated operation.  Using bitwise operations may also be effective to
    // balance compactness vs speed of splitting and combining.
    //
    // Then, the sym index should really be generic so it can be fit to just
    // the right size, but it almost certainly fits in a u8 for practical
    // cubes.  If the sym index is never more than 8 bits, then PermIndex can
    // never save a full 8 bits, and so the likelihood of actually being able
    // to step down the total byte count is diminished.
    //
    // So it's quite possible that this can be optimized to efficiently pack
    // bits here, it's going to be quite fiddly to enable that degree of
    // genericness, and packing cannot every be perfectly efficient without
    // potentially compromising speed by adding multiplication and modulos.
    pub fn perm_to_indexes(&self, perm: &Perm) -> (RepIndex<PermIndex>, SymIndex) {
        let (pi, si) = smallest_equivalence(&self.syms, perm);
        let ri =
            self.table
                .binary_search(&pi)
                // TODO: Make it invariant that all permutations have representatives, by requiring that Perm be enumerable.  If you need an even perm only, then you should need a different type for all perms and even only perms.
                .expect("Permutation does not have a representative in the RepresentativeTable.  This is nearly invariant, but since one passes their own iterator to construct the RepresentativeTable, it's possible to create an incomplete representative table for a Permutation type, but then look up one of the missing members later.  This appears to have happened.");
        (RepIndex(ri.try_into().expect("Invariant violated: the size of the rep table exceeded PermIndexes maximum bound.")), si)
    }

    pub fn rep_index_to_perm(&self, i: RepIndex<PermIndex>) -> Perm {
        let pi: PermIndex = self.table[<PermIndex as Into<usize>>::into(i.0)];
        <PermIndex as Into<Perm>>::into(pi)
    }

    pub fn sym_index_to_sym(&self, i: SymIndex) -> Option<Sym> {
        if i.0 == 255 {
            None
        } else {
            Some(self.syms[i.0 as usize].clone())
        }
    }

    pub fn len(&self) -> usize {
        self.table.len()
    }

    pub fn rep_indexes(&self) -> impl Iterator<Item = RepIndex<PermIndex>> + '_ {
        (0..self.table.len()).map(|ri| RepIndex(ri.try_into().expect("Invariant violated: the size of the rep table exceeded PermIndexes maximum bound.")))
    }
}

fn smallest_equivalence<Perm: PG + Clone + EquivalenceClass<Sym> + Into<PermIndex>, Sym: Clone, PermIndex: Ord>(syms: &Vec<Sym>, perm: &Perm) -> (PermIndex, SymIndex) {
    // Identity Sym should not be included in the Sym list (but hypothetically won't hurt anything?)
    let mut smallest = perm.clone().into(); // Not sure why From<&Perm> is such a pain
    let mut sym_index = 255;
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
            sym_index = i as u8;
        }
    }
    (smallest, SymIndex(sym_index))
}

#[cfg(test)]
mod tests {
    use super::*;
    use two_triangles::*;

    #[test]
    fn representative_table_is_correct_for_two_triangles_without_symmetry() {
        let syms = vec![];
        let all_perms = (0..120u8).map(|i| i.into());
        let rep_table: RepresentativeTable<TwoTriangles, Sym, u8> = RepresentativeTable::new(syms, all_perms.clone());

        // Finds the expected number
        assert_eq!(rep_table.table.len(), 120);

        // Is ordered without duplicates
        for i in 1..rep_table.table.len() {
            assert_eq!(rep_table.table[i - 1] < rep_table.table[i], true);
        }

        // perm_to_indexes round trips
        for p in all_perms {
            let (ri, si) = rep_table.perm_to_indexes(&p);
            let rep = match rep_table.sym_index_to_sym(si) {
                Some(sym) => p.get_equivalent(&sym),
                None => p,
            };
            assert_eq!(rep_table.rep_index_to_perm(ri), rep)
        }
    }

    #[test]
    fn representative_table_is_correct_for_two_triangles_with_rotational_symmetry() {
        let syms = vec![Sym::MirrorBoth];
        let all_perms = (0..120u8).map(|i| i.into());
        let rep_table: RepresentativeTable<TwoTriangles, Sym, u8> = RepresentativeTable::new(syms, all_perms.clone());

        // Finds the expected number
        assert_eq!(rep_table.table.len(), 64);

        // Is ordered without duplicates
        for i in 1..rep_table.table.len() {
            assert_eq!(rep_table.table[i - 1] < rep_table.table[i], true);
        }

        // perm_to_indexes round trips
        for p in all_perms {
            let (ri, si) = rep_table.perm_to_indexes(&p);
            let rep = match rep_table.sym_index_to_sym(si) {
                Some(sym) => p.get_equivalent(&sym),
                None => p,
            };
            assert_eq!(rep_table.rep_index_to_perm(ri), rep)
        }
    }

    #[test]
    fn representative_table_is_correct_for_two_triangles_with_full_symmetry() {
        let syms = vec![Sym::MirrorLR, Sym::MirrorTD, Sym::MirrorBoth];
        let all_perms = (0..120u8).map(|i| i.into());
        let rep_table: RepresentativeTable<TwoTriangles, Sym, u8> = RepresentativeTable::new(syms, all_perms.clone());

        // Finds the expected number
        assert_eq!(rep_table.table.len(), 36);

        // Is ordered without duplicates
        for i in 1..rep_table.table.len() {
            assert_eq!(rep_table.table[i - 1] < rep_table.table[i], true);
        }

        // perm_to_indexes round trips
        for p in all_perms {
            let (ri, si) = rep_table.perm_to_indexes(&p);
            let rep = match rep_table.sym_index_to_sym(si) {
                Some(sym) => p.get_equivalent(&sym),
                None => p,
            };
            assert_eq!(rep_table.rep_index_to_perm(ri), rep)
        }
    }

    #[test]
    fn representative_table_is_correct_for_two_triangles_even_perms_without_symmetry() {
        let syms = vec![];
        let all_perms = (0..120u8).map(|i| i.into()).filter(|t: &TwoTriangles| t.is_even_parity());
        let rep_table: RepresentativeTable<TwoTriangles, Sym, u8> = RepresentativeTable::new(syms, all_perms.clone());

        // Finds the expected number
        assert_eq!(rep_table.table.len(), 60);

        // Is ordered without duplicates
        for i in 1..rep_table.table.len() {
            assert_eq!(rep_table.table[i - 1] < rep_table.table[i], true);
        }

        // perm_to_indexes round trips
        for p in all_perms {
            let (ri, si) = rep_table.perm_to_indexes(&p);
            let rep = match rep_table.sym_index_to_sym(si) {
                Some(sym) => p.get_equivalent(&sym),
                None => p,
            };
            assert_eq!(rep_table.rep_index_to_perm(ri), rep)
        }
    }

    #[test]
    fn representative_table_is_correct_for_two_triangles_even_perms_with_rotational_symmetry() {
        let syms = vec![Sym::MirrorBoth];
        let all_perms = (0..120u8).map(|i| i.into()).filter(|t: &TwoTriangles| t.is_even_parity());
        let rep_table: RepresentativeTable<TwoTriangles, Sym, u8> = RepresentativeTable::new(syms, all_perms.clone());

        // Finds the expected number
        assert_eq!(rep_table.table.len(), 32);

        // Is ordered without duplicates
        for i in 1..rep_table.table.len() {
            assert_eq!(rep_table.table[i - 1] < rep_table.table[i], true);
        }

        // perm_to_indexes round trips
        for p in all_perms {
            let (ri, si) = rep_table.perm_to_indexes(&p);
            let rep = match rep_table.sym_index_to_sym(si) {
                Some(sym) => p.get_equivalent(&sym),
                None => p,
            };
            assert_eq!(rep_table.rep_index_to_perm(ri), rep)
        }
    }

    #[test]
    fn representative_table_is_correct_for_two_triangles_even_perms_with_full_symmetry() {
        let syms = vec![Sym::MirrorLR, Sym::MirrorTD, Sym::MirrorBoth];
        let all_perms = (0..120u8).map(|i| i.into()).filter(|t: &TwoTriangles| t.is_even_parity());
        let rep_table: RepresentativeTable<TwoTriangles, Sym, u8> = RepresentativeTable::new(syms, all_perms.clone());

        // Finds the expected number
        assert_eq!(rep_table.table.len(), 18);

        // Is ordered without duplicates
        for i in 1..rep_table.table.len() {
            assert_eq!(rep_table.table[i - 1] < rep_table.table[i], true);
        }

        // perm_to_indexes round trips
        for p in all_perms {
            let (ri, si) = rep_table.perm_to_indexes(&p);
            let rep = match rep_table.sym_index_to_sym(si) {
                Some(sym) => p.get_equivalent(&sym),
                None => p,
            };
            assert_eq!(rep_table.rep_index_to_perm(ri), rep)
        }
    }
}
