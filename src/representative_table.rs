use permutation_group::PermutationGroup as PG;
use invertable::Invertable;
use equivalence_class::EquivalenceClass;

use std::collections::BTreeSet;
use std::convert::{TryInto, TryFrom};
use enum_iterator::{Sequence, all};

// Opaque type to prevent accidental misuse
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct RepIndex<PermIndex>(PermIndex);

impl<PermIndex: Into<usize>> Into<usize> for RepIndex<PermIndex> {
    fn into(self) -> usize {
        self.0.into()
    }
}

// PermIndex should be small to make the final array compact, like a u8 or u16 if possible.
#[derive(Debug)]
pub struct RepresentativeTable<Perm, Sym, PermIndex> {
    table: Vec<PermIndex>,
    syms: std::marker::PhantomData<Sym>,
    perm: std::marker::PhantomData<Perm>,
}

impl<Perm: PG + Clone + EquivalenceClass<Sym> + Into<PermIndex>, Sym: Sequence + Clone, PermIndex: Sequence + Copy + Ord + TryFrom<usize> + Into<usize> + Into<Perm>> RepresentativeTable<Perm, Sym, PermIndex> where <PermIndex as TryFrom<usize>>::Error: std::fmt::Debug {
    pub fn new() -> Self {
        let mut discovered = BTreeSet::new();

        for pi in all::<PermIndex>() {
            let perm: Perm = pi.into();
            let (ri, _): (PermIndex, Sym) = smallest_equivalence(&perm);
            discovered.insert(ri);
        }

        // Not sure why BTreeSet<T> isn't Into<Vec<T>>
        let mut table = Vec::with_capacity(discovered.len());
        for x in discovered {
           table.push(x);
        }

        RepresentativeTable {
            table,
            syms: std::marker::PhantomData,
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
    pub fn perm_to_indexes(&self, perm: &Perm) -> (RepIndex<PermIndex>, Sym) {
        let (pi, si): (PermIndex, Sym) = smallest_equivalence(perm);
        let ri =
            self.table
                .binary_search(&pi)
                .expect("Invariant violation: Permutation does not have a representative in the RepresentativeTable.");
        (RepIndex(ri.try_into().expect("Invariant violated: the size of the rep table exceeded PermIndexes maximum bound.")), si)
    }

    pub fn rep_index_to_perm(&self, i: RepIndex<PermIndex>) -> Perm {
        let pi: PermIndex = self.table[<PermIndex as Into<usize>>::into(i.0)];
        <PermIndex as Into<Perm>>::into(pi)
    }

    // TODO: Should MoveTable implement Sequence?
    pub fn len(&self) -> usize {
        self.table.len()
    }

    // TODO: Should MoveTable implement Sequence?
    pub fn rep_indexes(&self) -> impl Iterator<Item = RepIndex<PermIndex>> + '_ {
        (0..self.table.len()).map(|ri| RepIndex(ri.try_into().expect("Invariant violated: the size of the rep table exceeded PermIndexes maximum bound.")))
    }
}

fn smallest_equivalence<Perm: PG + Clone + EquivalenceClass<Sym> + Into<PermIndex>, Sym: Sequence, PermIndex: Ord>(perm: &Perm) -> (PermIndex, Sym) {
    // Identity must be included as a symmetry
    let mut smallest = perm.clone().into(); // Not sure why From<&Perm> is such a pain
    let mut sym = Sym::first().expect("Error: The Sym type has no members, but it _must_ have at least the Identity.");

    // TODO: don't replay first
    for s in all::<Sym>() {
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
        let ei = perm.clone().get_equivalent(&s).into();
        if ei < smallest {
            smallest = ei;
            sym = s;
        }
    }
    (smallest, sym)
}

#[cfg(test)]
mod tests {
    use super::*;
    use two_triangles::*;

    #[test]
    fn representative_table_is_correct_for_two_triangles_without_symmetry() {
        let rep_table: RepresentativeTable<TwoTriangles, NoSymmetry, TwoTrianglesIndex> = RepresentativeTable::new();

        // Finds the expected number
        assert_eq!(rep_table.table.len(), 120);

        // Is ordered without duplicates
        for i in 1..rep_table.table.len() {
            assert_eq!(rep_table.table[i - 1] < rep_table.table[i], true);
        }

        // perm_to_indexes round trips
        for pi in all::<TwoTrianglesIndex>() {
            let p = pi.into();
            let (ri, sym) = rep_table.perm_to_indexes(&p);
            let rep = p.get_equivalent(&sym);
            assert_eq!(rep_table.rep_index_to_perm(ri), rep)
        }
    }

    #[test]
    fn representative_table_is_correct_for_two_triangles_with_rotational_symmetry() {
        let rep_table: RepresentativeTable<TwoTriangles, RotationalSymmetry, TwoTrianglesIndex> = RepresentativeTable::new();

        // Finds the expected number
        assert_eq!(rep_table.table.len(), 64);

        // Is ordered without duplicates
        for i in 1..rep_table.table.len() {
            assert_eq!(rep_table.table[i - 1] < rep_table.table[i], true);
        }

        // perm_to_indexes round trips
        for pi in all::<TwoTrianglesIndex>() {
            let p = pi.into();
            let (ri, sym) = rep_table.perm_to_indexes(&p);
            let rep = p.get_equivalent(&sym);
            assert_eq!(rep_table.rep_index_to_perm(ri), rep)
        }
    }

    #[test]
    fn representative_table_is_correct_for_two_triangles_with_full_symmetry() {
        let rep_table: RepresentativeTable<TwoTriangles, FullSymmetry, TwoTrianglesIndex> = RepresentativeTable::new();

        // Finds the expected number
        assert_eq!(rep_table.table.len(), 36);

        // Is ordered without duplicates
        for i in 1..rep_table.table.len() {
            assert_eq!(rep_table.table[i - 1] < rep_table.table[i], true);
        }

        // perm_to_indexes round trips
        for pi in all::<TwoTrianglesIndex>() {
            let p = pi.into();
            let (ri, sym) = rep_table.perm_to_indexes(&p);
            let rep = p.get_equivalent(&sym);
            assert_eq!(rep_table.rep_index_to_perm(ri), rep)
        }
    }

    #[test]
    fn representative_table_is_correct_for_two_triangles_even_perms_without_symmetry() {
        let rep_table: RepresentativeTable<TwoTriangles, NoSymmetry, TwoTrianglesEvenIndex> = RepresentativeTable::new();

        // Finds the expected number
        assert_eq!(rep_table.table.len(), 60);

        // Is ordered without duplicates
        for i in 1..rep_table.table.len() {
            assert_eq!(rep_table.table[i - 1] < rep_table.table[i], true);
        }

        // perm_to_indexes round trips
        for pi in all::<TwoTrianglesEvenIndex>() {
            let p = pi.into();
            let (ri, sym) = rep_table.perm_to_indexes(&p);
            let rep = p.get_equivalent(&sym);
            assert_eq!(rep_table.rep_index_to_perm(ri), rep)
        }
    }

    #[test]
    fn representative_table_is_correct_for_two_triangles_even_perms_with_rotational_symmetry() {
        let rep_table: RepresentativeTable<TwoTriangles, RotationalSymmetry, TwoTrianglesEvenIndex> = RepresentativeTable::new();

        // Finds the expected number
        assert_eq!(rep_table.table.len(), 32);

        // Is ordered without duplicates
        for i in 1..rep_table.table.len() {
            assert_eq!(rep_table.table[i - 1] < rep_table.table[i], true);
        }

        // perm_to_indexes round trips
        for pi in all::<TwoTrianglesEvenIndex>() {
            let p = pi.into();
            let (ri, sym) = rep_table.perm_to_indexes(&p);
            let rep = p.get_equivalent(&sym);
            assert_eq!(rep_table.rep_index_to_perm(ri), rep)
        }
    }

    #[test]
    fn representative_table_is_correct_for_two_triangles_even_perms_with_full_symmetry() {
        let rep_table: RepresentativeTable<TwoTriangles, FullSymmetry, TwoTrianglesEvenIndex> = RepresentativeTable::new();

        // Finds the expected number
        assert_eq!(rep_table.table.len(), 18);

        // Is ordered without duplicates
        for i in 1..rep_table.table.len() {
            assert_eq!(rep_table.table[i - 1] < rep_table.table[i], true);
        }

        // perm_to_indexes round trips
        for pi in all::<TwoTrianglesEvenIndex>() {
            let p = pi.into();
            let (ri, sym) = rep_table.perm_to_indexes(&p);
            let rep = p.get_equivalent(&sym);
            assert_eq!(rep_table.rep_index_to_perm(ri), rep)
        }
    }
}
