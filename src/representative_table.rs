use equivalence_class::EquivalenceClass;

use std::collections::BTreeMap;
use std::convert::{TryInto, TryFrom};
use enum_iterator::{Sequence, all, cardinality};

// Opaque type to prevent accidental misuse
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct RepIndex<Sym, PermIndex>(PermIndex, std::marker::PhantomData<Sym>);

impl<Sym, PermIndex: Into<usize>> Into<usize> for RepIndex<Sym, PermIndex> {
    fn into(self) -> usize {
        self.0.into()
    }
}

// PermIndex should be small to make the final array compact, like a u8 or u16 if possible.
#[derive(Debug)]
pub struct RepresentativeTable<Sym, PermIndex> {
    table: Vec<PermIndex>,
    self_symmetric_table: Vec<u8>,
    to_sym_index_table: Vec<(RepIndex<Sym, PermIndex>, Sym)>,
    syms: std::marker::PhantomData<Sym>,
}

impl<Sym: Sequence + Clone, PermIndex: Sequence + Copy + Ord + TryFrom<usize> + Into<usize>> RepresentativeTable<Sym, PermIndex> where <PermIndex as TryFrom<usize>>::Error: std::fmt::Debug {
    pub fn new<Perm: Clone + EquivalenceClass<Sym> + Into<PermIndex>>() -> Self where PermIndex: Into<Perm> {
        let mut discovered = BTreeMap::new();
        let mut self_symmetric_table = Vec::new();
        let mut table = Vec::new();
        let mut to_sym_index_table = Vec::with_capacity(cardinality::<PermIndex>());

        for pi in all::<PermIndex>() {
            let perm: Perm = pi.into();
            let (pi2, sym, is_self_symmetric): (PermIndex, Sym, bool) = smallest_equivalence(&perm);

            let mut ri = discovered.len();
            match discovered.get(&pi2) {
                None => {
                    // Extend on self_symmetry table when necessary
                    let byte_index = discovered.len() % 8;
                    if byte_index == 0 {
                        self_symmetric_table.push(0);
                    }
                    // If the position is self symmetric, set its bit flag
                    if is_self_symmetric {
                        let i = self_symmetric_table.len() - 1;
                        let new = self_symmetric_table[i] | (1 << byte_index);
                        self_symmetric_table[i] = new;
                    }

                    discovered.insert(pi2, ri);
                    table.push(pi2);
                },
                Some(i) => {
                    ri = *i;
                }
            }
            to_sym_index_table.push((RepIndex(ri.try_into().expect("Invariant violated: the size of the rep table exceeded PermIndexes maximum bound."), std::marker::PhantomData), sym));
        }

        RepresentativeTable {
            table,
            self_symmetric_table,
            to_sym_index_table,
            syms: std::marker::PhantomData,
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
    // Requiring that PermIndex be big enough doesn't make sense with our
    // Sequence strategy--we want every member of the PermIndex to represent a
    // valid Perm.  Though, it doesn't really make a lot of sense the way we're
    // using it here either as a compact housing.
    //
    // Next, perfectly compacting a sym-coordinate isn't necessarily a good
    // idea.  Splitting a sym-coordinate will actually be a very common
    // operation, so we can look up the next possible moves on the rep index
    // alone.  It may take more memory to hold them in separate bytes (and
    // therefore less effective in our caches), but maybe be more efficient for
    // a repeated operation.  Using bitwise operations may also be effective to
    // balance compactness vs speed of splitting and combining.
    //
    // So it's quite possible that this can be optimized to efficiently pack
    // bits here, it's going to be quite fiddly to enable that degree of
    // genericness, and packing cannot every be perfectly efficient without
    // potentially compromising speed by adding multiplication and modulos.
    pub fn raw_index_to_sym_index(&self, pi: PermIndex) -> (RepIndex<Sym, PermIndex>, Sym) {
        self.to_sym_index_table[<PermIndex as Into<usize>>::into(pi)].clone()
    }

    pub fn rep_index_to_perm_index(&self, i: RepIndex<Sym, PermIndex>) -> PermIndex {
        self.table[<PermIndex as Into<usize>>::into(i.0)]
    }

    // TODO: Should MoveTable implement Sequence?
    pub fn len(&self) -> usize {
        self.table.len()
    }

    // TODO: Should MoveTable implement Sequence?
    pub fn rep_indexes(&self) -> impl Iterator<Item = RepIndex<Sym, PermIndex>> + '_ {
        (0..self.table.len()).map(|ri| RepIndex(ri.try_into().expect("Invariant violated: the size of the rep table exceeded PermIndexes maximum bound."), std::marker::PhantomData))
    }

    pub fn is_self_symmetric(&self, ri: RepIndex<Sym, PermIndex>) -> bool {
        let i: usize = ri.0.into();
        ((self.self_symmetric_table[i / 8] >> (i % 8)) & 1) != 0
    }
}

// We consider all symmetries for a position, and return the smallest,
// according to the Ord implementation.  We also return the symmetry (which may
// be the Identity), and we return whether or not this position is
// self-symmetric in any way.  Self-symmetric means that an original and
// symmetric equivalent state are identical.  For example, the identity state
// will always be self-symmetric (assuming we have meaningful symmetry, rather
// than `enum Sym { Identitiy }`).  We return this value, because it indicates
// whether or not the returned symmetry is the _only_ symmetry that does this
// reduction, or if there are other valid options.  Since self-symmetric cases
// are rare, this allows us to skip searching through all valid symmetries in
// the cases where we need to consider them all, like composite move tables.
fn smallest_equivalence<Perm: Clone + EquivalenceClass<Sym> + Into<PermIndex>, Sym: Sequence, PermIndex: Ord>(perm: &Perm) -> (PermIndex, Sym, bool) {
    let mut smallest: Option<PermIndex> = None;
    let mut sym: Option<Sym> = None;
    let mut is_self_symmetric = false;

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

        if smallest.as_ref().map(|pi| &ei == pi).unwrap_or(false) {
            is_self_symmetric = true;
        }

        if smallest.as_ref().map(|pi| &ei < pi).unwrap_or(true) {
            smallest = Some(ei);
            sym = Some(s);
            is_self_symmetric = false;
        }
    }
    (smallest.expect("Invariant Violation: Did not find a smallest symmetry!"), sym.expect("Invariant Violation: Did not find a smallest symmetry!"), is_self_symmetric)
}

#[cfg(test)]
mod tests {
    use super::*;
    use two_triangles::*;
    use enum_iterator::cardinality;

    fn test<Sym: Sequence + Clone, PermIndex: Sequence + Copy + Ord + TryFrom<usize> + Into<usize> + Into<Perm> + std::fmt::Debug, Perm: Clone + EquivalenceClass<Sym> + Into<PermIndex>>(len: usize) where <PermIndex as TryFrom<usize>>::Error: std::fmt::Debug {
        let rep_table: RepresentativeTable<Sym, PermIndex> = RepresentativeTable::new::<Perm>();

        // Finds the expected number
        assert_eq!(rep_table.table.len(), len);

        // Is ordered without duplicates
        for i in 1..rep_table.table.len() {
            assert_eq!(rep_table.table[i - 1] < rep_table.table[i], true);
        }

        // raw_index_to_sym_index round trips
        for pi in all::<PermIndex>() {
            let (ri, sym) = rep_table.raw_index_to_sym_index(pi);
            let rep = Into::<Perm>::into(pi).get_equivalent(&sym);
            assert_eq!(rep_table.rep_index_to_perm_index(ri), rep.into())
        }

        // Number of self-symmetric positions does not exceed possible bound
        let mut count = 0;
        for ri in rep_table.rep_indexes() {
            count += cardinality::<Sym>() - (if rep_table.is_self_symmetric(ri) { 1 } else { 0 });
        }
        assert_eq!(count >= cardinality::<PermIndex>(), true);
    }

    #[test]
    fn representative_table_is_correct_for_two_triangles_without_symmetry() {
        test::<NoSymmetry, TwoTrianglesIndex, TwoTriangles>(120);
    }

    #[test]
    fn representative_table_is_correct_for_two_triangles_with_rotational_symmetry() {
        test::<RotationalSymmetry, TwoTrianglesIndex, TwoTriangles>(64);
    }

    #[test]
    fn representative_table_is_correct_for_two_triangles_even_perms_without_symmetry() {
        test::<NoSymmetry, TwoTrianglesEvenIndex, TwoTriangles>(60);
    }

    #[test]
    fn representative_table_is_correct_for_two_triangles_even_perms_with_rotational_symmetry() {
        test::<RotationalSymmetry, TwoTrianglesEvenIndex, TwoTriangles>(32);
    }

    #[test]
    fn representative_table_is_correct_for_two_triangles_even_perms_with_full_symmetry() {
        test::<FullSymmetry, TwoTrianglesEvenIndex, TwoTriangles>(18);
    }

    use three_triangles;

    #[test]
    fn representative_table_is_correct_for_three_triangles_without_symmetry() {
        test::<three_triangles::NoSymmetry, three_triangles::ThreeTrianglesIndex, three_triangles::ThreeTriangles>(24);
    }

    #[test]
    fn representative_table_is_correct_for_three_triangles_with_rotational_symmetry() {
        test::<three_triangles::RotationalSymmetry, three_triangles::ThreeTrianglesIndex, three_triangles::ThreeTriangles>(10);
    }

    #[test]
    fn representative_table_is_correct_for_three_triangles_with_full_symmetry() {
        test::<three_triangles::FullSymmetry, three_triangles::ThreeTrianglesIndex, three_triangles::ThreeTriangles>(7);
    }

    #[test]
    fn representative_table_is_correct_for_three_triangles_even_perms_without_symmetry() {
        test::<three_triangles::NoSymmetry, three_triangles::ThreeTrianglesEvenIndex, three_triangles::ThreeTriangles>(12);
    }

    #[test]
    fn representative_table_is_correct_for_three_triangles_even_perms_with_rotational_symmetry() {
        test::<three_triangles::RotationalSymmetry, three_triangles::ThreeTrianglesEvenIndex, three_triangles::ThreeTriangles>(6);
    }

    #[test]
    fn representative_table_is_correct_for_three_triangles_even_perms_with_full_symmetry() {
        test::<three_triangles::FullSymmetry, three_triangles::ThreeTrianglesEvenIndex, three_triangles::ThreeTriangles>(4);
    }

    use three_trapezoids;

    #[test]
    fn representative_table_is_correct_for_three_trapezoids_without_symmetry() {
        test::<three_trapezoids::NoSymmetry, three_trapezoids::ThreeTrapezoidsIndex, three_trapezoids::ThreeTrapezoids>(720);
    }

    #[test]
    fn representative_table_is_correct_for_three_trapezoids_with_mirror_ud_symmetry() {
        // TODO: Never really validated the count, but it at least make senses
        test::<three_trapezoids::MirrorUDSymmetry, three_trapezoids::ThreeTrapezoidsIndex, three_trapezoids::ThreeTrapezoids>(368);
    }

    #[test]
    fn representative_table_is_correct_for_three_trapezoids_with_rotational_symmetry() {
        // TODO: Never really validated the count, but it at least make senses
        test::<three_trapezoids::RotationalSymmetry, three_trapezoids::ThreeTrapezoidsIndex, three_trapezoids::ThreeTrapezoids>(252);
    }

    #[test]
    fn representative_table_is_correct_for_three_trapezoids_with_full_symmetry() {
        // TODO: Never really validated the count, but it at least make senses
        test::<three_trapezoids::FullSymmetry, three_trapezoids::ThreeTrapezoidsIndex, three_trapezoids::ThreeTrapezoids>(134);
    }
}
