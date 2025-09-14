use permutation_group::PermutationGroup as PG;
use invertable::Invertable;
use equivalence_class::EquivalenceClass;
use coord_wing_edges::CoordWingEdges;
use move_sets::g1_wide_turns::G1WideTurn;
use move_sets::g1a_wide_turns::G1aWideTurn;
use symmetries::cube::{UF2Symmetry, U2F2Symmetry, U2Symmetry};

use std::convert::{TryInto, TryFrom};
use enum_iterator::Sequence;

// This module is an orbit of of CoordWingEdges under a subset of turns,
// tracing only the positions of the slice pieces between the up and down face,
// but assuming they cannot move out of the middle slice locations.  So there
// are only eight entries: eight pieces in eight locations.
//
// We can only apply turns that don't disrupt the middle slice.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct CoordWingEdgesLockedUDSlice([u8; 8]);

impl TryFrom<CoordWingEdges> for CoordWingEdgesLockedUDSlice {
    type Error = ();

    fn try_from(p: CoordWingEdges) -> Result<Self, Self::Error> {
        for i in 8..16 {
            if p.0[i] < 8 || p.0[i] >= 16 {
                Err(())?;
            }
        }

        Ok(CoordWingEdgesLockedUDSlice([
            p.0[8] - 8,
            p.0[9] - 8,
            p.0[10] - 8,
            p.0[11] - 8,
            p.0[12] - 8,
            p.0[13] - 8,
            p.0[14] - 8,
            p.0[15] - 8,
        ]))
    }
}

const fn permute_arr(a: &[u8; 8], b: &[u8; 8]) -> [u8; 8] {
    [
        b[a[0] as usize],
        b[a[1] as usize],
        b[a[2] as usize],
        b[a[3] as usize],
        b[a[4] as usize],
        b[a[5] as usize],
        b[a[6] as usize],
        b[a[7] as usize],
    ]
}

const fn arr_inv(a: &[u8; 8]) -> [u8; 8] {
    let mut r = [0; 8];
    r[a[0] as usize] = 0;
    r[a[1] as usize] = 1;
    r[a[2] as usize] = 2;
    r[a[3] as usize] = 3;
    r[a[4] as usize] = 4;
    r[a[5] as usize] = 5;
    r[a[6] as usize] = 6;
    r[a[7] as usize] = 7;
    r
}

impl functional::BinaryOperation<CoordWingEdgesLockedUDSlice> for CoordWingEdgesLockedUDSlice {
    fn apply(a: CoordWingEdgesLockedUDSlice, b: CoordWingEdgesLockedUDSlice) -> CoordWingEdgesLockedUDSlice {
        CoordWingEdgesLockedUDSlice(permute_arr(&a.0, &b.0))
    }
}

impl functional::AssociativeOperation<CoordWingEdgesLockedUDSlice> for CoordWingEdgesLockedUDSlice { }

impl functional::Monoid<CoordWingEdgesLockedUDSlice> for CoordWingEdgesLockedUDSlice {
    fn one() -> CoordWingEdgesLockedUDSlice {
        CoordWingEdgesLockedUDSlice([0, 1, 2, 3, 4, 5, 6, 7])
    }
}

impl Invertable for CoordWingEdgesLockedUDSlice {
    fn invert(&self) -> CoordWingEdgesLockedUDSlice {
        CoordWingEdgesLockedUDSlice(arr_inv(&self.0))
    }
}

impl PG for CoordWingEdgesLockedUDSlice {}

impl From<G1aWideTurn> for CoordWingEdgesLockedUDSlice {
    fn from(x: G1aWideTurn) -> Self {
        let x: CoordWingEdges = x.into();
        x.try_into().expect("Invariant Violation: G1aWideTurn should not move pieces outside of UD slice.")
    }
}

impl From<G1WideTurn> for CoordWingEdgesLockedUDSlice {
    fn from(x: G1WideTurn) -> Self {
        let x: CoordWingEdges = x.into();
        x.try_into().expect("Invariant Violation: G1WideTurn should not move pieces outside of UD slice.")
    }
}

impl Into<CoordWingEdgesLockedUDSlice> for U2Symmetry {
    fn into(self) -> CoordWingEdgesLockedUDSlice {
        match self {
            U2Symmetry::Identity => CoordWingEdgesLockedUDSlice::identity(),
            U2Symmetry::U2 => CoordWingEdgesLockedUDSlice::try_from(super::S_U2).expect("Invariant Violation: U2 symmetry should not move pieces outside of UD slice."),
        }
    }
}

impl EquivalenceClass<U2Symmetry> for CoordWingEdgesLockedUDSlice {
    fn get_equivalent(self, sym: &U2Symmetry) -> CoordWingEdgesLockedUDSlice {
        let x: CoordWingEdgesLockedUDSlice = sym.clone().into();
        x.invert().permute(self).permute(x)
    }
}

impl Into<CoordWingEdgesLockedUDSlice> for U2F2Symmetry {
    fn into(self) -> CoordWingEdgesLockedUDSlice {
        match self {
            U2F2Symmetry::Identity => CoordWingEdgesLockedUDSlice::identity(),
            U2F2Symmetry::U2 => CoordWingEdgesLockedUDSlice::try_from(super::S_U2).expect("Invariant Violation: U2 symmetry should not move pieces outside of UD slice."),
            U2F2Symmetry::F2 => CoordWingEdgesLockedUDSlice::try_from(super::S_F2).expect("Invariant Violation: F2 symmetry should not move pieces outside of UD slice."),
            U2F2Symmetry::U2F2 => CoordWingEdgesLockedUDSlice::try_from(super::S_U2F2).expect("Invariant Violation: U2F2 symmetry should not move pieces outside of UD slice."),
        }
    }
}

impl EquivalenceClass<U2F2Symmetry> for CoordWingEdgesLockedUDSlice {
    fn get_equivalent(self, sym: &U2F2Symmetry) -> CoordWingEdgesLockedUDSlice {
        let x: CoordWingEdgesLockedUDSlice = sym.clone().into();
        x.invert().permute(self).permute(x)
    }
}

impl Into<CoordWingEdgesLockedUDSlice> for UF2Symmetry {
    fn into(self) -> CoordWingEdgesLockedUDSlice {
        match self {
            UF2Symmetry::Identity => CoordWingEdgesLockedUDSlice::identity(),
            UF2Symmetry::U => CoordWingEdgesLockedUDSlice::try_from(super::S_U).expect("Invariant Violation: U symmetry should not move pieces outside of UD slice."),
            UF2Symmetry::U2 => CoordWingEdgesLockedUDSlice::try_from(super::S_U2).expect("Invariant Violation: U2 symmetry should not move pieces outside of UD slice."),
            UF2Symmetry::UPrime => CoordWingEdgesLockedUDSlice::try_from(super::S_U_PRIME).expect("Invariant Violation: UPrime symmetry should not move pieces outside of UD slice."),
            UF2Symmetry::F2 => CoordWingEdgesLockedUDSlice::try_from(super::S_F2).expect("Invariant Violation: F2 symmetry should not move pieces outside of UD slice."),
            UF2Symmetry::UF2 => CoordWingEdgesLockedUDSlice::try_from(super::S_UF2).expect("Invariant Violation: UF2 symmetry should not move pieces outside of UD slice."),
            UF2Symmetry::U2F2 => CoordWingEdgesLockedUDSlice::try_from(super::S_U2F2).expect("Invariant Violation: U2F2 symmetry should not move pieces outside of UD slice."),
            UF2Symmetry::UPrimeF2 => CoordWingEdgesLockedUDSlice::try_from(super::S_U_PRIME_F2).expect("Invariant Violation: UPrimeF2 symmetry should not move pieces outside of UD slice."),
        }
    }
}

impl EquivalenceClass<UF2Symmetry> for CoordWingEdgesLockedUDSlice {
    fn get_equivalent(self, sym: &UF2Symmetry) -> CoordWingEdgesLockedUDSlice {
        let x: CoordWingEdgesLockedUDSlice = sym.clone().into();
        x.invert().permute(self).permute(x)
    }
}

// TODO: There are faster algorithms than this
fn to_lehmer(p: CoordWingEdgesLockedUDSlice) -> u16 {
    let mut x = p.0.clone();
    for i in 0..8 {
        for j in (i+1)..8 {
            if x[j] > x[i] {
                x[j] -= 1;
            }
        }
    }
    // NOTE that x[7] is guaranteed to be 0
    5040 * (x[0] as u16) + 720 * (x[1] as u16) + 120 * (x[2] as u16) + 24 * (x[3] as u16) + 6 * (x[4] as u16) + 2 * (x[5] as u16) + (x[6] as u16)
}

// TODO: There are faster algorithms than this
fn from_lehmer(i: u16) -> CoordWingEdgesLockedUDSlice {
    let mut x =
        [ (i / 5040) as u8
        , ((i / 720) % 7) as u8
        , ((i / 120) % 6) as u8
        , ((i / 24) % 5) as u8
        , ((i / 6) % 4) as u8
        , ((i / 2) % 3) as u8
        , (i % 2) as u8
        , 0
        ];

    for i in (0..8).rev() {
        for j in (i+1)..8 {
            if x[j as usize] >= x[i] {
                x[j as usize] += 1;
            }
        }
    }
    CoordWingEdgesLockedUDSlice(x)
}

// TODO: For G1aWideTuns, this permutation is totally correct, we need all
// positions and there's odd parity.  However, for G1WideTurns, we actually
// have 4!*4!/2; the two orbits have overall even parity.  This more general
// permutation works fine, but it could be more specific.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
pub struct CoordWingEdgesLockedUDSliceIndex(u16);

impl Sequence for CoordWingEdgesLockedUDSliceIndex {
    const CARDINALITY: usize = 40320;

    fn next(&self) -> Option<Self> {
        if self.0 == (Self::CARDINALITY - 1) as u16 {
            None
        } else {
            Some(CoordWingEdgesLockedUDSliceIndex(self.0 + 1))
        }
    }

    fn previous(&self) -> Option<Self> {
        if self.0 == 0 {
            None
        } else {
            Some(CoordWingEdgesLockedUDSliceIndex(self.0 - 1))
        }
    }

    fn first() -> Option<Self> {
        Some(CoordWingEdgesLockedUDSliceIndex(0))
    }

    fn last() -> Option<Self> {
        Some(CoordWingEdgesLockedUDSliceIndex((Self::CARDINALITY - 1) as u16))
    }
}

impl Into<CoordWingEdgesLockedUDSlice> for CoordWingEdgesLockedUDSliceIndex {
    fn into(self) -> CoordWingEdgesLockedUDSlice {
        from_lehmer(self.0)
    }
}

impl Into<usize> for CoordWingEdgesLockedUDSliceIndex {
    fn into(self) -> usize {
        self.0 as usize
    }
}

impl TryFrom<usize> for CoordWingEdgesLockedUDSliceIndex {
    type Error = std::num::TryFromIntError;

    fn try_from(i: usize) -> Result<Self, Self::Error> {
        let j = i.try_into()?;
        Ok(CoordWingEdgesLockedUDSliceIndex(j))
    }
}

impl Into<CoordWingEdgesLockedUDSliceIndex> for CoordWingEdgesLockedUDSlice {
    fn into(self) -> CoordWingEdgesLockedUDSliceIndex {
        CoordWingEdgesLockedUDSliceIndex(to_lehmer(self))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck::Gen;
    use rand::Rng;
    use enum_iterator::{all, cardinality};

    impl quickcheck::Arbitrary for CoordWingEdgesLockedUDSliceIndex {
        fn arbitrary<G: Gen>(g: &mut G) -> CoordWingEdgesLockedUDSliceIndex {
            CoordWingEdgesLockedUDSliceIndex(g.gen_range(0, cardinality::<CoordWingEdgesLockedUDSliceIndex>() as u16))
        }
    }

    #[test]
    fn all_lehmer_codes_round_trip() {
        for a in all::<CoordWingEdgesLockedUDSliceIndex>() {
            let b: CoordWingEdgesLockedUDSlice = a.into();
            assert_eq!(a, b.into());
        }
    }

    quickcheck! {
        fn permutation_is_associative(pi0: CoordWingEdgesLockedUDSliceIndex, pi1: CoordWingEdgesLockedUDSliceIndex, pi2: CoordWingEdgesLockedUDSliceIndex) -> bool {
            let p0: CoordWingEdgesLockedUDSlice = pi0.into();
            let p1: CoordWingEdgesLockedUDSlice = pi1.into();
            let p2: CoordWingEdgesLockedUDSlice = pi2.into();
            p0.permute(p1).permute(p2) == p0.permute(p1.permute(p2))
        }
    }

    quickcheck! {
        fn identity_has_no_effect(pi: CoordWingEdgesLockedUDSliceIndex) -> bool {
            let p: CoordWingEdgesLockedUDSlice = pi.into();
            p.permute(CoordWingEdgesLockedUDSlice::identity()) == p
                && CoordWingEdgesLockedUDSlice::identity().permute(p) == p
        }
    }

    quickcheck! {
        fn inversion_is_identity(pi: CoordWingEdgesLockedUDSliceIndex) -> bool {
            let p: CoordWingEdgesLockedUDSlice = pi.into();
            p.permute(p.invert()) == CoordWingEdgesLockedUDSlice::identity()
        }
    }

    quickcheck! {
        fn perm_and_g1a_turns_and_sym_invert_round_trips(pi: CoordWingEdgesLockedUDSliceIndex, t: G1aWideTurn, s: U2F2Symmetry) -> bool {
            let p: CoordWingEdgesLockedUDSlice = pi.into();
            p == p.invert().invert()
                && p == p.permute(t.into()).permute(t.invert().into())
                && p == p.permute(s.into()).permute(s.invert().into())
        }
    }

    quickcheck! {
        fn perm_and_g1a_turn_full_symmetries_are_equivalent_through_uf2(pi: CoordWingEdgesLockedUDSliceIndex, t: G1aWideTurn, s: UF2Symmetry) -> bool {
            let p: CoordWingEdgesLockedUDSlice = pi.into();
            let after_permute = p.permute(t.into()).get_equivalent(&s);
            let before_permute = p.get_equivalent(&s).permute(t.get_equivalent(&s).into());
            after_permute == before_permute
        }
    }

    quickcheck! {
        fn perm_and_g1a_turn_full_symmetries_are_equivalent_through_u2f2(pi: CoordWingEdgesLockedUDSliceIndex, t: G1aWideTurn, s: U2F2Symmetry) -> bool {
            let p: CoordWingEdgesLockedUDSlice = pi.into();
            let after_permute = p.permute(t.into()).get_equivalent(&s);
            let before_permute = p.get_equivalent(&s).permute(t.get_equivalent(&s).into());
            after_permute == before_permute
        }
    }

    quickcheck! {
        fn perm_and_g1a_turn_full_symmetries_are_equivalent_through_u2(pi: CoordWingEdgesLockedUDSliceIndex, t: G1aWideTurn, s: U2Symmetry) -> bool {
            let p: CoordWingEdgesLockedUDSlice = pi.into();
            let after_permute = p.permute(t.into()).get_equivalent(&s);
            let before_permute = p.get_equivalent(&s).permute(t.get_equivalent(&s).into());
            after_permute == before_permute
        }
    }

    quickcheck! {
        fn perm_and_turns_and_sym_invert_round_trips(pi: CoordWingEdgesLockedUDSliceIndex, t: G1WideTurn, s: U2F2Symmetry) -> bool {
            let p: CoordWingEdgesLockedUDSlice = pi.into();
            p == p.invert().invert()
                && p == p.permute(t.into()).permute(t.invert().into())
                && p == p.permute(s.into()).permute(s.invert().into())
        }
    }

    quickcheck! {
        fn perm_and_turn_full_symmetries_are_equivalent_through_uf2(pi: CoordWingEdgesLockedUDSliceIndex, t: G1WideTurn, s: UF2Symmetry) -> bool {
            let p: CoordWingEdgesLockedUDSlice = pi.into();
            let after_permute = p.permute(t.into()).get_equivalent(&s);
            let before_permute = p.get_equivalent(&s).permute(t.get_equivalent(&s).into());
            after_permute == before_permute
        }
    }

    quickcheck! {
        fn perm_and_turn_full_symmetries_are_equivalent_through_u2f2(pi: CoordWingEdgesLockedUDSliceIndex, t: G1WideTurn, s: U2F2Symmetry) -> bool {
            let p: CoordWingEdgesLockedUDSlice = pi.into();
            let after_permute = p.permute(t.into()).get_equivalent(&s);
            let before_permute = p.get_equivalent(&s).permute(t.get_equivalent(&s).into());
            after_permute == before_permute
        }
    }

    quickcheck! {
        fn perm_and_turn_full_symmetries_are_equivalent_through_u2(pi: CoordWingEdgesLockedUDSliceIndex, t: G1WideTurn, s: U2Symmetry) -> bool {
            let p: CoordWingEdgesLockedUDSlice = pi.into();
            let after_permute = p.permute(t.into()).get_equivalent(&s);
            let before_permute = p.get_equivalent(&s).permute(t.get_equivalent(&s).into());
            after_permute == before_permute
        }
    }

    quickcheck! {
        fn full_turn_and_permutation_are_homomorphic_through_uf2(s0: UF2Symmetry, s1: UF2Symmetry) -> bool {
            let as_perm = <UF2Symmetry as Into<CoordWingEdgesLockedUDSlice>>::into(s0).permute(s1.into());
            let as_sym: CoordWingEdgesLockedUDSlice = s0.permute(s1).into();
            as_perm == as_sym
        }
    }

    quickcheck! {
        fn full_turn_and_permutation_are_homomorphic_through_u2f2(s0: U2F2Symmetry, s1: U2F2Symmetry) -> bool {
            let as_perm = <U2F2Symmetry as Into<CoordWingEdgesLockedUDSlice>>::into(s0).permute(s1.into());
            let as_sym: CoordWingEdgesLockedUDSlice = s0.permute(s1).into();
            as_perm == as_sym
        }
    }

    quickcheck! {
        fn full_turn_and_permutation_are_homomorphic_through_u2(s0: U2Symmetry, s1: U2Symmetry) -> bool {
            let as_perm = <U2Symmetry as Into<CoordWingEdgesLockedUDSlice>>::into(s0).permute(s1.into());
            let as_sym: CoordWingEdgesLockedUDSlice = s0.permute(s1).into();
            as_perm == as_sym
        }
    }

    quickcheck! {
        // This should technically already be proven true by being homomorphic
        fn direct_and_indirect_sym_multiplication_are_equivalent_for_uf2_symmetry(pi: CoordWingEdgesLockedUDSliceIndex, s0: UF2Symmetry, s1: UF2Symmetry) -> bool {
            let p: CoordWingEdgesLockedUDSlice = pi.into();
            let as_perm = p.permute(s0.into()).permute(s1.into());
            let as_sym = p.permute(s0.permute(s1).into());
            as_perm == as_sym
        }
    }

    quickcheck! {
        // This should technically already be proven true by being homomorphic
        fn direct_and_indirect_sym_multiplication_are_equivalent_for_u2f2_symmetry(pi: CoordWingEdgesLockedUDSliceIndex, s0: U2F2Symmetry, s1: U2F2Symmetry) -> bool {
            let p: CoordWingEdgesLockedUDSlice = pi.into();
            let as_perm = p.permute(s0.into()).permute(s1.into());
            let as_sym = p.permute(s0.permute(s1).into());
            as_perm == as_sym
        }
    }

    quickcheck! {
        // This should technically already be proven true by being homomorphic
        fn direct_and_indirect_sym_multiplication_are_equivalent_for_u2_symmetry(pi: CoordWingEdgesLockedUDSliceIndex, s0: U2Symmetry, s1: U2Symmetry) -> bool {
            let p: CoordWingEdgesLockedUDSlice = pi.into();
            let as_perm = p.permute(s0.into()).permute(s1.into());
            let as_sym = p.permute(s0.permute(s1).into());
            as_perm == as_sym
        }
    }
}
