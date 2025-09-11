use permutation_group::PermutationGroup as PG;
use invertable::Invertable;
use equivalence_class::EquivalenceClass;
use coord_wing_edges::CoordWingEdges;
use move_sets::g1_wide_turns::G1WideTurn;
use symmetries::cube::{UF2Symmetry, U2F2Symmetry, U2Symmetry};

use std::convert::{TryInto, TryFrom};

// This module is an orbit of of CoordWingEdges under a subset of turns,
// tracing only the positions of the even pieces on the top face and bottom
// face, but assuming they cannot be moved into odd or middle slice locations.
// So there are only eight entries: eight pieces in eight locations.
//
// We can only apply turns that retain the even and odd split, and don't
// disrupt the middle slice.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct CoordWingEdgesLockedUDEvens([u8; 8]);

fn adjust(x: u8) -> u8 {
    if x > 6 {
        x - 8
    } else {
        x
    }
}

impl TryFrom<CoordWingEdges> for CoordWingEdgesLockedUDEvens {
    type Error = ();

    fn try_from(p: CoordWingEdges) -> Result<Self, Self::Error> {
        for i in 0..4 {
            let is_odd = p.0[i * 2] % 2 == 1 || p.0[i * 2 + 16] % 2 == 1;
            let u_is_middle = p.0[i * 2] > 6 && p.0[i * 2] < 16;
            let d_is_middle = p.0[i * 2 + 16] > 6 && p.0[i * 2 + 16] < 16;

            if is_odd || u_is_middle || d_is_middle {
                Err(())?;
            }
        }

        Ok(CoordWingEdgesLockedUDEvens([
            adjust(p.0[0]) >> 1,
            adjust(p.0[2]) >> 1,
            adjust(p.0[4]) >> 1,
            adjust(p.0[6]) >> 1,
            adjust(p.0[16]) >> 1,
            adjust(p.0[18]) >> 1,
            adjust(p.0[20]) >> 1,
            adjust(p.0[22]) >> 1,
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

impl functional::BinaryOperation<CoordWingEdgesLockedUDEvens> for CoordWingEdgesLockedUDEvens {
    fn apply(a: CoordWingEdgesLockedUDEvens, b: CoordWingEdgesLockedUDEvens) -> CoordWingEdgesLockedUDEvens {
        CoordWingEdgesLockedUDEvens(permute_arr(&a.0, &b.0))
    }
}

impl functional::AssociativeOperation<CoordWingEdgesLockedUDEvens> for CoordWingEdgesLockedUDEvens { }

impl functional::Monoid<CoordWingEdgesLockedUDEvens> for CoordWingEdgesLockedUDEvens {
    fn one() -> CoordWingEdgesLockedUDEvens {
        CoordWingEdgesLockedUDEvens([0, 1, 2, 3, 4, 5, 6, 7])
    }
}

impl Invertable for CoordWingEdgesLockedUDEvens {
    fn invert(&self) -> CoordWingEdgesLockedUDEvens {
        CoordWingEdgesLockedUDEvens(arr_inv(&self.0))
    }
}

impl PG for CoordWingEdgesLockedUDEvens {}

impl From<G1WideTurn> for CoordWingEdgesLockedUDEvens {
    fn from(x: G1WideTurn) -> Self {
        let x: CoordWingEdges = x.into();
        x.try_into().expect("Invariant Violation: G1WideTurn should not move pieces outside of UD orbit.")
    }
}

impl Into<CoordWingEdgesLockedUDEvens> for U2Symmetry {
    fn into(self) -> CoordWingEdgesLockedUDEvens {
        match self {
            U2Symmetry::Identity => CoordWingEdgesLockedUDEvens::identity(),
            U2Symmetry::U2 => CoordWingEdgesLockedUDEvens::try_from(super::S_U2).expect("Invariant Violation: U2 symmetry should not move pieces outside of UD orbit."),
        }
    }
}

impl EquivalenceClass<U2Symmetry> for CoordWingEdgesLockedUDEvens {
    fn get_equivalent(self, sym: &U2Symmetry) -> CoordWingEdgesLockedUDEvens {
        let x: CoordWingEdgesLockedUDEvens = sym.clone().into();
        x.invert().permute(self).permute(x)
    }
}

impl Into<CoordWingEdgesLockedUDEvens> for U2F2Symmetry {
    fn into(self) -> CoordWingEdgesLockedUDEvens {
        match self {
            U2F2Symmetry::Identity => CoordWingEdgesLockedUDEvens::identity(),
            U2F2Symmetry::U2 => CoordWingEdgesLockedUDEvens::try_from(super::S_U2).expect("Invariant Violation: U2 symmetry should not move pieces outside of UD orbit."),
            U2F2Symmetry::F2 => CoordWingEdgesLockedUDEvens::try_from(super::S_F2).expect("Invariant Violation: F2 symmetry should not move pieces outside of UD orbit."),
            U2F2Symmetry::U2F2 => CoordWingEdgesLockedUDEvens::try_from(super::S_U2F2).expect("Invariant Violation: U2F2 symmetry should not move pieces outside of UD orbit."),
        }
    }
}

impl EquivalenceClass<U2F2Symmetry> for CoordWingEdgesLockedUDEvens {
    fn get_equivalent(self, sym: &U2F2Symmetry) -> CoordWingEdgesLockedUDEvens {
        let x: CoordWingEdgesLockedUDEvens = sym.clone().into();
        x.invert().permute(self).permute(x)
    }
}

impl Into<CoordWingEdgesLockedUDEvens> for UF2Symmetry {
    fn into(self) -> CoordWingEdgesLockedUDEvens {
        match self {
            UF2Symmetry::Identity => CoordWingEdgesLockedUDEvens::identity(),
            UF2Symmetry::U => CoordWingEdgesLockedUDEvens::try_from(super::S_U).expect("Invariant Violation: U symmetry should not move pieces outside of UD orbit."),
            UF2Symmetry::U2 => CoordWingEdgesLockedUDEvens::try_from(super::S_U2).expect("Invariant Violation: U2 symmetry should not move pieces outside of UD orbit."),
            UF2Symmetry::UPrime => CoordWingEdgesLockedUDEvens::try_from(super::S_U_PRIME).expect("Invariant Violation: UPrime symmetry should not move pieces outside of UD orbit."),
            UF2Symmetry::F2 => CoordWingEdgesLockedUDEvens::try_from(super::S_F2).expect("Invariant Violation: F2 symmetry should not move pieces outside of UD orbit."),
            UF2Symmetry::UF2 => CoordWingEdgesLockedUDEvens::try_from(super::S_UF2).expect("Invariant Violation: UF2 symmetry should not move pieces outside of UD orbit."),
            UF2Symmetry::U2F2 => CoordWingEdgesLockedUDEvens::try_from(super::S_U2F2).expect("Invariant Violation: U2F2 symmetry should not move pieces outside of UD orbit."),
            UF2Symmetry::UPrimeF2 => CoordWingEdgesLockedUDEvens::try_from(super::S_U_PRIME_F2).expect("Invariant Violation: UPrimeF2 symmetry should not move pieces outside of UD orbit."),
        }
    }
}

impl EquivalenceClass<UF2Symmetry> for CoordWingEdgesLockedUDEvens {
    fn get_equivalent(self, sym: &UF2Symmetry) -> CoordWingEdgesLockedUDEvens {
        let x: CoordWingEdgesLockedUDEvens = sym.clone().into();
        x.invert().permute(self).permute(x)
    }
}

// NOTE: My assumption is that we _don't_ want an index on this permutation.
// The reasons are that a) it's quite big to index anyway, b) we want to
// discourage people from building tables with it by _not_ implementing
// Sequence.


#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck::Gen;
    use rand::Rng;

    impl quickcheck::Arbitrary for CoordWingEdgesLockedUDEvens {
        fn arbitrary<G: Gen>(g: &mut G) -> CoordWingEdgesLockedUDEvens {
            let mut x = CoordWingEdgesLockedUDEvens::identity();
            g.shuffle(&mut x.0);
            x
        }
    }

    quickcheck! {
        fn permutation_is_associative(p0: CoordWingEdgesLockedUDEvens, p1: CoordWingEdgesLockedUDEvens, p2: CoordWingEdgesLockedUDEvens) -> bool {
            p0.permute(p1).permute(p2) == p0.permute(p1.permute(p2))
        }
    }

    quickcheck! {
        fn identity_has_no_effect(p: CoordWingEdgesLockedUDEvens) -> bool {
            p.permute(CoordWingEdgesLockedUDEvens::identity()) == p
                && CoordWingEdgesLockedUDEvens::identity().permute(p) == p
        }
    }

    quickcheck! {
        fn inversion_is_identity(p: CoordWingEdgesLockedUDEvens) -> bool {
            p.permute(p.invert()) == CoordWingEdgesLockedUDEvens::identity()
        }
    }

    quickcheck! {
        fn perm_and_turns_and_sym_invert_round_trips(p: CoordWingEdgesLockedUDEvens, t: G1WideTurn, s: U2F2Symmetry) -> bool {
            p == p.invert().invert()
                && p == p.permute(t.into()).permute(t.invert().into())
                && p == p.permute(s.into()).permute(s.invert().into())
        }
    }

    quickcheck! {
        fn perm_and_turn_full_symmetries_are_equivalent_through_uf2(p: CoordWingEdgesLockedUDEvens, t: G1WideTurn, s: UF2Symmetry) -> bool {
            let after_permute = p.permute(t.into()).get_equivalent(&s);
            let before_permute = p.get_equivalent(&s).permute(t.get_equivalent(&s).into());
            after_permute == before_permute
        }
    }

    quickcheck! {
        fn perm_and_turn_full_symmetries_are_equivalent_through_u2f2(p: CoordWingEdgesLockedUDEvens, t: G1WideTurn, s: U2F2Symmetry) -> bool {
            let after_permute = p.permute(t.into()).get_equivalent(&s);
            let before_permute = p.get_equivalent(&s).permute(t.get_equivalent(&s).into());
            after_permute == before_permute
        }
    }

    quickcheck! {
        fn perm_and_turn_full_symmetries_are_equivalent_through_u2(p: CoordWingEdgesLockedUDEvens, t: G1WideTurn, s: U2Symmetry) -> bool {
            let after_permute = p.permute(t.into()).get_equivalent(&s);
            let before_permute = p.get_equivalent(&s).permute(t.get_equivalent(&s).into());
            after_permute == before_permute
        }
    }

    quickcheck! {
        fn full_turn_and_permutation_are_homomorphic_through_uf2(s0: UF2Symmetry, s1: UF2Symmetry) -> bool {
            let as_perm = <UF2Symmetry as Into<CoordWingEdgesLockedUDEvens>>::into(s0).permute(s1.into());
            let as_sym: CoordWingEdgesLockedUDEvens = s0.permute(s1).into();
            as_perm == as_sym
        }
    }

    quickcheck! {
        fn full_turn_and_permutation_are_homomorphic_through_u2f2(s0: U2F2Symmetry, s1: U2F2Symmetry) -> bool {
            let as_perm = <U2F2Symmetry as Into<CoordWingEdgesLockedUDEvens>>::into(s0).permute(s1.into());
            let as_sym: CoordWingEdgesLockedUDEvens = s0.permute(s1).into();
            as_perm == as_sym
        }
    }

    quickcheck! {
        fn full_turn_and_permutation_are_homomorphic_through_u2(s0: U2Symmetry, s1: U2Symmetry) -> bool {
            let as_perm = <U2Symmetry as Into<CoordWingEdgesLockedUDEvens>>::into(s0).permute(s1.into());
            let as_sym: CoordWingEdgesLockedUDEvens = s0.permute(s1).into();
            as_perm == as_sym
        }
    }

    quickcheck! {
        // This should technically already be proven true by being homomorphic
        fn direct_and_indirect_sym_multiplication_are_equivalent_for_uf2_symmetry(p: CoordWingEdgesLockedUDEvens, s0: UF2Symmetry, s1: UF2Symmetry) -> bool {
            let as_perm = p.permute(s0.into()).permute(s1.into());
            let as_sym = p.permute(s0.permute(s1).into());
            as_perm == as_sym
        }
    }

    quickcheck! {
        // This should technically already be proven true by being homomorphic
        fn direct_and_indirect_sym_multiplication_are_equivalent_for_u2f2_symmetry(p: CoordWingEdgesLockedUDEvens, s0: U2F2Symmetry, s1: U2F2Symmetry) -> bool {
            let as_perm = p.permute(s0.into()).permute(s1.into());
            let as_sym = p.permute(s0.permute(s1).into());
            as_perm == as_sym
        }
    }

    quickcheck! {
        // This should technically already be proven true by being homomorphic
        fn direct_and_indirect_sym_multiplication_are_equivalent_for_u2_symmetry(p: CoordWingEdgesLockedUDEvens, s0: U2Symmetry, s1: U2Symmetry) -> bool {
            let as_perm = p.permute(s0.into()).permute(s1.into());
            let as_sym = p.permute(s0.permute(s1).into());
            as_perm == as_sym
        }
    }
}
