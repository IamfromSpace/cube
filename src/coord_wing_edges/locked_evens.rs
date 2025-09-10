pub mod top_six;
pub mod ud;
pub mod ud_slice;

use permutation_group::PermutationGroup as PG;
use invertable::Invertable;
use equivalence_class::EquivalenceClass;
use coord_wing_edges::CoordWingEdges;
use move_sets::h1_wide_turns::H1WideTurn;
use move_sets::g1_wide_turns::G1WideTurn;
use symmetries::cube::{U2F2Symmetry, U2Symmetry};

use std::convert::{TryInto, TryFrom};

// This module is an orbit of of CoordWingEdges under a subset of turns,
// tracing only the positions of the even pieces, but assuming they cannot be
// moved into odd locations.  So there are only twelve entries: twelve pieces
// in twelve locations.
//
// We can only apply turns that retain the even and odd split.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct CoordWingEdgesLockedEvens([u8; 12]);

impl TryFrom<CoordWingEdges> for CoordWingEdgesLockedEvens {
    type Error = ();

    fn try_from(p: CoordWingEdges) -> Result<Self, Self::Error> {
        for i in 0..12 {
            if p.0[i * 2] % 2 == 1 {
                Err(())?;
            }
        }

        Ok(CoordWingEdgesLockedEvens([
            p.0[0] >> 1,
            p.0[2] >> 1,
            p.0[4] >> 1,
            p.0[6] >> 1,
            p.0[8] >> 1,
            p.0[10] >> 1,
            p.0[12] >> 1,
            p.0[14] >> 1,
            p.0[16] >> 1,
            p.0[18] >> 1,
            p.0[20] >> 1,
            p.0[22] >> 1,
        ]))
    }
}

const fn permute_arr(a: &[u8; 12], b: &[u8; 12]) -> [u8; 12] {
    [
        b[a[0] as usize],
        b[a[1] as usize],
        b[a[2] as usize],
        b[a[3] as usize],
        b[a[4] as usize],
        b[a[5] as usize],
        b[a[6] as usize],
        b[a[7] as usize],
        b[a[8] as usize],
        b[a[9] as usize],
        b[a[10] as usize],
        b[a[11] as usize],
    ]
}

const fn arr_inv(a: &[u8; 12]) -> [u8; 12] {
    let mut r = [0; 12];
    r[a[0] as usize] = 0;
    r[a[1] as usize] = 1;
    r[a[2] as usize] = 2;
    r[a[3] as usize] = 3;
    r[a[4] as usize] = 4;
    r[a[5] as usize] = 5;
    r[a[6] as usize] = 6;
    r[a[7] as usize] = 7;
    r[a[8] as usize] = 8;
    r[a[9] as usize] = 9;
    r[a[10] as usize] = 10;
    r[a[11] as usize] = 11;
    r
}

impl functional::BinaryOperation<CoordWingEdgesLockedEvens> for CoordWingEdgesLockedEvens {
    fn apply(a: CoordWingEdgesLockedEvens, b: CoordWingEdgesLockedEvens) -> CoordWingEdgesLockedEvens {
        CoordWingEdgesLockedEvens(permute_arr(&a.0, &b.0))
    }
}

impl functional::AssociativeOperation<CoordWingEdgesLockedEvens> for CoordWingEdgesLockedEvens { }

impl functional::Monoid<CoordWingEdgesLockedEvens> for CoordWingEdgesLockedEvens {
    fn one() -> CoordWingEdgesLockedEvens {
        CoordWingEdgesLockedEvens([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11])
    }
}

impl Invertable for CoordWingEdgesLockedEvens {
    fn invert(&self) -> CoordWingEdgesLockedEvens {
        CoordWingEdgesLockedEvens(arr_inv(&self.0))
    }
}

impl PG for CoordWingEdgesLockedEvens {}

impl From<H1WideTurn> for CoordWingEdgesLockedEvens {
    fn from(x: H1WideTurn) -> Self {
        let x: CoordWingEdges = x.into();
        x.try_into().expect("Invariant Violation: H1WideTurn should not move pieces between even and odd orbits.")
    }
}

impl From<G1WideTurn> for CoordWingEdgesLockedEvens {
    fn from(x: G1WideTurn) -> Self {
        let x: CoordWingEdges = x.into();
        x.try_into().expect("Invariant Violation: G1WideTurn should not move pieces between even and odd orbits.")
    }
}

impl Into<CoordWingEdgesLockedEvens> for U2Symmetry {
    fn into(self) -> CoordWingEdgesLockedEvens {
        match self {
            U2Symmetry::Identity => CoordWingEdgesLockedEvens::identity(),
            U2Symmetry::U2 => CoordWingEdgesLockedEvens::try_from(super::S_U2).expect("Invariant Violation: U2 symmetry should not move pieces between even and odd orbits."),
        }
    }
}

impl EquivalenceClass<U2Symmetry> for CoordWingEdgesLockedEvens {
    fn get_equivalent(self, sym: &U2Symmetry) -> CoordWingEdgesLockedEvens {
        let x: CoordWingEdgesLockedEvens = sym.clone().into();
        x.invert().permute(self).permute(x)
    }
}

impl Into<CoordWingEdgesLockedEvens> for U2F2Symmetry {
    fn into(self) -> CoordWingEdgesLockedEvens {
        match self {
            U2F2Symmetry::Identity => CoordWingEdgesLockedEvens::identity(),
            U2F2Symmetry::U2 => CoordWingEdgesLockedEvens::try_from(super::S_U2).expect("Invariant Violation: U2 symmetry should not move pieces between even and odd orbits."),
            U2F2Symmetry::F2 => CoordWingEdgesLockedEvens::try_from(super::S_F2).expect("Invariant Violation: F2 symmetry should not move pieces between even and odd orbits."),
            U2F2Symmetry::U2F2 => CoordWingEdgesLockedEvens::try_from(super::S_U2F2).expect("Invariant Violation: U2F2 symmetry should not move pieces between even and odd orbits."),
        }
    }
}

impl EquivalenceClass<U2F2Symmetry> for CoordWingEdgesLockedEvens {
    fn get_equivalent(self, sym: &U2F2Symmetry) -> CoordWingEdgesLockedEvens {
        let x: CoordWingEdgesLockedEvens = sym.clone().into();
        x.invert().permute(self).permute(x)
    }
}

// NOTE: My assumption is that we _don't_ want an index on this permutation.
// The reasons are that a) it's too big to index anyway, b) we want to
// discourage people from building tables with it by _not_ implementing
// Sequence.


#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck::Gen;
    use rand::Rng;

    // TODO: this can be generalized, also, isn't this a property of being a permutation group?
    fn is_even_parity(x: [u8; 12]) -> bool {
        let mut visited_in_look_ahead = [false; 12];
        let mut even_cycle_count = 0;
        for i in 0..12 {
            if visited_in_look_ahead[i] == false {
                let mut j = x[i];
                if j != i as u8 {
                    let mut piece_count = 2; // i & j
                    loop {
                        visited_in_look_ahead[j as usize] = true;
                        j = x[j as usize];
                        if j == i as u8 {
                            if piece_count % 2 == 0 {
                                even_cycle_count += 1;
                            }
                            break;
                        } else {
                            piece_count += 1;
                        }
                    }
                }
            }
        }
        even_cycle_count % 2 == 0
    }

    impl quickcheck::Arbitrary for CoordWingEdgesLockedEvens {
        fn arbitrary<G: Gen>(g: &mut G) -> CoordWingEdgesLockedEvens {
            let mut x = CoordWingEdgesLockedEvens::identity();
            g.shuffle(&mut x.0);

            // If not even, do one more swap
            if !is_even_parity(x.0) {
                let tmp = x.0[0];
                x.0[0] = x.0[1];
                x.0[1] = tmp;
            }

            x
        }
    }

    quickcheck! {
        fn permutation_is_associative(p0: CoordWingEdgesLockedEvens, p1: CoordWingEdgesLockedEvens, p2: CoordWingEdgesLockedEvens) -> bool {
            p0.permute(p1).permute(p2) == p0.permute(p1.permute(p2))
        }
    }

    quickcheck! {
        fn identity_has_no_effect(p: CoordWingEdgesLockedEvens) -> bool {
            p.permute(CoordWingEdgesLockedEvens::identity()) == p
                && CoordWingEdgesLockedEvens::identity().permute(p) == p
        }
    }

    quickcheck! {
        fn inversion_is_identity(p: CoordWingEdgesLockedEvens) -> bool {
            p.permute(p.invert()) == CoordWingEdgesLockedEvens::identity()
        }
    }

    quickcheck! {
        fn perm_and_h1_turns_and_sym_invert_round_trips(p: CoordWingEdgesLockedEvens, t: H1WideTurn, s: U2F2Symmetry) -> bool {
            p == p.invert().invert()
                && p == p.permute(t.into()).permute(t.invert().into())
                && p == p.permute(s.into()).permute(s.invert().into())
        }
    }

    quickcheck! {
        fn perm_and_h1_turn_full_symmetries_are_equivalent_through_u2f2(p: CoordWingEdgesLockedEvens, t: H1WideTurn, s: U2F2Symmetry) -> bool {
            let after_permute = p.permute(t.into()).get_equivalent(&s);
            let before_permute = p.get_equivalent(&s).permute(t.get_equivalent(&s).into());
            after_permute == before_permute
        }
    }

    quickcheck! {
        fn perm_and_h1_turn_full_symmetries_are_equivalent_through_u2(p: CoordWingEdgesLockedEvens, t: H1WideTurn, s: U2Symmetry) -> bool {
            let after_permute = p.permute(t.into()).get_equivalent(&s);
            let before_permute = p.get_equivalent(&s).permute(t.get_equivalent(&s).into());
            after_permute == before_permute
        }
    }

    quickcheck! {
        fn perm_and_g1_turns_and_sym_invert_round_trips(p: CoordWingEdgesLockedEvens, t: G1WideTurn, s: U2F2Symmetry) -> bool {
            p == p.invert().invert()
                && p == p.permute(t.into()).permute(t.invert().into())
                && p == p.permute(s.into()).permute(s.invert().into())
        }
    }

    quickcheck! {
        fn perm_and_g1_turn_full_symmetries_are_equivalent_through_u2f2(p: CoordWingEdgesLockedEvens, t: G1WideTurn, s: U2F2Symmetry) -> bool {
            let after_permute = p.permute(t.into()).get_equivalent(&s);
            let before_permute = p.get_equivalent(&s).permute(t.get_equivalent(&s).into());
            after_permute == before_permute
        }
    }

    quickcheck! {
        fn perm_and_g1_turn_full_symmetries_are_equivalent_through_u2(p: CoordWingEdgesLockedEvens, t: G1WideTurn, s: U2Symmetry) -> bool {
            let after_permute = p.permute(t.into()).get_equivalent(&s);
            let before_permute = p.get_equivalent(&s).permute(t.get_equivalent(&s).into());
            after_permute == before_permute
        }
    }

    quickcheck! {
        fn full_turn_and_permutation_are_homomorphic_through_u2f2(s0: U2F2Symmetry, s1: U2F2Symmetry) -> bool {
            let as_perm = <U2F2Symmetry as Into<CoordWingEdgesLockedEvens>>::into(s0).permute(s1.into());
            let as_sym: CoordWingEdgesLockedEvens = s0.permute(s1).into();
            as_perm == as_sym
        }
    }

    quickcheck! {
        fn full_turn_and_permutation_are_homomorphic_through_u2(s0: U2Symmetry, s1: U2Symmetry) -> bool {
            let as_perm = <U2Symmetry as Into<CoordWingEdgesLockedEvens>>::into(s0).permute(s1.into());
            let as_sym: CoordWingEdgesLockedEvens = s0.permute(s1).into();
            as_perm == as_sym
        }
    }

    quickcheck! {
        // This should technically already be proven true by being homomorphic
        fn direct_and_indirect_sym_multiplication_are_equivalent_for_u2f2_symmetry(p: CoordWingEdgesLockedEvens, s0: U2F2Symmetry, s1: U2F2Symmetry) -> bool {
            let as_perm = p.permute(s0.into()).permute(s1.into());
            let as_sym = p.permute(s0.permute(s1).into());
            as_perm == as_sym
        }
    }

    quickcheck! {
        // This should technically already be proven true by being homomorphic
        fn direct_and_indirect_sym_multiplication_are_equivalent_for_u2_symmetry(p: CoordWingEdgesLockedEvens, s0: U2Symmetry, s1: U2Symmetry) -> bool {
            let as_perm = p.permute(s0.into()).permute(s1.into());
            let as_sym = p.permute(s0.permute(s1).into());
            as_perm == as_sym
        }
    }
}
