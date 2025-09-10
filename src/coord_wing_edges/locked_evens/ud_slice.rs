use permutation_group::PermutationGroup as PG;
use invertable::Invertable;
use equivalence_class::EquivalenceClass;
use algebraic_actions::{MagmaAction, MonoidAction, LeftMagmaAction, LeftMonoidAction};
use super::CoordWingEdgesLockedEvens;
use move_sets::h1_wide_turns::H1WideTurn;
use move_sets::g1_wide_turns::G1WideTurn;
use symmetries::cube::U2F2Symmetry;

use std::convert::{TryInto, TryFrom};
use enum_iterator::Sequence;

// This module is a pattern of CoordWingEdgesLockedEvens, tracing only the
// positions of the eight top and bottom pieces.  So there are only eight
// entries, despite being 12 pieces in 12 locations.  Each entry tracks the
// current location of a single piece, so [0, 1, 2, 3, 8, 9, 10, 11] is when
// the tracked pieces are solved.
//
// The positions of the untracked positions are completely unknown.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct CoordWingEdgesLockedEvensUDSlice([u8; 4]);

impl From<CoordWingEdgesLockedEvens> for CoordWingEdgesLockedEvensUDSlice {
    fn from(t: CoordWingEdgesLockedEvens) -> Self {
        CoordWingEdgesLockedEvensUDSlice([t.0[4], t.0[5], t.0[6], t.0[7]])
    }
}

fn arr_act(a: [u8; 4], b: [u8; 12]) -> [u8; 4] {
    [
        b[a[0] as usize],
        b[a[1] as usize],
        b[a[2] as usize],
        b[a[3] as usize],
    ]
}

fn arr_act_left(a: [u8; 12], b: [u8; 4]) -> [u8; 4] {
    [
        b[a[4] as usize - 4],
        b[a[5] as usize - 4],
        b[a[6] as usize - 4],
        b[a[7] as usize - 4],
    ]
}

impl MagmaAction<CoordWingEdgesLockedEvens> for CoordWingEdgesLockedEvensUDSlice {
    fn act(self, b: CoordWingEdgesLockedEvens) -> Self {
        Self(arr_act(self.0, b.0))
    }
}

impl MonoidAction<CoordWingEdgesLockedEvens> for CoordWingEdgesLockedEvensUDSlice {}

impl MagmaAction<U2F2Symmetry> for CoordWingEdgesLockedEvensUDSlice {
    fn act(self, s: U2F2Symmetry) -> Self {
        let t: CoordWingEdgesLockedEvens = s.into();
        self.act(t)
    }
}

impl MonoidAction<U2F2Symmetry> for CoordWingEdgesLockedEvensUDSlice {}

impl LeftMagmaAction<U2F2Symmetry> for CoordWingEdgesLockedEvensUDSlice {
    fn act_left(s: U2F2Symmetry, t: CoordWingEdgesLockedEvensUDSlice) -> Self {
        let s: CoordWingEdgesLockedEvens = s.into();
        CoordWingEdgesLockedEvensUDSlice(arr_act_left(s.0, t.0))
    }
}

impl LeftMonoidAction<U2F2Symmetry> for CoordWingEdgesLockedEvensUDSlice {}

impl EquivalenceClass<U2F2Symmetry> for CoordWingEdgesLockedEvensUDSlice {
    fn get_equivalent(self, sym: &U2F2Symmetry) -> CoordWingEdgesLockedEvensUDSlice {
        CoordWingEdgesLockedEvensUDSlice::act_left(sym.invert(), self).act(*sym)
    }
}

// TODO: There are faster algorithms than this
fn to_lehmer(p: CoordWingEdgesLockedEvensUDSlice) -> u16 {
    let mut x = p.0.clone();
    for i in 0..4 {
        for j in (i+1)..4 {
            if x[j] > x[i] {
                x[j] -= 1;
            }
        }
    }
    990 * x[0] as u16 + 90 * x[1] as u16 + 9 * x[2] as u16 + x[3] as u16
}

// TODO: There are faster algorithms than this
fn from_lehmer(i: u16) -> CoordWingEdgesLockedEvensUDSlice {
    let mut x =
        [ (i / 990) as u8
        , ((i / 90) % 11) as u8
        , ((i / 9) % 10) as u8
        , (i % 9) as u8
        ];
    for i in (0..4).rev() {
        for j in (i+1)..4 {
            if x[j as usize] >= x[i] {
                x[j as usize] += 1;
            }
        }
    }
    CoordWingEdgesLockedEvensUDSlice(x)
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
pub struct CoordWingEdgesLockedEvensUDSliceIndex(u16);

impl Sequence for CoordWingEdgesLockedEvensUDSliceIndex {
    const CARDINALITY: usize = 11_880;

    fn next(&self) -> Option<Self> {
        if self.0 == (Self::CARDINALITY - 1) as u16 {
            None
        } else {
            Some(CoordWingEdgesLockedEvensUDSliceIndex(self.0 + 1))
        }
    }

    fn previous(&self) -> Option<Self> {
        if self.0 == 0 {
            None
        } else {
            Some(CoordWingEdgesLockedEvensUDSliceIndex(self.0 - 1))
        }
    }

    fn first() -> Option<Self> {
        Some(CoordWingEdgesLockedEvensUDSliceIndex(0))
    }

    fn last() -> Option<Self> {
        Some(CoordWingEdgesLockedEvensUDSliceIndex((Self::CARDINALITY - 1) as u16))
    }
}

impl Into<CoordWingEdgesLockedEvensUDSlice> for CoordWingEdgesLockedEvensUDSliceIndex {
    fn into(self) -> CoordWingEdgesLockedEvensUDSlice {
        from_lehmer(self.0)
    }
}

impl Into<usize> for CoordWingEdgesLockedEvensUDSliceIndex {
    fn into(self) -> usize {
        self.0 as usize
    }
}

impl TryFrom<usize> for CoordWingEdgesLockedEvensUDSliceIndex {
    type Error = std::num::TryFromIntError;

    fn try_from(i: usize) -> Result<Self, Self::Error> {
        let j = i.try_into()?;
        Ok(CoordWingEdgesLockedEvensUDSliceIndex(j))
    }
}

impl Into<CoordWingEdgesLockedEvensUDSliceIndex> for CoordWingEdgesLockedEvensUDSlice {
    fn into(self) -> CoordWingEdgesLockedEvensUDSliceIndex {
        CoordWingEdgesLockedEvensUDSliceIndex(to_lehmer(self))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck::Gen;
    use rand::Rng;
    use enum_iterator::cardinality;

    impl quickcheck::Arbitrary for CoordWingEdgesLockedEvensUDSliceIndex {
        fn arbitrary<G: Gen>(g: &mut G) -> CoordWingEdgesLockedEvensUDSliceIndex {
            CoordWingEdgesLockedEvensUDSliceIndex(g.gen_range(0, cardinality::<CoordWingEdgesLockedEvensUDSliceIndex>() as u16))
        }
    }

    quickcheck! {
        fn indexing_round_trips(pi: CoordWingEdgesLockedEvensUDSliceIndex) -> bool {
            let p: CoordWingEdgesLockedEvensUDSlice = pi.into();
            pi == p.into()
        }
    }

    quickcheck! {
        fn group_action_obeys_identity_law(p: CoordWingEdgesLockedEvens) -> bool {
            p == p.act(CoordWingEdgesLockedEvens::identity())
        }
    }

    quickcheck! {
        fn group_action_obeys_compatibility_law(xi: CoordWingEdgesLockedEvensUDSliceIndex, g_0: CoordWingEdgesLockedEvens, g_1: CoordWingEdgesLockedEvens) -> bool {
            let x: CoordWingEdgesLockedEvensUDSlice = xi.into();
            let merged = g_0.clone().permute(g_1.clone());
            let act_together = x.act(merged);
            let act_separate = x.act(g_0.clone()).act(g_1.clone());
            act_together == act_separate
        }
    }

    quickcheck! {
        fn h1_turns_and_sym_invert_round_trips(pi: CoordWingEdgesLockedEvensUDSliceIndex, t: H1WideTurn, s: U2F2Symmetry) -> bool {
            let p: CoordWingEdgesLockedEvensUDSlice = pi.into();
            let t_p: CoordWingEdgesLockedEvens = t.into();
            let t_p_prime: CoordWingEdgesLockedEvens = t.invert().into();
            let s_p: CoordWingEdgesLockedEvens = s.into();
            let s_p_prime: CoordWingEdgesLockedEvens = s.invert().into();
            p == p.act(t_p).act(t_p_prime)
                && p == p.act(s_p).act(s_p_prime)
        }
    }


    quickcheck! {
        fn perm_and_h1_turn_u2f2_symmetries_are_equivalent(pi: CoordWingEdgesLockedEvensUDSliceIndex, t: H1WideTurn, s: U2F2Symmetry) -> bool {
            let p: CoordWingEdgesLockedEvensUDSlice = pi.into();
            let turn: CoordWingEdgesLockedEvens = t.into();
            let after_action = p.act(turn).get_equivalent(&s);
            let before_action = p.get_equivalent(&s).act(turn.get_equivalent(&s));
            after_action == before_action
        }
    }

    quickcheck! {
        fn g1_turns_and_sym_invert_round_trips(pi: CoordWingEdgesLockedEvensUDSliceIndex, t: G1WideTurn, s: U2F2Symmetry) -> bool {
            let p: CoordWingEdgesLockedEvensUDSlice = pi.into();
            let t_p: CoordWingEdgesLockedEvens = t.into();
            let t_p_prime: CoordWingEdgesLockedEvens = t.invert().into();
            let s_p: CoordWingEdgesLockedEvens = s.into();
            let s_p_prime: CoordWingEdgesLockedEvens = s.invert().into();
            p == p.act(t_p).act(t_p_prime)
                && p == p.act(s_p).act(s_p_prime)
        }
    }


    quickcheck! {
        fn perm_and_g1_turn_u2f2_symmetries_are_equivalent(pi: CoordWingEdgesLockedEvensUDSliceIndex, t: G1WideTurn, s: U2F2Symmetry) -> bool {
            let p: CoordWingEdgesLockedEvensUDSlice = pi.into();
            let turn: CoordWingEdgesLockedEvens = t.into();
            let after_action = p.act(turn).get_equivalent(&s);
            let before_action = p.get_equivalent(&s).act(turn.get_equivalent(&s));
            after_action == before_action
        }
    }

    quickcheck! {
        fn u2f2_symmetry_group_action_obeys_identity_law(pi: CoordWingEdgesLockedEvensUDSliceIndex) -> bool {
            let p: CoordWingEdgesLockedEvensUDSlice = pi.into();
            p == p.act(U2F2Symmetry::identity())
        }
    }

    quickcheck! {
        fn u2f2_symmetry_group_action_obeys_compatibility_law(pi: CoordWingEdgesLockedEvensUDSliceIndex, s0: U2F2Symmetry, s1: U2F2Symmetry) -> bool {
            let p: CoordWingEdgesLockedEvensUDSlice = pi.into();
            let as_tt = p.act(s0).act(s1);
            let as_sym = p.act(s0.permute(s1));
            as_tt == as_sym
        }
    }

    quickcheck! {
        fn u2f2_symmetry_group_left_action_obeys_compatibility_law(pi: CoordWingEdgesLockedEvensUDSliceIndex, s0: U2F2Symmetry, s1: U2F2Symmetry) -> bool {
            let p: CoordWingEdgesLockedEvensUDSlice = pi.into();
            let s01 = s0.permute(s1);
            let as_tt = CoordWingEdgesLockedEvensUDSlice::act_left(s0, CoordWingEdgesLockedEvensUDSlice::act_left(s1, p));
            let as_sym = CoordWingEdgesLockedEvensUDSlice::act_left(s01, p);
            as_tt == as_sym
        }
    }
}
