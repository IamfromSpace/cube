use permutation_group::PermutationGroup as PG;
use invertable::Invertable;
use equivalence_class::EquivalenceClass;
use algebraic_actions::{MagmaAction, MonoidAction, LeftMagmaAction, LeftMonoidAction};
use super::CoordWingEdgesLockedEvens;
use move_sets::g1_wide_turns::G1WideTurn;
use symmetries::cube::U2Symmetry;

use std::convert::{TryInto, TryFrom};
use enum_iterator::Sequence;

// This module is a pattern of CoordWingEdgesLockedEvens, tracing only the
// positions of the top six pieces.  So there are only six entries, despite
// being 12 pieces in 12 locations.  Each entry tracks the current location of
// a single piece, so [0, 1, 2, 3, 4, 5] is when the tracked pieces are solved.
//
// The positions of the untracked positions are completely unknown.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct CoordWingEdgesLockedEvensTopSix([u8; 6]);

impl From<CoordWingEdgesLockedEvens> for CoordWingEdgesLockedEvensTopSix {
    fn from(t: CoordWingEdgesLockedEvens) -> Self {
        CoordWingEdgesLockedEvensTopSix([t.0[0], t.0[1], t.0[2], t.0[3], t.0[4], t.0[5]])
    }
}

fn arr_act(a: [u8; 6], b: [u8; 12]) -> [u8; 6] {
    [
        b[a[0] as usize],
        b[a[1] as usize],
        b[a[2] as usize],
        b[a[3] as usize],
        b[a[4] as usize],
        b[a[5] as usize],
    ]
}

fn arr_act_left(a: [u8; 12], b: [u8; 6]) -> [u8; 6] {
    [
        b[a[0] as usize],
        b[a[1] as usize],
        b[a[2] as usize],
        b[a[3] as usize],
        b[a[4] as usize],
        b[a[5] as usize],
    ]
}

impl MagmaAction<CoordWingEdgesLockedEvens> for CoordWingEdgesLockedEvensTopSix {
    fn act(self, b: CoordWingEdgesLockedEvens) -> Self {
        Self(arr_act(self.0, b.0))
    }
}

impl MonoidAction<CoordWingEdgesLockedEvens> for CoordWingEdgesLockedEvensTopSix {}

impl MagmaAction<U2Symmetry> for CoordWingEdgesLockedEvensTopSix {
    fn act(self, s: U2Symmetry) -> Self {
        let t: CoordWingEdgesLockedEvens = s.into();
        self.act(t)
    }
}

impl MonoidAction<U2Symmetry> for CoordWingEdgesLockedEvensTopSix {}

impl LeftMagmaAction<U2Symmetry> for CoordWingEdgesLockedEvensTopSix {
    fn act_left(s: U2Symmetry, t: CoordWingEdgesLockedEvensTopSix) -> Self {
        let s: CoordWingEdgesLockedEvens = s.into();
        CoordWingEdgesLockedEvensTopSix(arr_act_left(s.0, t.0))
    }
}

impl LeftMonoidAction<U2Symmetry> for CoordWingEdgesLockedEvensTopSix {}

impl EquivalenceClass<U2Symmetry> for CoordWingEdgesLockedEvensTopSix {
    fn get_equivalent(self, sym: &U2Symmetry) -> CoordWingEdgesLockedEvensTopSix {
        CoordWingEdgesLockedEvensTopSix::act_left(sym.invert(), self).act(*sym)
    }
}

// TODO: There are faster algorithms than this
fn to_lehmer(p: CoordWingEdgesLockedEvensTopSix) -> u32 {
    let mut x = p.0.clone();
    for i in 0..6 {
        for j in (i+1)..6 {
            if x[j] > x[i] {
                x[j] -= 1;
            }
        }
    }
    55440 * x[0] as u32 + 5040 * x[1] as u32 + 504 * x[2] as u32 + 56 * x[3] as u32 + 7 * x[4] as u32 + x[5] as u32
}

// TODO: There are faster algorithms than this
fn from_lehmer(i: u32) -> CoordWingEdgesLockedEvensTopSix {
    let mut x =
        [ (i / 55440) as u8
        , ((i / 5040) % 11) as u8
        , ((i / 504) % 10) as u8
        , ((i / 56) % 9) as u8
        , ((i / 7) % 8) as u8
        , (i % 7) as u8
        ];
    for i in (0..6).rev() {
        for j in (i+1)..6 {
            if x[j as usize] >= x[i] {
                x[j as usize] += 1;
            }
        }
    }
    CoordWingEdgesLockedEvensTopSix(x)
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
pub struct CoordWingEdgesLockedEvensTopSixIndex(u32);

impl Sequence for CoordWingEdgesLockedEvensTopSixIndex {
    const CARDINALITY: usize = 665_280;

    fn next(&self) -> Option<Self> {
        if self.0 == (Self::CARDINALITY - 1) as u32 {
            None
        } else {
            Some(CoordWingEdgesLockedEvensTopSixIndex(self.0 + 1))
        }
    }

    fn previous(&self) -> Option<Self> {
        if self.0 == 0 {
            None
        } else {
            Some(CoordWingEdgesLockedEvensTopSixIndex(self.0 - 1))
        }
    }

    fn first() -> Option<Self> {
        Some(CoordWingEdgesLockedEvensTopSixIndex(0))
    }

    fn last() -> Option<Self> {
        Some(CoordWingEdgesLockedEvensTopSixIndex((Self::CARDINALITY - 1) as u32))
    }
}

impl Into<CoordWingEdgesLockedEvensTopSix> for CoordWingEdgesLockedEvensTopSixIndex {
    fn into(self) -> CoordWingEdgesLockedEvensTopSix {
        from_lehmer(self.0)
    }
}

impl Into<usize> for CoordWingEdgesLockedEvensTopSixIndex {
    fn into(self) -> usize {
        self.0 as usize
    }
}

impl TryFrom<usize> for CoordWingEdgesLockedEvensTopSixIndex {
    type Error = std::num::TryFromIntError;

    fn try_from(i: usize) -> Result<Self, Self::Error> {
        let j = i.try_into()?;
        Ok(CoordWingEdgesLockedEvensTopSixIndex(j))
    }
}

impl Into<CoordWingEdgesLockedEvensTopSixIndex> for CoordWingEdgesLockedEvensTopSix {
    fn into(self) -> CoordWingEdgesLockedEvensTopSixIndex {
        CoordWingEdgesLockedEvensTopSixIndex(to_lehmer(self))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck::Gen;
    use rand::Rng;
    use enum_iterator::cardinality;

    impl quickcheck::Arbitrary for CoordWingEdgesLockedEvensTopSixIndex {
        fn arbitrary<G: Gen>(g: &mut G) -> CoordWingEdgesLockedEvensTopSixIndex {
            CoordWingEdgesLockedEvensTopSixIndex(*g.choose(&(0..cardinality::<CoordWingEdgesLockedEvensTopSixIndex>() as u32).collect::<Vec<_>>()).unwrap())
        }
    }

    #[test]
    fn all_odd_lehmer_codes_round_trip() {
        for i in 0..(cardinality::<CoordWingEdgesLockedEvensTopSixIndex>() as u32) {
            let t: CoordWingEdgesLockedEvensTopSix = from_lehmer(i);
            assert_eq!(i, to_lehmer(t));
        }
    }

    quickcheck! {
        fn group_action_obeys_identity_law(p: CoordWingEdgesLockedEvens) -> bool {
            p == p.act(CoordWingEdgesLockedEvens::identity())
        }
    }

    quickcheck! {
        fn three_trapezoids_group_action_obeys_compatibility_law(xi: CoordWingEdgesLockedEvensTopSixIndex, g_0: CoordWingEdgesLockedEvens, g_1: CoordWingEdgesLockedEvens) -> bool {
            let x: CoordWingEdgesLockedEvensTopSix = xi.into();
            let merged = g_0.clone().permute(g_1.clone());
            let act_together = x.act(merged);
            let act_separate = x.act(g_0.clone()).act(g_1.clone());
            act_together == act_separate
        }
    }

    quickcheck! {
        fn turns_and_sym_invert_round_trips(pi: CoordWingEdgesLockedEvensTopSixIndex, t: G1WideTurn, s: U2Symmetry) -> bool {
            let p: CoordWingEdgesLockedEvensTopSix = pi.into();
            let t_p: CoordWingEdgesLockedEvens = t.into();
            let t_p_prime: CoordWingEdgesLockedEvens = t.invert().into();
            let s_p: CoordWingEdgesLockedEvens = s.into();
            let s_p_prime: CoordWingEdgesLockedEvens = s.invert().into();
            p == p.act(t_p).act(t_p_prime)
                && p == p.act(s_p).act(s_p_prime)
        }
    }


    quickcheck! {
        fn perm_and_turn_u2_symmetries_are_equivalent(pi: CoordWingEdgesLockedEvensTopSixIndex, t: G1WideTurn, s: U2Symmetry) -> bool {
            let p: CoordWingEdgesLockedEvensTopSix = pi.into();
            let turn: CoordWingEdgesLockedEvens = t.into();
            let after_action = p.act(turn).get_equivalent(&s);
            let before_action = p.get_equivalent(&s).act(turn.get_equivalent(&s));
            after_action == before_action
        }
    }

    quickcheck! {
        fn u2_symmetry_group_action_obeys_identity_law(pi: CoordWingEdgesLockedEvensTopSixIndex) -> bool {
            let p: CoordWingEdgesLockedEvensTopSix = pi.into();
            p == p.act(U2Symmetry::identity())
        }
    }

    quickcheck! {
        fn u2_symmetry_group_action_obeys_compatibility_law(pi: CoordWingEdgesLockedEvensTopSixIndex, s0: U2Symmetry, s1: U2Symmetry) -> bool {
            let p: CoordWingEdgesLockedEvensTopSix = pi.into();
            let as_tt = p.act(s0).act(s1);
            let as_sym = p.act(s0.permute(s1));
            as_tt == as_sym
        }
    }

    quickcheck! {
        fn u2_symmetry_group_left_action_obeys_compatibility_law(pi: CoordWingEdgesLockedEvensTopSixIndex, s0: U2Symmetry, s1: U2Symmetry) -> bool {
            let p: CoordWingEdgesLockedEvensTopSix = pi.into();
            let s01 = s0.permute(s1);
            let as_tt = CoordWingEdgesLockedEvensTopSix::act_left(s0, CoordWingEdgesLockedEvensTopSix::act_left(s1, p));
            let as_sym = CoordWingEdgesLockedEvensTopSix::act_left(s01, p);
            as_tt == as_sym
        }
    }
}
