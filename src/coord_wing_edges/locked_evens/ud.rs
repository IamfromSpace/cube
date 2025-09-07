use permutation_group::PermutationGroup as PG;
use invertable::Invertable;
use equivalence_class::EquivalenceClass;
use algebraic_actions::{MagmaAction, MonoidAction, LeftMagmaAction, LeftMonoidAction};
use super::CoordWingEdgesLockedEvens;
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
pub struct CoordWingEdgesLockedEvensUD([u8; 8]);

impl From<CoordWingEdgesLockedEvens> for CoordWingEdgesLockedEvensUD {
    fn from(t: CoordWingEdgesLockedEvens) -> Self {
        CoordWingEdgesLockedEvensUD([t.0[0], t.0[1], t.0[2], t.0[3], t.0[8], t.0[9], t.0[10], t.0[11]])
    }
}

fn arr_act(a: [u8; 8], b: [u8; 12]) -> [u8; 8] {
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

fn adjust(x: u8) -> u8 {
    if x >= 4 { x - 4 } else { x }
}

fn arr_act_left(a: [u8; 12], b: [u8; 8]) -> [u8; 8] {
    [
        b[adjust(a[0]) as usize],
        b[adjust(a[1]) as usize],
        b[adjust(a[2]) as usize],
        b[adjust(a[3]) as usize],
        b[adjust(a[8]) as usize],
        b[adjust(a[9]) as usize],
        b[adjust(a[10]) as usize],
        b[adjust(a[11]) as usize],
    ]
}

impl MagmaAction<CoordWingEdgesLockedEvens> for CoordWingEdgesLockedEvensUD {
    fn act(self, b: CoordWingEdgesLockedEvens) -> Self {
        Self(arr_act(self.0, b.0))
    }
}

impl MonoidAction<CoordWingEdgesLockedEvens> for CoordWingEdgesLockedEvensUD {}

impl MagmaAction<U2F2Symmetry> for CoordWingEdgesLockedEvensUD {
    fn act(self, s: U2F2Symmetry) -> Self {
        let t: CoordWingEdgesLockedEvens = s.into();
        self.act(t)
    }
}

impl MonoidAction<U2F2Symmetry> for CoordWingEdgesLockedEvensUD {}

impl LeftMagmaAction<U2F2Symmetry> for CoordWingEdgesLockedEvensUD {
    fn act_left(s: U2F2Symmetry, t: CoordWingEdgesLockedEvensUD) -> Self {
        let s: CoordWingEdgesLockedEvens = s.into();
        CoordWingEdgesLockedEvensUD(arr_act_left(s.0, t.0))
    }
}

impl LeftMonoidAction<U2F2Symmetry> for CoordWingEdgesLockedEvensUD {}

impl EquivalenceClass<U2F2Symmetry> for CoordWingEdgesLockedEvensUD {
    fn get_equivalent(self, sym: &U2F2Symmetry) -> CoordWingEdgesLockedEvensUD {
        CoordWingEdgesLockedEvensUD::act_left(sym.invert(), self).act(*sym)
    }
}

// TODO: There are faster algorithms than this
fn to_lehmer(p: CoordWingEdgesLockedEvensUD) -> u32 {
    let mut x = p.0.clone();
    for i in 0..8 {
        for j in (i+1)..8 {
            if x[j] > x[i] {
                x[j] -= 1;
            }
        }
    }
    1_663_200 * x[0] as u32 + 151_200 * x[1] as u32 + 15_120 * x[2] as u32 + 1_680 * x[3] as u32 + 210 * x[4] as u32 + 30 * x[5] as u32 + 5 * x[6] as u32 + x[7] as u32
}

// TODO: There are faster algorithms than this
fn from_lehmer(i: u32) -> CoordWingEdgesLockedEvensUD {
    let mut x =
        [ (i / 1_663_200) as u8
        , ((i / 151_200) % 11) as u8
        , ((i / 15_120) % 10) as u8
        , ((i / 1_680) % 9) as u8
        , ((i / 210) % 8) as u8
        , ((i / 30) % 7) as u8
        , ((i / 5) % 6) as u8
        , (i % 5) as u8
        ];
    for i in (0..8).rev() {
        for j in (i+1)..8 {
            if x[j as usize] >= x[i] {
                x[j as usize] += 1;
            }
        }
    }
    CoordWingEdgesLockedEvensUD(x)
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
pub struct CoordWingEdgesLockedEvensUDIndex(u32);

impl Sequence for CoordWingEdgesLockedEvensUDIndex {
    const CARDINALITY: usize = 19_958_400;

    fn next(&self) -> Option<Self> {
        if self.0 == (Self::CARDINALITY - 1) as u32 {
            None
        } else {
            Some(CoordWingEdgesLockedEvensUDIndex(self.0 + 1))
        }
    }

    fn previous(&self) -> Option<Self> {
        if self.0 == 0 {
            None
        } else {
            Some(CoordWingEdgesLockedEvensUDIndex(self.0 - 1))
        }
    }

    fn first() -> Option<Self> {
        Some(CoordWingEdgesLockedEvensUDIndex(0))
    }

    fn last() -> Option<Self> {
        Some(CoordWingEdgesLockedEvensUDIndex((Self::CARDINALITY - 1) as u32))
    }
}

impl Into<CoordWingEdgesLockedEvensUD> for CoordWingEdgesLockedEvensUDIndex {
    fn into(self) -> CoordWingEdgesLockedEvensUD {
        from_lehmer(self.0)
    }
}

impl Into<usize> for CoordWingEdgesLockedEvensUDIndex {
    fn into(self) -> usize {
        self.0 as usize
    }
}

impl TryFrom<usize> for CoordWingEdgesLockedEvensUDIndex {
    type Error = std::num::TryFromIntError;

    fn try_from(i: usize) -> Result<Self, Self::Error> {
        let j = i.try_into()?;
        Ok(CoordWingEdgesLockedEvensUDIndex(j))
    }
}

impl Into<CoordWingEdgesLockedEvensUDIndex> for CoordWingEdgesLockedEvensUD {
    fn into(self) -> CoordWingEdgesLockedEvensUDIndex {
        CoordWingEdgesLockedEvensUDIndex(to_lehmer(self))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck::Gen;
    use rand::Rng;
    use enum_iterator::cardinality;

    impl quickcheck::Arbitrary for CoordWingEdgesLockedEvensUDIndex {
        fn arbitrary<G: Gen>(g: &mut G) -> CoordWingEdgesLockedEvensUDIndex {
            CoordWingEdgesLockedEvensUDIndex(g.gen_range(0, cardinality::<CoordWingEdgesLockedEvensUDIndex>() as u32))
        }
    }

    quickcheck! {
        fn indexing_round_trips(pi: CoordWingEdgesLockedEvensUDIndex) -> bool {
            let p: CoordWingEdgesLockedEvensUD = pi.into();
            pi == p.into()
        }
    }

    quickcheck! {
        fn group_action_obeys_identity_law(p: CoordWingEdgesLockedEvens) -> bool {
            p == p.act(CoordWingEdgesLockedEvens::identity())
        }
    }

    quickcheck! {
        fn group_action_obeys_compatibility_law(xi: CoordWingEdgesLockedEvensUDIndex, g_0: CoordWingEdgesLockedEvens, g_1: CoordWingEdgesLockedEvens) -> bool {
            let x: CoordWingEdgesLockedEvensUD = xi.into();
            let merged = g_0.clone().permute(g_1.clone());
            let act_together = x.act(merged);
            let act_separate = x.act(g_0.clone()).act(g_1.clone());
            act_together == act_separate
        }
    }

    quickcheck! {
        fn turns_and_sym_invert_round_trips(pi: CoordWingEdgesLockedEvensUDIndex, t: G1WideTurn, s: U2F2Symmetry) -> bool {
            let p: CoordWingEdgesLockedEvensUD = pi.into();
            let t_p: CoordWingEdgesLockedEvens = t.into();
            let t_p_prime: CoordWingEdgesLockedEvens = t.invert().into();
            let s_p: CoordWingEdgesLockedEvens = s.into();
            let s_p_prime: CoordWingEdgesLockedEvens = s.invert().into();
            p == p.act(t_p).act(t_p_prime)
                && p == p.act(s_p).act(s_p_prime)
        }
    }


    quickcheck! {
        fn perm_and_turn_u2f2_symmetries_are_equivalent(pi: CoordWingEdgesLockedEvensUDIndex, t: G1WideTurn, s: U2F2Symmetry) -> bool {
            let p: CoordWingEdgesLockedEvensUD = pi.into();
            let turn: CoordWingEdgesLockedEvens = t.into();
            let after_action = p.act(turn).get_equivalent(&s);
            let before_action = p.get_equivalent(&s).act(turn.get_equivalent(&s));
            after_action == before_action
        }
    }

    quickcheck! {
        fn u2f2_symmetry_group_action_obeys_identity_law(pi: CoordWingEdgesLockedEvensUDIndex) -> bool {
            let p: CoordWingEdgesLockedEvensUD = pi.into();
            p == p.act(U2F2Symmetry::identity())
        }
    }

    quickcheck! {
        fn u2f2_symmetry_group_action_obeys_compatibility_law(pi: CoordWingEdgesLockedEvensUDIndex, s0: U2F2Symmetry, s1: U2F2Symmetry) -> bool {
            let p: CoordWingEdgesLockedEvensUD = pi.into();
            let as_tt = p.act(s0).act(s1);
            let as_sym = p.act(s0.permute(s1));
            as_tt == as_sym
        }
    }

    quickcheck! {
        fn u2f2_symmetry_group_left_action_obeys_compatibility_law(pi: CoordWingEdgesLockedEvensUDIndex, s0: U2F2Symmetry, s1: U2F2Symmetry) -> bool {
            let p: CoordWingEdgesLockedEvensUD = pi.into();
            let s01 = s0.permute(s1);
            let as_tt = CoordWingEdgesLockedEvensUD::act_left(s0, CoordWingEdgesLockedEvensUD::act_left(s1, p));
            let as_sym = CoordWingEdgesLockedEvensUD::act_left(s01, p);
            as_tt == as_sym
        }
    }
}
