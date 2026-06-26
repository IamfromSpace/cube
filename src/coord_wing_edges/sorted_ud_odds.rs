use invertable::Invertable;
use equivalence_class::EquivalenceClass;
use algebraic_actions::{MagmaAction, MonoidAction, LeftMagmaAction, LeftMonoidAction};
use symmetries::cube::UF2Symmetry;
use coord_wing_edges::CoordWingEdges;

use std::convert::{TryInto, TryFrom};
use enum_iterator::Sequence;

// This module is a sorted pattern of CoordWingEdges, tracing only the
// distribution of the odd pieces on the up and down faces, but ignoring which
// exact piece is in which position.  So there are only eight entries, despite
// being 24 pieces in 24 locations, and we always sort the result, to throw
// away the exact placement.  We know where the eight pieces are, but we don't
// know _exactly_ which one is where.  So [0, 1, 2, 3, 4, 5, 6, 7] is when the
// tracked pieces are only scrambled amongst each other's positions (or maybe
// fully solved).
//
// The positions of the untracked positions are completely unknown.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct CoordWingEdgesSortedUDOdds([u8; 8]);

impl From<CoordWingEdges> for CoordWingEdgesSortedUDOdds {
    fn from(t: CoordWingEdges) -> Self {
        CoordWingEdgesSortedUDOdds([1, 3, 5, 7, 17, 19, 21, 23]).act(t)
    }
}

fn arr_act(a: [u8; 8], b: [u8; 24]) -> [u8; 8] {
    let mut arr = [
        b[a[0] as usize],
        b[a[1] as usize],
        b[a[2] as usize],
        b[a[3] as usize],
        b[a[4] as usize],
        b[a[5] as usize],
        b[a[6] as usize],
        b[a[7] as usize],
    ];
    // NOTE: If we needed to track parity, we would count the swaps required to sort here!
    arr.sort_unstable();
    arr
}

fn adjust(i: usize) -> usize {
    match i {
        1 => 0,
        3 => 1,
        5 => 2,
        7 => 3,
        17 => 4,
        19 => 5,
        21 => 6,
        23 => 7,
        _ => unreachable!("Tried to act left on a permution that does swaps between this orbit and other pieces.  Typically this means that you've tried to implement an invalid symmetry.  We can only go backwards through a permutation where the pieces we track interact only with themselves.  Otherwise, we don't know how to apply it, because we don't know where some of the pieces involved actually are."),
    }
}

fn arr_act_left(a: [u8; 24], b: [u8; 8]) -> [u8; 8] {
    let mut arr = [
        b[adjust(a[1] as usize)],
        b[adjust(a[3] as usize)],
        b[adjust(a[5] as usize)],
        b[adjust(a[7] as usize)],
        b[adjust(a[17] as usize)],
        b[adjust(a[19] as usize)],
        b[adjust(a[21] as usize)],
        b[adjust(a[23] as usize)],
    ];
    arr.sort_unstable();
    arr
}

impl MagmaAction<CoordWingEdges> for CoordWingEdgesSortedUDOdds {
    fn act(self, b: CoordWingEdges) -> Self {
        Self(arr_act(self.0, b.0))
    }
}

impl MonoidAction<CoordWingEdges> for CoordWingEdgesSortedUDOdds {}

impl MagmaAction<UF2Symmetry> for CoordWingEdgesSortedUDOdds {
    fn act(self, s: UF2Symmetry) -> Self {
        let t: CoordWingEdges = s.into();
        self.act(t)
    }
}

impl MonoidAction<UF2Symmetry> for CoordWingEdgesSortedUDOdds {}

impl LeftMagmaAction<UF2Symmetry> for CoordWingEdgesSortedUDOdds {
    fn act_left(s: UF2Symmetry, t: CoordWingEdgesSortedUDOdds) -> Self {
        let s: CoordWingEdges = s.into();
        CoordWingEdgesSortedUDOdds(arr_act_left(s.0, t.0))
    }
}

impl LeftMonoidAction<UF2Symmetry> for CoordWingEdgesSortedUDOdds {}

impl EquivalenceClass<UF2Symmetry> for CoordWingEdgesSortedUDOdds {
    fn get_equivalent(self, sym: &UF2Symmetry) -> CoordWingEdgesSortedUDOdds {
        CoordWingEdgesSortedUDOdds::act_left(sym.invert(), self).act(*sym)
    }
}

fn c(n: i128, k: i128) -> i128 {
    let numerator: i128 = ((n - k + 1)..=n).product();
    let denominator: i128 = (1..=k).product();
    numerator / denominator
}

fn to_combinadic(p: CoordWingEdgesSortedUDOdds) -> u32 {
    let mut x = 0;
    for i in 0..8 {
        x += c(p.0[i] as i128, i as i128 + 1) as u32;
    }
    x
}

fn from_combinadic(i: u32) -> CoordWingEdgesSortedUDOdds {
    let mut x = [0; 8];
    let mut remainder = i;
    for i in (0..8).rev() {
        for j in i..(24 + 1) {
            if c(j, i+1) as u32 > remainder {
                x[i as usize] = (j-1) as u8;
                remainder -= c(j-1, i+1) as u32; // TODO: silly to calculate again
                break;
            }
        }
    }
    CoordWingEdgesSortedUDOdds(x)
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
pub struct CoordWingEdgesSortedUDOddsIndex(u32);

impl Sequence for CoordWingEdgesSortedUDOddsIndex {
    const CARDINALITY: usize = 735_471;

    fn next(&self) -> Option<Self> {
        if self.0 == (Self::CARDINALITY - 1) as u32 {
            None
        } else {
            Some(CoordWingEdgesSortedUDOddsIndex(self.0 + 1))
        }
    }

    fn previous(&self) -> Option<Self> {
        if self.0 == 0 {
            None
        } else {
            Some(CoordWingEdgesSortedUDOddsIndex(self.0 - 1))
        }
    }

    fn first() -> Option<Self> {
        Some(CoordWingEdgesSortedUDOddsIndex(0))
    }

    fn last() -> Option<Self> {
        Some(CoordWingEdgesSortedUDOddsIndex((Self::CARDINALITY - 1) as u32))
    }
}

impl Into<CoordWingEdgesSortedUDOdds> for CoordWingEdgesSortedUDOddsIndex {
    fn into(self) -> CoordWingEdgesSortedUDOdds {
        from_combinadic(self.0)
    }
}

impl Into<usize> for CoordWingEdgesSortedUDOddsIndex {
    fn into(self) -> usize {
        self.0 as usize
    }
}

impl TryFrom<usize> for CoordWingEdgesSortedUDOddsIndex {
    type Error = std::num::TryFromIntError;

    fn try_from(i: usize) -> Result<Self, Self::Error> {
        let j = i.try_into()?;
        Ok(CoordWingEdgesSortedUDOddsIndex(j))
    }
}

impl Into<CoordWingEdgesSortedUDOddsIndex> for CoordWingEdgesSortedUDOdds {
    fn into(self) -> CoordWingEdgesSortedUDOddsIndex {
        CoordWingEdgesSortedUDOddsIndex(to_combinadic(self))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use permutation_group::PermutationGroup as PG;
    use move_sets::wide_turns::WideTurn;
    use quickcheck::Gen;
    use enum_iterator::cardinality;
    use rand::prelude::IteratorRandom;

    impl quickcheck::Arbitrary for CoordWingEdgesSortedUDOddsIndex {
        fn arbitrary<G: Gen>(g: &mut G) -> CoordWingEdgesSortedUDOddsIndex {
            // NOTE: all is extremely slow
            CoordWingEdgesSortedUDOddsIndex((0..cardinality::<CoordWingEdgesSortedUDOddsIndex>() as u32).choose(g).unwrap())
        }
    }

    // 17s to exhaustively check, so we use quickcheck instead
    quickcheck! {
        fn all_combinadic(i: CoordWingEdgesSortedUDOddsIndex) -> bool {
            i.0 == to_combinadic(from_combinadic(i.0))
        }
    }

    quickcheck! {
        fn group_action_obeys_identity_law(p: CoordWingEdges) -> bool {
            p == p.act(CoordWingEdges::identity())
        }
    }

    quickcheck! {
        fn group_action_obeys_compatibility_law(xi: CoordWingEdgesSortedUDOddsIndex, g_0: CoordWingEdges, g_1: CoordWingEdges) -> bool {
            let x: CoordWingEdgesSortedUDOdds = xi.into();
            let merged = g_0.clone().permute(g_1.clone());
            let act_together = x.act(merged);
            let act_separate = x.act(g_0.clone()).act(g_1.clone());
            act_together == act_separate
        }
    }

    quickcheck! {
        fn turns_and_sym_invert_round_trips(pi: CoordWingEdgesSortedUDOddsIndex, t: WideTurn, s: UF2Symmetry) -> bool {
            let p: CoordWingEdgesSortedUDOdds = pi.into();
            let t_p: CoordWingEdges = t.into();
            let t_p_prime: CoordWingEdges = t.invert().into();
            let s_p: CoordWingEdges = s.into();
            let s_p_prime: CoordWingEdges = s.invert().into();
            p == p.act(t_p).act(t_p_prime)
                && p == p.act(s_p).act(s_p_prime)
        }
    }


    quickcheck! {
        fn perm_and_turn_uf2_symmetries_are_equivalent(pi: CoordWingEdgesSortedUDOddsIndex, t: WideTurn, s: UF2Symmetry) -> bool {
            let p: CoordWingEdgesSortedUDOdds = pi.into();
            let turn: CoordWingEdges = t.into();
            let after_action = p.act(turn).get_equivalent(&s);
            let before_action = p.get_equivalent(&s).act(turn.get_equivalent(&s));
            after_action == before_action
        }
    }

    quickcheck! {
        fn uf2_symmetry_group_action_obeys_identity_law(pi: CoordWingEdgesSortedUDOddsIndex) -> bool {
            let p: CoordWingEdgesSortedUDOdds = pi.into();
            p == p.act(UF2Symmetry::identity())
        }
    }

    quickcheck! {
        fn uf2_symmetry_group_action_obeys_compatibility_law(pi: CoordWingEdgesSortedUDOddsIndex, s0: UF2Symmetry, s1: UF2Symmetry) -> bool {
            let p: CoordWingEdgesSortedUDOdds = pi.into();
            let as_tt = p.act(s0).act(s1);
            let as_sym = p.act(s0.permute(s1));
            as_tt == as_sym
        }
    }

    quickcheck! {
        fn uf2_symmetry_group_left_action_obeys_compatibility_law(pi: CoordWingEdgesSortedUDOddsIndex, s0: UF2Symmetry, s1: UF2Symmetry) -> bool {
            let p: CoordWingEdgesSortedUDOdds = pi.into();
            let s01 = s0.permute(s1);
            let as_tt = CoordWingEdgesSortedUDOdds::act_left(s0, CoordWingEdgesSortedUDOdds::act_left(s1, p));
            let as_sym = CoordWingEdgesSortedUDOdds::act_left(s01, p);
            as_tt == as_sym
        }
    }
}
