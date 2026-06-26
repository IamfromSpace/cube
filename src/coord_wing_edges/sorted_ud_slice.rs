use invertable::Invertable;
use equivalence_class::EquivalenceClass;
use algebraic_actions::{MagmaAction, MonoidAction, LeftMagmaAction, LeftMonoidAction};
use swap_parity::sort_and_count_swaps;
use symmetries::cube::UF2Symmetry;
use coord_wing_edges::CoordWingEdges;

use std::convert::{TryInto, TryFrom};
use enum_iterator::Sequence;

// This module is a sorted pattern of CoordWingEdges, tracing only the
// distribution of the UD slice pieces between the up and down faces, but
// ignoring which exact piece is in which position.  So there are only eight
// entries, despite being 24 pieces in 24 locations, and we always sort the
// result, to throw away the exact placement.  We know where the eight pieces
// are, but we don't know _exactly_ which one is where.  So [8, 9, 10, 11, 12,
// 13, 14, 15] is when the tracked pieces are only scrambled amongst each
// other's positions (or maybe fully solved).
//
// In addition, we track the parity of the _other_ pieces, which we want to be
// even.  This helps reduce to the G1a position, because we need the UD evens
// and UD odds to have matched parity.  This orbit can be odd, but those two
// orbits together must be even.  We can get the parity of the other pieces by
// considering the current parity, the parity of the incoming move, and the
// parity of the resulting sort.
//
// The positions of the untracked positions are completely unknown.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct CoordWingEdgesSortedUDSlice([u8; 8], bool);

impl From<CoordWingEdges> for CoordWingEdgesSortedUDSlice {
    fn from(t: CoordWingEdges) -> Self {
        CoordWingEdgesSortedUDSlice([8, 9, 10, 11, 12, 13, 14, 15], true).act(t)
    }
}

fn arr_act(a: [u8; 8], b: [u8; 24]) -> ([u8; 8], bool) {
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
    let swaps = sort_and_count_swaps(&mut arr);
    (arr, swaps % 2 == 0)
}

fn adjust(i: usize) -> usize {
    i - 8
}

fn arr_act_left(a: [u8; 24], b: [u8; 8]) -> ([u8; 8], bool) {
    let mut arr = [
        b[adjust(a[8] as usize)],
        b[adjust(a[9] as usize)],
        b[adjust(a[10] as usize)],
        b[adjust(a[11] as usize)],
        b[adjust(a[12] as usize)],
        b[adjust(a[13] as usize)],
        b[adjust(a[14] as usize)],
        b[adjust(a[15] as usize)],
    ];
    let swaps = sort_and_count_swaps(&mut arr);
    (arr, swaps % 2 == 0)
}

impl MagmaAction<CoordWingEdges> for CoordWingEdgesSortedUDSlice {
    fn act(self, b: CoordWingEdges) -> Self {
        let (arr, sort_swaps_even) = arr_act(self.0, b.0);
        let global_perm_even = b.is_even_parity();
        Self(arr, self.1 == (sort_swaps_even == global_perm_even))
    }
}

impl MonoidAction<CoordWingEdges> for CoordWingEdgesSortedUDSlice {}

impl MagmaAction<UF2Symmetry> for CoordWingEdgesSortedUDSlice {
    fn act(self, s: UF2Symmetry) -> Self {
        let t: CoordWingEdges = s.into();
        self.act(t)
    }
}

impl MonoidAction<UF2Symmetry> for CoordWingEdgesSortedUDSlice {}

impl LeftMagmaAction<UF2Symmetry> for CoordWingEdgesSortedUDSlice {
    fn act_left(s: UF2Symmetry, t: CoordWingEdgesSortedUDSlice) -> Self {
        let s: CoordWingEdges = s.into();
        let (arr, sort_swaps_even) = arr_act_left(s.0, t.0);
        let global_perm_even = s.is_even_parity();
        Self(arr, t.1 == (sort_swaps_even == global_perm_even))
    }
}

impl LeftMonoidAction<UF2Symmetry> for CoordWingEdgesSortedUDSlice {}

impl EquivalenceClass<UF2Symmetry> for CoordWingEdgesSortedUDSlice {
    fn get_equivalent(self, sym: &UF2Symmetry) -> CoordWingEdgesSortedUDSlice {
        CoordWingEdgesSortedUDSlice::act_left(sym.invert(), self).act(*sym)
    }
}


fn c(n: i128, k: i128) -> i128 {
    let numerator: i128 = ((n - k + 1)..=n).product();
    let denominator: i128 = (1..=k).product();
    numerator / denominator
}

fn to_combinadic(p: [u8; 8]) -> u32 {
    let mut x = 0;
    for i in 0..8 {
        x += c(p[i] as i128, i as i128 + 1) as u32;
    }
    x
}

fn from_combinadic(i: u32) -> [u8; 8] {
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
    x
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
pub struct CoordWingEdgesSortedUDSliceIndex(u32);

impl Sequence for CoordWingEdgesSortedUDSliceIndex {
    const CARDINALITY: usize = 1_470_942;

    fn next(&self) -> Option<Self> {
        if self.0 == (Self::CARDINALITY - 1) as u32 {
            None
        } else {
            Some(CoordWingEdgesSortedUDSliceIndex(self.0 + 1))
        }
    }

    fn previous(&self) -> Option<Self> {
        if self.0 == 0 {
            None
        } else {
            Some(CoordWingEdgesSortedUDSliceIndex(self.0 - 1))
        }
    }

    fn first() -> Option<Self> {
        Some(CoordWingEdgesSortedUDSliceIndex(0))
    }

    fn last() -> Option<Self> {
        Some(CoordWingEdgesSortedUDSliceIndex((Self::CARDINALITY - 1) as u32))
    }
}

impl Into<CoordWingEdgesSortedUDSlice> for CoordWingEdgesSortedUDSliceIndex {
    fn into(self) -> CoordWingEdgesSortedUDSlice {
        CoordWingEdgesSortedUDSlice(from_combinadic(self.0 / 2), self.0 % 2 == 0)
    }
}

impl Into<usize> for CoordWingEdgesSortedUDSliceIndex {
    fn into(self) -> usize {
        self.0 as usize
    }
}

impl TryFrom<usize> for CoordWingEdgesSortedUDSliceIndex {
    type Error = std::num::TryFromIntError;

    fn try_from(i: usize) -> Result<Self, Self::Error> {
        let j = i.try_into()?;
        Ok(CoordWingEdgesSortedUDSliceIndex(j))
    }
}

impl Into<CoordWingEdgesSortedUDSliceIndex> for CoordWingEdgesSortedUDSlice {
    fn into(self) -> CoordWingEdgesSortedUDSliceIndex {
        CoordWingEdgesSortedUDSliceIndex(to_combinadic(self.0) * 2 + if self.1 { 0 } else { 1 })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use permutation_group::PermutationGroup as PG;
    use move_sets::wide_turns::WideTurn;
    use quickcheck::Gen;
    use rand::prelude::IteratorRandom;
    use enum_iterator::cardinality;

    impl quickcheck::Arbitrary for CoordWingEdgesSortedUDSliceIndex {
        fn arbitrary<G: Gen>(g: &mut G) -> CoordWingEdgesSortedUDSliceIndex {
            // NOTE: all is extremely slow
            CoordWingEdgesSortedUDSliceIndex((0..cardinality::<CoordWingEdgesSortedUDSliceIndex>() as u32).choose(g).unwrap())
        }
    }

    // Too long to exhaustively check, so we use quickcheck instead
    quickcheck! {
        fn indexing_round_trips(i: CoordWingEdgesSortedUDSliceIndex) -> bool {
            i == Into::<CoordWingEdgesSortedUDSlice>::into(i).into()
        }
    }

    quickcheck! {
        fn group_action_obeys_identity_law(p: CoordWingEdges) -> bool {
            p == p.act(CoordWingEdges::identity())
        }
    }

    quickcheck! {
        fn group_action_obeys_compatibility_law(xi: CoordWingEdgesSortedUDSliceIndex, g_0: CoordWingEdges, g_1: CoordWingEdges) -> bool {
            let x: CoordWingEdgesSortedUDSlice = xi.into();
            let merged = g_0.clone().permute(g_1.clone());
            let act_together = x.act(merged);
            let act_separate = x.act(g_0.clone()).act(g_1.clone());
            act_together == act_separate
        }
    }

    quickcheck! {
        fn turns_and_sym_invert_round_trips(pi: CoordWingEdgesSortedUDSliceIndex, t: WideTurn, s: UF2Symmetry) -> bool {
            let p: CoordWingEdgesSortedUDSlice = pi.into();
            let t_p: CoordWingEdges = t.into();
            let t_p_prime: CoordWingEdges = t.invert().into();
            let s_p: CoordWingEdges = s.into();
            let s_p_prime: CoordWingEdges = s.invert().into();
            p == p.act(t_p).act(t_p_prime)
                && p == p.act(s_p).act(s_p_prime)
        }
    }


    quickcheck! {
        fn perm_and_turn_uf2_symmetries_are_equivalent(pi: CoordWingEdgesSortedUDSliceIndex, t: WideTurn, s: UF2Symmetry) -> bool {
            let p: CoordWingEdgesSortedUDSlice = pi.into();
            let turn: CoordWingEdges = t.into();
            let after_action = p.act(turn).get_equivalent(&s);
            let before_action = p.get_equivalent(&s).act(turn.get_equivalent(&s));
            after_action == before_action
        }
    }

    quickcheck! {
        fn uf2_symmetry_group_action_obeys_identity_law(pi: CoordWingEdgesSortedUDSliceIndex) -> bool {
            let p: CoordWingEdgesSortedUDSlice = pi.into();
            p == p.act(UF2Symmetry::identity())
        }
    }

    quickcheck! {
        fn uf2_symmetry_group_action_obeys_compatibility_law(pi: CoordWingEdgesSortedUDSliceIndex, s0: UF2Symmetry, s1: UF2Symmetry) -> bool {
            let p: CoordWingEdgesSortedUDSlice = pi.into();
            let as_tt = p.act(s0).act(s1);
            let as_sym = p.act(s0.permute(s1));
            as_tt == as_sym
        }
    }

    quickcheck! {
        fn uf2_symmetry_group_left_action_obeys_compatibility_law(pi: CoordWingEdgesSortedUDSliceIndex, s0: UF2Symmetry, s1: UF2Symmetry) -> bool {
            let p: CoordWingEdgesSortedUDSlice = pi.into();
            let s01 = s0.permute(s1);
            let as_tt = CoordWingEdgesSortedUDSlice::act_left(s0, CoordWingEdgesSortedUDSlice::act_left(s1, p));
            let as_sym = CoordWingEdgesSortedUDSlice::act_left(s01, p);
            as_tt == as_sym
        }
    }
}
