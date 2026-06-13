use invertable::Invertable;
use equivalence_class::EquivalenceClass;
use algebraic_actions::{MagmaAction, MonoidAction, LeftMagmaAction, LeftMonoidAction};
use symmetries::cube::UF2Symmetry;
use coord_wing_edges::CoordWingEdges;

use std::convert::{TryInto, TryFrom};
use enum_iterator::Sequence;

// This module is a sorted pattern of CoordWingEdges, tracing only the
// distribution of the even pieces on the up and down faces, but ignoring which
// exact piece is in which position.  So there are only eight entries, despite
// being 24 pieces in 24 locations, and we always sort the result, to throw
// away the exact placement.  We know where the eight pieces are, but we don't
// know _exactly_ which one is where.  So [0, 1, 2, 3, 4, 5, 6, 7] is when the
// tracked pieces are only scrambled amongst each other's positions (or maybe
// fully solved).
//
// The positions of the untracked positions are completely unknown.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct CoordWingEdgesSortedUDEvens([u8; 8]);

impl From<CoordWingEdges> for CoordWingEdgesSortedUDEvens {
    fn from(t: CoordWingEdges) -> Self {
        CoordWingEdgesSortedUDEvens([0, 2, 4, 6, 16, 18, 20, 22]).act(t)
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
        0 => 0,
        2 => 1,
        4 => 2,
        6 => 3,
        16 => 4,
        18 => 5,
        20 => 6,
        22 => 7,
        _ => unreachable!("Tried to act left on a permution that does swaps between this orbit and other pieces.  Typically this means that you've tried to implement an invalid symmetry.  We can only go backwards through a permutation where the pieces we track interact only with themselves.  Otherwise, we don't know how to apply it, because we don't know where some of the pieces involved actually are."),
    }
}

fn arr_act_left(a: [u8; 24], b: [u8; 8]) -> [u8; 8] {
    let mut arr = [
        b[adjust(a[0] as usize)],
        b[adjust(a[2] as usize)],
        b[adjust(a[4] as usize)],
        b[adjust(a[6] as usize)],
        b[adjust(a[16] as usize)],
        b[adjust(a[18] as usize)],
        b[adjust(a[20] as usize)],
        b[adjust(a[22] as usize)],
    ];
    arr.sort_unstable();
    arr
}

impl MagmaAction<CoordWingEdges> for CoordWingEdgesSortedUDEvens {
    fn act(self, b: CoordWingEdges) -> Self {
        Self(arr_act(self.0, b.0))
    }
}

impl MonoidAction<CoordWingEdges> for CoordWingEdgesSortedUDEvens {}

impl MagmaAction<UF2Symmetry> for CoordWingEdgesSortedUDEvens {
    fn act(self, s: UF2Symmetry) -> Self {
        let t: CoordWingEdges = s.into();
        self.act(t)
    }
}

impl MonoidAction<UF2Symmetry> for CoordWingEdgesSortedUDEvens {}

impl LeftMagmaAction<UF2Symmetry> for CoordWingEdgesSortedUDEvens {
    fn act_left(s: UF2Symmetry, t: CoordWingEdgesSortedUDEvens) -> Self {
        let s: CoordWingEdges = s.into();
        CoordWingEdgesSortedUDEvens(arr_act_left(s.0, t.0))
    }
}

impl LeftMonoidAction<UF2Symmetry> for CoordWingEdgesSortedUDEvens {}

impl EquivalenceClass<UF2Symmetry> for CoordWingEdgesSortedUDEvens {
    fn get_equivalent(self, sym: &UF2Symmetry) -> CoordWingEdgesSortedUDEvens {
        CoordWingEdgesSortedUDEvens::act_left(sym.invert(), self).act(*sym)
    }
}

fn c(n: i128, k: i128) -> i128 {
    let numerator: i128 = ((n - k + 1)..=n).product();
    let denominator: i128 = (1..=k).product();
    numerator / denominator
}

fn to_combinadic(p: CoordWingEdgesSortedUDEvens) -> u32 {
    let mut x = 0;
    for i in 0..8 {
        x += c(p.0[i] as i128, i as i128 + 1) as u32;
    }
    x
}

fn from_combinadic(i: u32) -> CoordWingEdgesSortedUDEvens {
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
    CoordWingEdgesSortedUDEvens(x)
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
pub struct CoordWingEdgesSortedUDEvensIndex(u32);

impl Sequence for CoordWingEdgesSortedUDEvensIndex {
    const CARDINALITY: usize = 735_471;

    fn next(&self) -> Option<Self> {
        if self.0 == (Self::CARDINALITY - 1) as u32 {
            None
        } else {
            Some(CoordWingEdgesSortedUDEvensIndex(self.0 + 1))
        }
    }

    fn previous(&self) -> Option<Self> {
        if self.0 == 0 {
            None
        } else {
            Some(CoordWingEdgesSortedUDEvensIndex(self.0 - 1))
        }
    }

    fn first() -> Option<Self> {
        Some(CoordWingEdgesSortedUDEvensIndex(0))
    }

    fn last() -> Option<Self> {
        Some(CoordWingEdgesSortedUDEvensIndex((Self::CARDINALITY - 1) as u32))
    }
}

impl Into<CoordWingEdgesSortedUDEvens> for CoordWingEdgesSortedUDEvensIndex {
    fn into(self) -> CoordWingEdgesSortedUDEvens {
        from_combinadic(self.0)
    }
}

impl Into<usize> for CoordWingEdgesSortedUDEvensIndex {
    fn into(self) -> usize {
        self.0 as usize
    }
}

impl TryFrom<usize> for CoordWingEdgesSortedUDEvensIndex {
    type Error = std::num::TryFromIntError;

    fn try_from(i: usize) -> Result<Self, Self::Error> {
        let j = i.try_into()?;
        Ok(CoordWingEdgesSortedUDEvensIndex(j))
    }
}

impl Into<CoordWingEdgesSortedUDEvensIndex> for CoordWingEdgesSortedUDEvens {
    fn into(self) -> CoordWingEdgesSortedUDEvensIndex {
        CoordWingEdgesSortedUDEvensIndex(to_combinadic(self))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use permutation_group::PermutationGroup as PG;
    use move_sets::wide_turns::WideTurn;
    use quickcheck::Gen;
    use rand::Rng;
    use enum_iterator::cardinality;

    impl quickcheck::Arbitrary for CoordWingEdgesSortedUDEvensIndex {
        fn arbitrary<G: Gen>(g: &mut G) -> CoordWingEdgesSortedUDEvensIndex {
            CoordWingEdgesSortedUDEvensIndex(*g.choose(&(0..cardinality::<CoordWingEdgesSortedUDEvensIndex>() as u32).collect::<Vec<_>>()).unwrap())
        }
    }

    // 17s to exhaustively check, so we use quickcheck instead
    quickcheck! {
        fn all_combinadic(i: CoordWingEdgesSortedUDEvensIndex) -> bool {
            i.0 == to_combinadic(from_combinadic(i.0))
        }
    }

    quickcheck! {
        fn group_action_obeys_identity_law(p: CoordWingEdges) -> bool {
            p == p.act(CoordWingEdges::identity())
        }
    }

    quickcheck! {
        fn group_action_obeys_compatibility_law(xi: CoordWingEdgesSortedUDEvensIndex, g_0: CoordWingEdges, g_1: CoordWingEdges) -> bool {
            let x: CoordWingEdgesSortedUDEvens = xi.into();
            let merged = g_0.clone().permute(g_1.clone());
            let act_together = x.act(merged);
            let act_separate = x.act(g_0.clone()).act(g_1.clone());
            act_together == act_separate
        }
    }

    quickcheck! {
        fn turns_and_sym_invert_round_trips(pi: CoordWingEdgesSortedUDEvensIndex, t: WideTurn, s: UF2Symmetry) -> bool {
            let p: CoordWingEdgesSortedUDEvens = pi.into();
            let t_p: CoordWingEdges = t.into();
            let t_p_prime: CoordWingEdges = t.invert().into();
            let s_p: CoordWingEdges = s.into();
            let s_p_prime: CoordWingEdges = s.invert().into();
            p == p.act(t_p).act(t_p_prime)
                && p == p.act(s_p).act(s_p_prime)
        }
    }


    quickcheck! {
        fn perm_and_turn_uf2_symmetries_are_equivalent(pi: CoordWingEdgesSortedUDEvensIndex, t: WideTurn, s: UF2Symmetry) -> bool {
            let p: CoordWingEdgesSortedUDEvens = pi.into();
            let turn: CoordWingEdges = t.into();
            let after_action = p.act(turn).get_equivalent(&s);
            let before_action = p.get_equivalent(&s).act(turn.get_equivalent(&s));
            after_action == before_action
        }
    }

    quickcheck! {
        fn uf2_symmetry_group_action_obeys_identity_law(pi: CoordWingEdgesSortedUDEvensIndex) -> bool {
            let p: CoordWingEdgesSortedUDEvens = pi.into();
            p == p.act(UF2Symmetry::identity())
        }
    }

    quickcheck! {
        fn uf2_symmetry_group_action_obeys_compatibility_law(pi: CoordWingEdgesSortedUDEvensIndex, s0: UF2Symmetry, s1: UF2Symmetry) -> bool {
            let p: CoordWingEdgesSortedUDEvens = pi.into();
            let as_tt = p.act(s0).act(s1);
            let as_sym = p.act(s0.permute(s1));
            as_tt == as_sym
        }
    }

    quickcheck! {
        fn uf2_symmetry_group_left_action_obeys_compatibility_law(pi: CoordWingEdgesSortedUDEvensIndex, s0: UF2Symmetry, s1: UF2Symmetry) -> bool {
            let p: CoordWingEdgesSortedUDEvens = pi.into();
            let s01 = s0.permute(s1);
            let as_tt = CoordWingEdgesSortedUDEvens::act_left(s0, CoordWingEdgesSortedUDEvens::act_left(s1, p));
            let as_sym = CoordWingEdgesSortedUDEvens::act_left(s01, p);
            as_tt == as_sym
        }
    }
}
