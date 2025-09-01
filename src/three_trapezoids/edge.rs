use permutation_group::PermutationGroup as PG;
use invertable::Invertable;
use equivalence_class::EquivalenceClass;
use algebraic_actions::{MagmaAction, MonoidAction, LeftMagmaAction, LeftMonoidAction};
use three_trapezoids::{ThreeTrapezoids, NoSymmetry, MirrorUDSymmetry, Turns};

use std::collections::BTreeMap;
use std::collections::VecDeque;
use std::convert::{TryInto, TryFrom};
use enum_iterator::{all, Sequence};

// This module is a pattern of ThreeTrapezoids, tracing only the two pieces
// that form the edge between the two right faces.  So there are only two
// entries, despite being 6 pieces in 6 locations.  Each entry tracks the
// current location of a single piece, so [0, 3] is when the tracked pieces are
// solved.
//
//  ?
//
//    ?
//       0  3
//    ?
//
//  ?
//
// And [1, 0] if we perform a clockwise upper right face turn.
//
//  ?
//
//    0
//       3  ?
//    ?
//
//  ?
//
// The positions of the untracked positions are completely unknown.
//
// Notably, this pattern only supports MirrorUDSymmetry, because we can only
// support symmetries that remap the correct positions of the pieces tracked to
// others that are also tracked.  MirrorUDSymmetry does this trivially, because
// they don't actually move.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ThreeTrapezoidsEdge([u16; 2]);

impl From<ThreeTrapezoids> for ThreeTrapezoidsEdge {
    fn from(t: ThreeTrapezoids) -> Self {
        ThreeTrapezoidsEdge([t.0[0], t.0[3]])
    }
}

fn arr_act(a: [u16; 2], b: [u16; 6]) -> [u16; 2] {
    [
        b[a[0] as usize],
        b[a[1] as usize],
    ]
}

fn arr_act_left(a: [u16; 6], b: [u16; 2]) -> [u16; 2] {
    [
        b[a[0] as usize],
        b[a[3] as usize - 2],
    ]
}

impl MagmaAction<ThreeTrapezoids> for ThreeTrapezoidsEdge {
    fn act(self, b: ThreeTrapezoids) -> Self {
        Self(arr_act(self.0, b.0))
    }
}

impl MonoidAction<ThreeTrapezoids> for ThreeTrapezoidsEdge {}

impl MagmaAction<NoSymmetry> for ThreeTrapezoidsEdge {
    fn act(self, s: NoSymmetry) -> Self {
        let t: ThreeTrapezoids = s.into();
        self.act(t)
    }
}

impl MonoidAction<NoSymmetry> for ThreeTrapezoidsEdge {}

impl LeftMagmaAction<NoSymmetry> for ThreeTrapezoidsEdge {
    fn act_left(s: NoSymmetry, t: ThreeTrapezoidsEdge) -> Self {
        let s: ThreeTrapezoids = s.into();
        ThreeTrapezoidsEdge(arr_act_left(s.0, t.0))
    }
}

impl LeftMonoidAction<NoSymmetry> for ThreeTrapezoidsEdge {}

impl EquivalenceClass<NoSymmetry> for ThreeTrapezoidsEdge {
    fn get_equivalent(self, sym: &NoSymmetry) -> ThreeTrapezoidsEdge {
        let x: MirrorUDSymmetry = sym.clone().into();
        self.get_equivalent(&x)
    }
}

impl MagmaAction<MirrorUDSymmetry> for ThreeTrapezoidsEdge {
    fn act(self, s: MirrorUDSymmetry) -> Self {
        let t: ThreeTrapezoids = s.into();
        self.act(t)
    }
}

impl MonoidAction<MirrorUDSymmetry> for ThreeTrapezoidsEdge {}

impl LeftMagmaAction<MirrorUDSymmetry> for ThreeTrapezoidsEdge {
    fn act_left(s: MirrorUDSymmetry, t: ThreeTrapezoidsEdge) -> Self {
        let s: ThreeTrapezoids = s.into();
        ThreeTrapezoidsEdge(arr_act_left(s.0, t.0))
    }
}

impl LeftMonoidAction<MirrorUDSymmetry> for ThreeTrapezoidsEdge {}

impl EquivalenceClass<MirrorUDSymmetry> for ThreeTrapezoidsEdge {
    fn get_equivalent(self, sym: &MirrorUDSymmetry) -> ThreeTrapezoidsEdge {
        ThreeTrapezoidsEdge::act_left(sym.invert(), self).act(*sym)
    }
}

// TODO: There are faster algorithms than this
fn to_lehmer(p: ThreeTrapezoidsEdge) -> u16 {
    let mut x = p.0.clone();
    for i in 0..2 {
        for j in (i+1)..2 {
            if x[j] > x[i] {
                x[j] -= 1;
            }
        }
    }
    5 * x[0] + x[1]
}

// TODO: There are faster algorithms than this
fn from_lehmer(i: u16) -> ThreeTrapezoidsEdge {
    let mut x =
        [ i / 5
        , i % 5
        ];
    for i in (0..2).rev() {
        for j in (i+1)..2 {
            if x[j as usize] >= x[i] {
                x[j as usize] += 1;
            }
        }
    }
    ThreeTrapezoidsEdge(x)
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
pub struct ThreeTrapezoidsEdgeIndex(u16);

impl Sequence for ThreeTrapezoidsEdgeIndex {
    const CARDINALITY: usize = 30;

    fn next(&self) -> Option<Self> {
        if self.0 == (Self::CARDINALITY - 1) as u16 {
            None
        } else {
            Some(ThreeTrapezoidsEdgeIndex(self.0 + 1))
        }
    }

    fn previous(&self) -> Option<Self> {
        if self.0 == 0 {
            None
        } else {
            Some(ThreeTrapezoidsEdgeIndex(self.0 - 1))
        }
    }

    fn first() -> Option<Self> {
        Some(ThreeTrapezoidsEdgeIndex(0))
    }

    fn last() -> Option<Self> {
        Some(ThreeTrapezoidsEdgeIndex((Self::CARDINALITY - 1) as u16))
    }
}

impl Into<ThreeTrapezoidsEdge> for ThreeTrapezoidsEdgeIndex {
    fn into(self) -> ThreeTrapezoidsEdge {
        from_lehmer(self.0)
    }
}

impl Into<usize> for ThreeTrapezoidsEdgeIndex {
    fn into(self) -> usize {
        self.0 as usize
    }
}

impl TryFrom<usize> for ThreeTrapezoidsEdgeIndex {
    type Error = std::num::TryFromIntError;

    fn try_from(i: usize) -> Result<Self, Self::Error> {
        let j = i.try_into()?;
        Ok(ThreeTrapezoidsEdgeIndex(j))
    }
}

impl Into<ThreeTrapezoidsEdgeIndex> for ThreeTrapezoidsEdge {
    fn into(self) -> ThreeTrapezoidsEdgeIndex {
        ThreeTrapezoidsEdgeIndex(to_lehmer(self))
    }
}

pub fn moves_to_solve() -> BTreeMap<ThreeTrapezoidsEdgeIndex, usize> {
    let mut queue = VecDeque::new();
    let mut map = BTreeMap::new();

    let identity: ThreeTrapezoidsEdge = ThreeTrapezoids::identity().into();
    map.insert(identity.clone().into(), 0);
    queue.push_back((identity, 1));

    loop {
        match queue.pop_front() {
            None => break,
            Some((p, count)) => {
                for t in all::<Turns>() {
                    let turn: ThreeTrapezoids = t.into();
                    let turned = p.clone().act(turn);
                    match map.get(&turned.into()) {
                        None => {
                            map.insert(turned.into(), count);
                            queue.push_back((turned, count + 1))
                        },
                        Some(_) => (),
                    }
                }
            },
        }
    }
    map
}

#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck::Gen;
    use rand::Rng;
    use enum_iterator::{all, cardinality};
    use three_trapezoids::ThreeTrapezoidsIndex;

    impl quickcheck::Arbitrary for ThreeTrapezoidsEdgeIndex {
        fn arbitrary<G: Gen>(g: &mut G) -> ThreeTrapezoidsEdgeIndex {
            ThreeTrapezoidsEdgeIndex(*g.choose(&(0..cardinality::<ThreeTrapezoidsEdgeIndex>() as u16).collect::<Vec<_>>()).unwrap())
        }
    }

    #[test]
    fn all_odd_lehmer_codes_round_trip() {
        for i in 0..720u16 {
            let t: ThreeTrapezoidsEdge = from_lehmer(i);
            assert_eq!(i, to_lehmer(t));
        }
    }

    #[test]
    fn three_trapezoids_group_action_obeys_identity_law() {
        for pi in all::<ThreeTrapezoidsEdgeIndex>() {
            let p: ThreeTrapezoidsEdge = pi.into();
            assert_eq!(p, p.act(ThreeTrapezoids::identity()));
        }
    }

    // Even thugh this puzzle is quite small, it's still too big for exhaustive checking
    quickcheck! {
        fn three_trapezoids_group_action_obeys_compatibility_law(xi: ThreeTrapezoidsEdgeIndex, gi_0: ThreeTrapezoidsIndex, gi_1: ThreeTrapezoidsIndex) -> bool {
            let x: ThreeTrapezoidsEdge = xi.into();
            let g_0: ThreeTrapezoids = gi_0.into();
            let g_1: ThreeTrapezoids = gi_1.into();
            let merged = g_0.clone().permute(g_1.clone());
            let act_together = x.act(merged);
            let act_separate = x.act(g_0.clone()).act(g_1.clone());
            act_together == act_separate
        }
    }

    #[test]
    fn perm_and_turns_and_sym_invert_round_trips() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for pi in all::<ThreeTrapezoidsEdgeIndex>() {
            let p: ThreeTrapezoidsEdge = pi.into();

            for t in all::<Turns>() {
                let pt: ThreeTrapezoids = t.into();
                assert_eq!(p, p.act(pt).act(pt.invert()));
            }

            for s in all::<NoSymmetry>() {
                let st: ThreeTrapezoids = s.into();
                assert_eq!(p, p.act(st).act(st.invert()));
            }

            for s in all::<MirrorUDSymmetry>() {
                let st: ThreeTrapezoids = s.into();
                assert_eq!(p, p.act(st).act(st.invert()));
            }
        }
    }

    #[test]
    fn perm_and_turn_no_symmetries_are_equivalent() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s in all::<NoSymmetry>() {
            for pi in all::<ThreeTrapezoidsEdgeIndex>() {
                let p: ThreeTrapezoidsEdge = pi.into();
                for t in all::<Turns>() {
                    let turn: ThreeTrapezoids = t.into();
                    let after_action = p.act(turn).get_equivalent(&s);
                    let before_action = p.get_equivalent(&s).act(turn.get_equivalent(&s));
                    assert_eq!(after_action, before_action)
                }
            }
        }
    }

    #[test]
    fn perm_and_turn_mirror_ud_symmetries_are_equivalent() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s in all::<MirrorUDSymmetry>() {
            for pi in all::<ThreeTrapezoidsEdgeIndex>() {
                let p: ThreeTrapezoidsEdge = pi.into();
                for t in all::<Turns>() {
                    let turn: ThreeTrapezoids = t.into();
                    let after_action = p.act(turn).get_equivalent(&s);
                    let before_action = p.get_equivalent(&s).act(turn.get_equivalent(&s));
                    assert_eq!(after_action, before_action)
                }
            }
        }
    }

    #[test]
    fn mirror_ud_symmetry_group_action_obeys_identity_law() {
        for pi in all::<ThreeTrapezoidsEdgeIndex>() {
            let p: ThreeTrapezoidsEdge = pi.into();
            assert_eq!(p, p.act(MirrorUDSymmetry::identity()));
        }
    }

    #[test]
    fn mirror_ud_symmetry_group_action_obeys_compatibility_law() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s0 in all::<MirrorUDSymmetry>() {
            for s1 in all::<MirrorUDSymmetry>() {
                for pi in all::<ThreeTrapezoidsEdgeIndex>() {
                    let p: ThreeTrapezoidsEdge = pi.into();
                    let as_tt = p.act(s0).act(s1);
                    let as_sym = p.act(s0.permute(s1));
                    assert_eq!(as_tt, as_sym);
                }
            }
        }
    }

    #[test]
    fn no_symmetry_group_action_obeys_identity_law() {
        for pi in all::<ThreeTrapezoidsEdgeIndex>() {
            let p: ThreeTrapezoidsEdge = pi.into();
            assert_eq!(p, p.act(NoSymmetry::identity()));
        }
    }

    #[test]
    fn no_symmetry_group_action_obeys_compatibility_law() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s0 in all::<NoSymmetry>() {
            for s1 in all::<NoSymmetry>() {
                for pi in all::<ThreeTrapezoidsEdgeIndex>() {
                    let p: ThreeTrapezoidsEdge = pi.into();
                    let as_tt = p.act(s0).act(s1);
                    let as_sym = p.act(s0.permute(s1));
                    assert_eq!(as_tt, as_sym);
                }
            }
        }
    }

    #[test]
    fn no_symmetry_group_left_action_obeys_compatibility_law() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s0 in all::<NoSymmetry>() {
            for s1 in all::<NoSymmetry>() {
                for pi in all::<ThreeTrapezoidsEdgeIndex>() {
                    let p: ThreeTrapezoidsEdge = pi.into();
                    let s01 = s0.permute(s1);
                    let as_tt = ThreeTrapezoidsEdge::act_left(s0, ThreeTrapezoidsEdge::act_left(s1, p));
                    let as_sym = ThreeTrapezoidsEdge::act_left(s01, p);
                    assert_eq!(as_tt, as_sym);
                }
            }
        }
    }

    #[test]
    fn mirror_ud_symmetry_group_left_action_obeys_compatibility_law() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s0 in all::<MirrorUDSymmetry>() {
            for s1 in all::<MirrorUDSymmetry>() {
                for pi in all::<ThreeTrapezoidsEdgeIndex>() {
                    let p: ThreeTrapezoidsEdge = pi.into();
                    let s01 = s0.permute(s1);
                    let as_tt = ThreeTrapezoidsEdge::act_left(s0, ThreeTrapezoidsEdge::act_left(s1, p));
                    let as_sym = ThreeTrapezoidsEdge::act_left(s01, p);
                    assert_eq!(as_tt, as_sym);
                }
            }
        }
    }
}
