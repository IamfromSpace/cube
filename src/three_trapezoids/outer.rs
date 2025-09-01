use permutation_group::PermutationGroup as PG;
use invertable::Invertable;
use equivalence_class::EquivalenceClass;
use algebraic_actions::{MagmaAction, MonoidAction, LeftMagmaAction, LeftMonoidAction};
use three_trapezoids::{ThreeTrapezoids, NoSymmetry, MirrorUDSymmetry, RotationalSymmetry, FullSymmetry, Turns};

use std::collections::BTreeMap;
use std::collections::VecDeque;
use std::convert::{TryInto, TryFrom};
use enum_iterator::{all, Sequence};

// This module is a pattern of ThreeTrapezoids, tracing only the positions of
// the outer three pieces.  So there are only three entries, despite being 6
// pieces in 6 locations.  Each entry tracks the current location of a single
// piece, so [3, 4, 5] is when the tracked pieces are solved.
//
//  4
//
//    ?
//       ?  3
//    ?
//
//  5
//
// And [3, 1, 4] if we perform a clockwise left face turn.
//
//  5
//
//    4
//       ?  3
//    ?
//
//  ?
//
// The positions of the untracked positions are completely unknown.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ThreeTrapezoidsOuter([u16; 3]);

impl From<ThreeTrapezoids> for ThreeTrapezoidsOuter {
    fn from(t: ThreeTrapezoids) -> Self {
        ThreeTrapezoidsOuter([t.0[3], t.0[4], t.0[5]])
    }
}

fn arr_act(a: [u16; 3], b: [u16; 6]) -> [u16; 3] {
    [
        b[a[0] as usize],
        b[a[1] as usize],
        b[a[2] as usize],
    ]
}

fn arr_act_left(a: [u16; 6], b: [u16; 3]) -> [u16; 3] {
    [
        b[a[3] as usize - 3],
        b[a[4] as usize - 3],
        b[a[5] as usize - 3],
    ]
}

impl MagmaAction<ThreeTrapezoids> for ThreeTrapezoidsOuter {
    fn act(self, b: ThreeTrapezoids) -> Self {
        Self(arr_act(self.0, b.0))
    }
}

impl MonoidAction<ThreeTrapezoids> for ThreeTrapezoidsOuter {}

impl MagmaAction<FullSymmetry> for ThreeTrapezoidsOuter {
    fn act(self, s: FullSymmetry) -> Self {
        let t: ThreeTrapezoids = s.into();
        self.act(t)
    }
}

impl MonoidAction<FullSymmetry> for ThreeTrapezoidsOuter {}

impl LeftMagmaAction<FullSymmetry> for ThreeTrapezoidsOuter {
    fn act_left(s: FullSymmetry, t: ThreeTrapezoidsOuter) -> Self {
        let s: ThreeTrapezoids = s.into();
        ThreeTrapezoidsOuter(arr_act_left(s.0, t.0))
    }
}

impl LeftMonoidAction<FullSymmetry> for ThreeTrapezoidsOuter {}

impl EquivalenceClass<FullSymmetry> for ThreeTrapezoidsOuter {
    fn get_equivalent(self, sym: &FullSymmetry) -> ThreeTrapezoidsOuter {
        ThreeTrapezoidsOuter::act_left(sym.invert(), self).act(*sym)
    }
}

impl MagmaAction<NoSymmetry> for ThreeTrapezoidsOuter {
    fn act(self, s: NoSymmetry) -> Self {
        let t: ThreeTrapezoids = s.into();
        self.act(t)
    }
}

impl MonoidAction<NoSymmetry> for ThreeTrapezoidsOuter {}

impl LeftMagmaAction<NoSymmetry> for ThreeTrapezoidsOuter {
    fn act_left(s: NoSymmetry, t: ThreeTrapezoidsOuter) -> Self {
        let s: ThreeTrapezoids = s.into();
        ThreeTrapezoidsOuter(arr_act_left(s.0, t.0))
    }
}

impl LeftMonoidAction<NoSymmetry> for ThreeTrapezoidsOuter {}

impl EquivalenceClass<NoSymmetry> for ThreeTrapezoidsOuter {
    fn get_equivalent(self, sym: &NoSymmetry) -> ThreeTrapezoidsOuter {
        let x: FullSymmetry = sym.clone().into();
        self.get_equivalent(&x)
    }
}

impl MagmaAction<MirrorUDSymmetry> for ThreeTrapezoidsOuter {
    fn act(self, s: MirrorUDSymmetry) -> Self {
        let t: ThreeTrapezoids = s.into();
        self.act(t)
    }
}

impl MonoidAction<MirrorUDSymmetry> for ThreeTrapezoidsOuter {}

impl LeftMagmaAction<MirrorUDSymmetry> for ThreeTrapezoidsOuter {
    fn act_left(s: MirrorUDSymmetry, t: ThreeTrapezoidsOuter) -> Self {
        let s: ThreeTrapezoids = s.into();
        ThreeTrapezoidsOuter(arr_act_left(s.0, t.0))
    }
}

impl LeftMonoidAction<MirrorUDSymmetry> for ThreeTrapezoidsOuter {}

impl EquivalenceClass<MirrorUDSymmetry> for ThreeTrapezoidsOuter {
    fn get_equivalent(self, sym: &MirrorUDSymmetry) -> ThreeTrapezoidsOuter {
        let x: FullSymmetry = sym.clone().into();
        self.get_equivalent(&x)
    }
}

impl MagmaAction<RotationalSymmetry> for ThreeTrapezoidsOuter {
    fn act(self, s: RotationalSymmetry) -> Self {
        let t: ThreeTrapezoids = s.into();
        self.act(t)
    }
}

impl MonoidAction<RotationalSymmetry> for ThreeTrapezoidsOuter {}

impl LeftMagmaAction<RotationalSymmetry> for ThreeTrapezoidsOuter {
    fn act_left(s: RotationalSymmetry, t: ThreeTrapezoidsOuter) -> Self {
        let s: ThreeTrapezoids = s.into();
        ThreeTrapezoidsOuter(arr_act_left(s.0, t.0))
    }
}

impl LeftMonoidAction<RotationalSymmetry> for ThreeTrapezoidsOuter {}

impl EquivalenceClass<RotationalSymmetry> for ThreeTrapezoidsOuter {
    fn get_equivalent(self, sym: &RotationalSymmetry) -> ThreeTrapezoidsOuter {
        let x: FullSymmetry = sym.clone().into();
        self.get_equivalent(&x)
    }
}

// TODO: There are faster algorithms than this
fn to_lehmer(p: ThreeTrapezoidsOuter) -> u16 {
    let mut x = p.0.clone();
    for i in 0..3 {
        for j in (i+1)..3 {
            if x[j] > x[i] {
                x[j] -= 1;
            }
        }
    }
    5 * 4 * x[0] + 4 * x[1] + x[2]
}

// TODO: There are faster algorithms than this
fn from_lehmer(i: u16) -> ThreeTrapezoidsOuter {
    let mut x =
        [ i / 20
        , (i / 4) % 5
        , i % 4
        ];
    for i in (0..3).rev() {
        for j in (i+1)..3 {
            if x[j as usize] >= x[i] {
                x[j as usize] += 1;
            }
        }
    }
    ThreeTrapezoidsOuter(x)
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
pub struct ThreeTrapezoidsOuterIndex(u16);

impl Sequence for ThreeTrapezoidsOuterIndex {
    const CARDINALITY: usize = 120;

    fn next(&self) -> Option<Self> {
        if self.0 == (Self::CARDINALITY - 1) as u16 {
            None
        } else {
            Some(ThreeTrapezoidsOuterIndex(self.0 + 1))
        }
    }

    fn previous(&self) -> Option<Self> {
        if self.0 == 0 {
            None
        } else {
            Some(ThreeTrapezoidsOuterIndex(self.0 - 1))
        }
    }

    fn first() -> Option<Self> {
        Some(ThreeTrapezoidsOuterIndex(0))
    }

    fn last() -> Option<Self> {
        Some(ThreeTrapezoidsOuterIndex((Self::CARDINALITY - 1) as u16))
    }
}

impl Into<ThreeTrapezoidsOuter> for ThreeTrapezoidsOuterIndex {
    fn into(self) -> ThreeTrapezoidsOuter {
        from_lehmer(self.0)
    }
}

impl Into<usize> for ThreeTrapezoidsOuterIndex {
    fn into(self) -> usize {
        self.0 as usize
    }
}

impl TryFrom<usize> for ThreeTrapezoidsOuterIndex {
    type Error = std::num::TryFromIntError;

    fn try_from(i: usize) -> Result<Self, Self::Error> {
        let j = i.try_into()?;
        Ok(ThreeTrapezoidsOuterIndex(j))
    }
}

impl Into<ThreeTrapezoidsOuterIndex> for ThreeTrapezoidsOuter {
    fn into(self) -> ThreeTrapezoidsOuterIndex {
        ThreeTrapezoidsOuterIndex(to_lehmer(self))
    }
}

pub fn moves_to_solve() -> BTreeMap<ThreeTrapezoidsOuterIndex, usize> {
    let mut queue = VecDeque::new();
    let mut map = BTreeMap::new();

    let identity: ThreeTrapezoidsOuter = ThreeTrapezoids::identity().into();
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

    impl quickcheck::Arbitrary for ThreeTrapezoidsOuterIndex {
        fn arbitrary<G: Gen>(g: &mut G) -> ThreeTrapezoidsOuterIndex {
            ThreeTrapezoidsOuterIndex(*g.choose(&(0..cardinality::<ThreeTrapezoidsOuterIndex>() as u16).collect::<Vec<_>>()).unwrap())
        }
    }

    #[test]
    fn all_odd_lehmer_codes_round_trip() {
        for i in 0..720u16 {
            let t: ThreeTrapezoidsOuter = from_lehmer(i);
            assert_eq!(i, to_lehmer(t));
        }
    }

    #[test]
    fn three_trapezoids_group_action_obeys_identity_law() {
        for pi in all::<ThreeTrapezoidsOuterIndex>() {
            let p: ThreeTrapezoidsOuter = pi.into();
            assert_eq!(p, p.act(ThreeTrapezoids::identity()));
        }
    }

    // Even thugh this puzzle is quite small, it's still too big for exhaustive checking
    quickcheck! {
        fn three_trapezoids_group_action_obeys_compatibility_law(xi: ThreeTrapezoidsOuterIndex, gi_0: ThreeTrapezoidsIndex, gi_1: ThreeTrapezoidsIndex) -> bool {
            let x: ThreeTrapezoidsOuter = xi.into();
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
        for pi in all::<ThreeTrapezoidsOuterIndex>() {
            let p: ThreeTrapezoidsOuter = pi.into();

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

            for s in all::<RotationalSymmetry>() {
                let st: ThreeTrapezoids = s.into();
                assert_eq!(p, p.act(st).act(st.invert()));
            }

            for s in all::<FullSymmetry>() {
                let st: ThreeTrapezoids = s.into();
                assert_eq!(p, p.act(st).act(st.invert()));
            }
        }
    }

    #[test]
    fn perm_and_turn_no_symmetries_are_equivalent() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s in all::<NoSymmetry>() {
            for pi in all::<ThreeTrapezoidsOuterIndex>() {
                let p: ThreeTrapezoidsOuter = pi.into();
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
            for pi in all::<ThreeTrapezoidsOuterIndex>() {
                let p: ThreeTrapezoidsOuter = pi.into();
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
    fn perm_and_turn_rotational_symmetries_are_equivalent() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s in all::<RotationalSymmetry>() {
            for pi in all::<ThreeTrapezoidsOuterIndex>() {
                let p: ThreeTrapezoidsOuter = pi.into();
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
    fn perm_and_turn_full_symmetries_are_equivalent() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s in all::<FullSymmetry>() {
            for pi in all::<ThreeTrapezoidsOuterIndex>() {
                let p: ThreeTrapezoidsOuter = pi.into();
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
    fn full_symmetry_group_action_obeys_identity_law() {
        for pi in all::<ThreeTrapezoidsOuterIndex>() {
            let p: ThreeTrapezoidsOuter = pi.into();
            assert_eq!(p, p.act(FullSymmetry::identity()));
        }
    }

    #[test]
    fn full_symmetry_group_action_obeys_compatibility_law() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s0 in all::<FullSymmetry>() {
            for s1 in all::<FullSymmetry>() {
                for pi in all::<ThreeTrapezoidsOuterIndex>() {
                    let p: ThreeTrapezoidsOuter = pi.into();
                    let as_tt = p.act(s0).act(s1);
                    let as_sym = p.act(s0.permute(s1));
                    assert_eq!(as_tt, as_sym);
                }
            }
        }
    }

    #[test]
    fn mirror_ud_symmetry_group_action_obeys_identity_law() {
        for pi in all::<ThreeTrapezoidsOuterIndex>() {
            let p: ThreeTrapezoidsOuter = pi.into();
            assert_eq!(p, p.act(MirrorUDSymmetry::identity()));
        }
    }

    #[test]
    fn mirror_ud_symmetry_group_action_obeys_compatibility_law() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s0 in all::<MirrorUDSymmetry>() {
            for s1 in all::<MirrorUDSymmetry>() {
                for pi in all::<ThreeTrapezoidsOuterIndex>() {
                    let p: ThreeTrapezoidsOuter = pi.into();
                    let as_tt = p.act(s0).act(s1);
                    let as_sym = p.act(s0.permute(s1));
                    assert_eq!(as_tt, as_sym);
                }
            }
        }
    }

    #[test]
    fn rotational_symmetry_group_action_obeys_identity_law() {
        for pi in all::<ThreeTrapezoidsOuterIndex>() {
            let p: ThreeTrapezoidsOuter = pi.into();
            assert_eq!(p, p.act(RotationalSymmetry::identity()));
        }
    }

    #[test]
    fn rotational_symmetry_group_action_obeys_compatibility_law() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s0 in all::<RotationalSymmetry>() {
            for s1 in all::<RotationalSymmetry>() {
                for pi in all::<ThreeTrapezoidsOuterIndex>() {
                    let p: ThreeTrapezoidsOuter = pi.into();
                    let as_tt = p.act(s0).act(s1);
                    let as_sym = p.act(s0.permute(s1));
                    assert_eq!(as_tt, as_sym);
                }
            }
        }
    }

    #[test]
    fn no_symmetry_group_action_obeys_identity_law() {
        for pi in all::<ThreeTrapezoidsOuterIndex>() {
            let p: ThreeTrapezoidsOuter = pi.into();
            assert_eq!(p, p.act(NoSymmetry::identity()));
        }
    }

    #[test]
    fn no_symmetry_group_action_obeys_compatibility_law() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s0 in all::<NoSymmetry>() {
            for s1 in all::<NoSymmetry>() {
                for pi in all::<ThreeTrapezoidsOuterIndex>() {
                    let p: ThreeTrapezoidsOuter = pi.into();
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
                for pi in all::<ThreeTrapezoidsOuterIndex>() {
                    let p: ThreeTrapezoidsOuter = pi.into();
                    let s01 = s0.permute(s1);
                    let as_tt = ThreeTrapezoidsOuter::act_left(s0, ThreeTrapezoidsOuter::act_left(s1, p));
                    let as_sym = ThreeTrapezoidsOuter::act_left(s01, p);
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
                for pi in all::<ThreeTrapezoidsOuterIndex>() {
                    let p: ThreeTrapezoidsOuter = pi.into();
                    let s01 = s0.permute(s1);
                    let as_tt = ThreeTrapezoidsOuter::act_left(s0, ThreeTrapezoidsOuter::act_left(s1, p));
                    let as_sym = ThreeTrapezoidsOuter::act_left(s01, p);
                    assert_eq!(as_tt, as_sym);
                }
            }
        }
    }

    #[test]
    fn rotational_symmetry_group_left_action_obeys_compatibility_law() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s0 in all::<RotationalSymmetry>() {
            for s1 in all::<RotationalSymmetry>() {
                for pi in all::<ThreeTrapezoidsOuterIndex>() {
                    let p: ThreeTrapezoidsOuter = pi.into();
                    let s01 = s0.permute(s1);
                    let as_tt = ThreeTrapezoidsOuter::act_left(s0, ThreeTrapezoidsOuter::act_left(s1, p));
                    let as_sym = ThreeTrapezoidsOuter::act_left(s01, p);
                    assert_eq!(as_tt, as_sym);
                }
            }
        }
    }

    #[test]
    fn full_symmetry_group_left_action_obeys_compatibility_law() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s0 in all::<FullSymmetry>() {
            for s1 in all::<FullSymmetry>() {
                for pi in all::<ThreeTrapezoidsOuterIndex>() {
                    let p: ThreeTrapezoidsOuter = pi.into();
                    let s01 = s0.permute(s1);
                    let as_tt = ThreeTrapezoidsOuter::act_left(s0, ThreeTrapezoidsOuter::act_left(s1, p));
                    let as_sym = ThreeTrapezoidsOuter::act_left(s01, p);
                    assert_eq!(as_tt, as_sym);
                }
            }
        }
    }
}
