use permutation_group::PermutationGroup as PG;
use invertable::Invertable;
use equivalence_class::EquivalenceClass;

use std::collections::BTreeMap;
use std::collections::VecDeque;
use std::convert::{TryInto, TryFrom};
use enum_iterator::{all, Sequence};

// We have a simple little puzzle with three "faces" that all share a middle
// and each share an edge.  we can rotate either clockwise or
// counter-clockwise.  Even though just clockwise turns generate all reachable
// states, we get more symmetries if we include clockwise turns (because the
// turns must also follow the symmetries).
//
// 2
//   0 1
// 3
//
// A clockwise turn of the upper-right face yields:
//
// 0
//   1 2
// 3
//
// While there are 24 hypothetical permutations, only 12 of them are reachable
// by moves, because turns always perform an even number of swaps--odd
// permutations are not representable.
//
// We consider symmetry through mirroring top-down and rotation by 120 degrees.
// Combination of the two allows mirroring along any face.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ThreeTriangles([u8; 4]);

const fn permute_arr(a: &[u8; 4], b: &[u8; 4]) -> [u8; 4] {
    [
        a[b[0] as usize],
        a[b[1] as usize],
        a[b[2] as usize],
        a[b[3] as usize],
    ]
}

const fn arr_inv(a: &[u8; 4]) -> [u8; 4] {
    let mut r = [0; 4];
    r[a[0] as usize] = 0;
    r[a[1] as usize] = 1;
    r[a[2] as usize] = 2;
    r[a[3] as usize] = 3;
    r
}

impl functional::BinaryOperation<ThreeTriangles> for ThreeTriangles {
    fn apply(a: ThreeTriangles, b: ThreeTriangles) -> ThreeTriangles {
        ThreeTriangles(permute_arr(&a.0, &b.0))
    }
}

// TODO: this can be generalized, also, isn't this a property of being a permutation group?
fn is_even_parity(x: [u8; 4]) -> bool {
    let mut visited_in_look_ahead = [false; 4];
    let mut even_cycle_count = 0;
    for i in 0..4 {
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

impl functional::AssociativeOperation<ThreeTriangles> for ThreeTriangles { }

impl functional::Monoid<ThreeTriangles> for ThreeTriangles {
    fn one() -> ThreeTriangles {
        ThreeTriangles([0, 1, 2, 3])
    }
}

impl Invertable for ThreeTriangles {
    fn invert(&self) -> ThreeTriangles {
        ThreeTriangles(arr_inv(&self.0))
    }
}

impl PG for ThreeTriangles {}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash, Sequence)]
#[repr(u8)]
pub enum Turns {
    Left,
    // Primes are always counter clockwise
    LeftPrime,
    UpperRight,
    UpperRightPrime,
    LowerRight,
    LowerRightPrime,
}

impl Invertable for Turns {
    fn invert(&self) -> Turns {
        match self {
            Turns::Left => Turns::LeftPrime,
            Turns::LeftPrime => Turns::Left,
            Turns::UpperRight => Turns::UpperRightPrime,
            Turns::UpperRightPrime => Turns::UpperRight,
            Turns::LowerRight => Turns::LowerRightPrime,
            Turns::LowerRightPrime => Turns::LowerRight,
        }
    }
}

impl Into<usize> for Turns {
    fn into(self) -> usize {
        self as usize
    }
}

impl Into<ThreeTriangles> for Turns {
    fn into(self) -> ThreeTriangles {
        match self {
            // 0 -> 2 -> 3
            Turns::Left => ThreeTriangles([2, 1, 3, 0]),
            Turns::LeftPrime => ThreeTriangles([3, 1, 0, 2]),
            // 0 -> 1 -> 2
            Turns::UpperRight => ThreeTriangles([1, 2, 0, 3]),
            Turns::UpperRightPrime => ThreeTriangles([2, 0, 1, 3]),
            // 0 -> 3 -> 1
            Turns::LowerRight => ThreeTriangles([3, 0, 2, 1]),
            Turns::LowerRightPrime => ThreeTriangles([1, 3, 2, 0]),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash, Sequence)]
#[repr(u8)]
pub enum FullSymmetry {
    Identity,
    RotateCounterClock,
    RotateClock,
    // Indicates mirroring along the middle of the face, or along the edge not of this face.
    MirrorLeft,
    MirrorUpperRight,
    MirrorLowerRight,
}

impl Into<usize> for FullSymmetry {
    fn into(self) -> usize {
        self as usize
    }
}

impl functional::BinaryOperation<FullSymmetry> for FullSymmetry {
    fn apply(a: FullSymmetry, b: FullSymmetry) -> FullSymmetry {
        match (a, b) {
            (FullSymmetry::Identity, FullSymmetry::Identity) => FullSymmetry::Identity,
            (FullSymmetry::Identity, FullSymmetry::RotateCounterClock) => FullSymmetry::RotateCounterClock,
            (FullSymmetry::Identity, FullSymmetry::RotateClock) => FullSymmetry::RotateClock,
            (FullSymmetry::Identity, FullSymmetry::MirrorLeft) => FullSymmetry::MirrorLeft,
            (FullSymmetry::Identity, FullSymmetry::MirrorUpperRight) => FullSymmetry::MirrorUpperRight,
            (FullSymmetry::Identity, FullSymmetry::MirrorLowerRight) => FullSymmetry::MirrorLowerRight,

            (FullSymmetry::RotateCounterClock, FullSymmetry::Identity) => FullSymmetry::RotateCounterClock,
            (FullSymmetry::RotateCounterClock, FullSymmetry::RotateCounterClock) => FullSymmetry::RotateClock,
            (FullSymmetry::RotateCounterClock, FullSymmetry::RotateClock) => FullSymmetry::Identity,
            (FullSymmetry::RotateCounterClock, FullSymmetry::MirrorLeft) => FullSymmetry::MirrorLowerRight,
            (FullSymmetry::RotateCounterClock, FullSymmetry::MirrorUpperRight) => FullSymmetry::MirrorLeft,
            (FullSymmetry::RotateCounterClock, FullSymmetry::MirrorLowerRight) => FullSymmetry::MirrorUpperRight,

            (FullSymmetry::RotateClock, FullSymmetry::Identity) => FullSymmetry::RotateClock,
            (FullSymmetry::RotateClock, FullSymmetry::RotateCounterClock) => FullSymmetry::Identity,
            (FullSymmetry::RotateClock, FullSymmetry::RotateClock) => FullSymmetry::RotateCounterClock,
            (FullSymmetry::RotateClock, FullSymmetry::MirrorLeft) => FullSymmetry::MirrorUpperRight,
            (FullSymmetry::RotateClock, FullSymmetry::MirrorUpperRight) => FullSymmetry::MirrorLowerRight,
            (FullSymmetry::RotateClock, FullSymmetry::MirrorLowerRight) => FullSymmetry::MirrorLeft,

            (FullSymmetry::MirrorLeft, FullSymmetry::Identity) => FullSymmetry::MirrorLeft,
            (FullSymmetry::MirrorLeft, FullSymmetry::RotateCounterClock) => FullSymmetry::MirrorUpperRight,
            (FullSymmetry::MirrorLeft, FullSymmetry::RotateClock) => FullSymmetry::MirrorLowerRight,
            (FullSymmetry::MirrorLeft, FullSymmetry::MirrorLeft) => FullSymmetry::Identity,
            (FullSymmetry::MirrorLeft, FullSymmetry::MirrorUpperRight) => FullSymmetry::RotateCounterClock,
            (FullSymmetry::MirrorLeft, FullSymmetry::MirrorLowerRight) => FullSymmetry::RotateClock,

            (FullSymmetry::MirrorUpperRight, FullSymmetry::Identity) => FullSymmetry::MirrorUpperRight,
            (FullSymmetry::MirrorUpperRight, FullSymmetry::RotateCounterClock) => FullSymmetry::MirrorLowerRight,
            (FullSymmetry::MirrorUpperRight, FullSymmetry::RotateClock) => FullSymmetry::MirrorLeft,
            (FullSymmetry::MirrorUpperRight, FullSymmetry::MirrorLeft) => FullSymmetry::RotateClock,
            (FullSymmetry::MirrorUpperRight, FullSymmetry::MirrorUpperRight) => FullSymmetry::Identity,
            (FullSymmetry::MirrorUpperRight, FullSymmetry::MirrorLowerRight) => FullSymmetry::RotateCounterClock,

            (FullSymmetry::MirrorLowerRight, FullSymmetry::Identity) => FullSymmetry::MirrorLowerRight,
            (FullSymmetry::MirrorLowerRight, FullSymmetry::RotateCounterClock) => FullSymmetry::MirrorLeft,
            (FullSymmetry::MirrorLowerRight, FullSymmetry::RotateClock) => FullSymmetry::MirrorUpperRight,
            (FullSymmetry::MirrorLowerRight, FullSymmetry::MirrorLeft) => FullSymmetry::RotateCounterClock,
            (FullSymmetry::MirrorLowerRight, FullSymmetry::MirrorUpperRight) => FullSymmetry::RotateClock,
            (FullSymmetry::MirrorLowerRight, FullSymmetry::MirrorLowerRight) => FullSymmetry::Identity,
        }
    }
}

impl functional::AssociativeOperation<FullSymmetry> for FullSymmetry { }

impl functional::Monoid<FullSymmetry> for FullSymmetry {
    fn one() -> FullSymmetry {
        FullSymmetry::Identity
    }
}

impl Invertable for FullSymmetry {
    fn invert(&self) -> FullSymmetry {
        match self {
            FullSymmetry::Identity => FullSymmetry::Identity,
            FullSymmetry::RotateCounterClock => FullSymmetry::RotateClock,
            FullSymmetry::RotateClock => FullSymmetry::RotateCounterClock,
            FullSymmetry::MirrorLeft => FullSymmetry::MirrorLeft,
            FullSymmetry::MirrorUpperRight => FullSymmetry::MirrorUpperRight,
            FullSymmetry::MirrorLowerRight => FullSymmetry::MirrorLowerRight,
        }
    }
}

impl PG for FullSymmetry {}

impl Into<ThreeTriangles> for FullSymmetry {
    fn into(self) -> ThreeTriangles {
        match self {
            FullSymmetry::Identity => ThreeTriangles([0, 1, 2, 3]),
            // 3 -> 2 -> 1
            FullSymmetry::RotateCounterClock => ThreeTriangles([0, 3, 1, 2]),
            // 1 -> 2 -> 3
            FullSymmetry::RotateClock => ThreeTriangles([0, 2, 3, 1]),
            // 2 -> 3
            FullSymmetry::MirrorLeft => ThreeTriangles([0, 1, 3, 2]),
            // 1 -> 2
            FullSymmetry::MirrorUpperRight => ThreeTriangles([0, 2, 1, 3]),
            // 1 -> 3
            FullSymmetry::MirrorLowerRight => ThreeTriangles([0, 3, 2, 1]),
        }
    }
}

impl EquivalenceClass<FullSymmetry> for ThreeTriangles {
    fn get_equivalent(self, sym: &FullSymmetry) -> ThreeTriangles {
        let x: ThreeTriangles = sym.clone().into();
        x.invert().permute(self).permute(x)
    }
}

impl EquivalenceClass<FullSymmetry> for Turns {
    fn get_equivalent(self, sym: &FullSymmetry) -> Turns {
        match (sym, self) {
            (FullSymmetry::Identity, Turns::Left) => Turns::Left,
            (FullSymmetry::Identity, Turns::LeftPrime) => Turns::LeftPrime,
            (FullSymmetry::Identity, Turns::UpperRight) => Turns::UpperRight,
            (FullSymmetry::Identity, Turns::UpperRightPrime) => Turns::UpperRightPrime,
            (FullSymmetry::Identity, Turns::LowerRight) => Turns::LowerRight,
            (FullSymmetry::Identity, Turns::LowerRightPrime) => Turns::LowerRightPrime,

            (FullSymmetry::RotateCounterClock, Turns::Left) => Turns::LowerRight,
            (FullSymmetry::RotateCounterClock, Turns::LeftPrime) => Turns::LowerRightPrime,
            (FullSymmetry::RotateCounterClock, Turns::UpperRight) => Turns::Left,
            (FullSymmetry::RotateCounterClock, Turns::UpperRightPrime) => Turns::LeftPrime,
            (FullSymmetry::RotateCounterClock, Turns::LowerRight) => Turns::UpperRight,
            (FullSymmetry::RotateCounterClock, Turns::LowerRightPrime) => Turns::UpperRightPrime,

            (FullSymmetry::RotateClock, Turns::Left) => Turns::UpperRight,
            (FullSymmetry::RotateClock, Turns::LeftPrime) => Turns::UpperRightPrime,
            (FullSymmetry::RotateClock, Turns::UpperRight) => Turns::LowerRight,
            (FullSymmetry::RotateClock, Turns::UpperRightPrime) => Turns::LowerRightPrime,
            (FullSymmetry::RotateClock, Turns::LowerRight) => Turns::Left,
            (FullSymmetry::RotateClock, Turns::LowerRightPrime) => Turns::LeftPrime,

            (FullSymmetry::MirrorLeft, Turns::Left) => Turns::LeftPrime,
            (FullSymmetry::MirrorLeft, Turns::LeftPrime) => Turns::Left,
            (FullSymmetry::MirrorLeft, Turns::UpperRight) => Turns::LowerRightPrime,
            (FullSymmetry::MirrorLeft, Turns::UpperRightPrime) => Turns::LowerRight,
            (FullSymmetry::MirrorLeft, Turns::LowerRight) => Turns::UpperRightPrime,
            (FullSymmetry::MirrorLeft, Turns::LowerRightPrime) => Turns::UpperRight,

            (FullSymmetry::MirrorUpperRight, Turns::Left) => Turns::LowerRightPrime,
            (FullSymmetry::MirrorUpperRight, Turns::LeftPrime) => Turns::LowerRight,
            (FullSymmetry::MirrorUpperRight, Turns::UpperRight) => Turns::UpperRightPrime,
            (FullSymmetry::MirrorUpperRight, Turns::UpperRightPrime) => Turns::UpperRight,
            (FullSymmetry::MirrorUpperRight, Turns::LowerRight) => Turns::LeftPrime,
            (FullSymmetry::MirrorUpperRight, Turns::LowerRightPrime) => Turns::Left,

            (FullSymmetry::MirrorLowerRight, Turns::Left) => Turns::UpperRightPrime,
            (FullSymmetry::MirrorLowerRight, Turns::LeftPrime) => Turns::UpperRight,
            (FullSymmetry::MirrorLowerRight, Turns::UpperRight) => Turns::LeftPrime,
            (FullSymmetry::MirrorLowerRight, Turns::UpperRightPrime) => Turns::Left,
            (FullSymmetry::MirrorLowerRight, Turns::LowerRight) => Turns::LowerRightPrime,
            (FullSymmetry::MirrorLowerRight, Turns::LowerRightPrime) => Turns::LowerRight,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash, Sequence)]
#[repr(u8)]
pub enum NoSymmetry {
    Identity,
}

impl Into<usize> for NoSymmetry {
    fn into(self) -> usize {
        self as usize
    }
}

impl functional::BinaryOperation<NoSymmetry> for NoSymmetry {
    fn apply(_: NoSymmetry, _: NoSymmetry) -> NoSymmetry {
        NoSymmetry::Identity
    }
}

impl functional::AssociativeOperation<NoSymmetry> for NoSymmetry { }

impl functional::Monoid<NoSymmetry> for NoSymmetry {
    fn one() -> NoSymmetry {
        NoSymmetry::Identity
    }
}

impl Invertable for NoSymmetry {
    fn invert(&self) -> NoSymmetry {
        NoSymmetry::Identity
    }
}

impl PG for NoSymmetry {}

impl Into<FullSymmetry> for NoSymmetry {
    fn into(self) -> FullSymmetry {
        match self {
            NoSymmetry::Identity => FullSymmetry::Identity,
        }
    }
}

impl Into<ThreeTriangles> for NoSymmetry {
    fn into(self) -> ThreeTriangles {
        let full: FullSymmetry = self.into();
        full.into()
    }
}

impl EquivalenceClass<NoSymmetry> for ThreeTriangles {
    fn get_equivalent(self, sym: &NoSymmetry) -> ThreeTriangles {
        let x: ThreeTriangles = sym.clone().into();
        x.invert().permute(self).permute(x)
    }
}

impl EquivalenceClass<NoSymmetry> for Turns {
    fn get_equivalent(self, sym: &NoSymmetry) -> Turns {
        let full: FullSymmetry = (*sym).into();
        self.get_equivalent(&full)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash, Sequence)]
#[repr(u8)]
pub enum MirrorUDSymmetry {
    Identity,
    Mirror,
}

impl Into<usize> for MirrorUDSymmetry {
    fn into(self) -> usize {
        self as usize
    }
}

impl functional::BinaryOperation<MirrorUDSymmetry> for MirrorUDSymmetry {
    fn apply(a: MirrorUDSymmetry, b: MirrorUDSymmetry) -> MirrorUDSymmetry {
        match (a, b) {
            (MirrorUDSymmetry::Identity, MirrorUDSymmetry::Identity) => MirrorUDSymmetry::Identity,
            (MirrorUDSymmetry::Identity, MirrorUDSymmetry::Mirror) => MirrorUDSymmetry::Mirror,
            (MirrorUDSymmetry::Mirror, MirrorUDSymmetry::Identity) => MirrorUDSymmetry::Mirror,
            (MirrorUDSymmetry::Mirror, MirrorUDSymmetry::Mirror) => MirrorUDSymmetry::Identity,
        }
    }
}

impl functional::AssociativeOperation<MirrorUDSymmetry> for MirrorUDSymmetry { }

impl functional::Monoid<MirrorUDSymmetry> for MirrorUDSymmetry {
    fn one() -> MirrorUDSymmetry {
        MirrorUDSymmetry::Identity
    }
}

impl Invertable for MirrorUDSymmetry {
    fn invert(&self) -> MirrorUDSymmetry {
        match self {
            MirrorUDSymmetry::Identity => MirrorUDSymmetry::Identity,
            MirrorUDSymmetry::Mirror => MirrorUDSymmetry::Mirror,
        }
    }
}

impl PG for MirrorUDSymmetry {}

impl Into<FullSymmetry> for MirrorUDSymmetry {
    fn into(self) -> FullSymmetry {
        match self {
            MirrorUDSymmetry::Identity => FullSymmetry::Identity,
            MirrorUDSymmetry::Mirror => FullSymmetry::MirrorLeft,
        }
    }
}

impl Into<ThreeTriangles> for MirrorUDSymmetry {
    fn into(self) -> ThreeTriangles {
        let full: FullSymmetry = self.into();
        full.into()
    }
}

impl EquivalenceClass<MirrorUDSymmetry> for ThreeTriangles {
    fn get_equivalent(self, sym: &MirrorUDSymmetry) -> ThreeTriangles {
        let x: ThreeTriangles = sym.clone().into();
        x.invert().permute(self).permute(x)
    }
}

impl EquivalenceClass<MirrorUDSymmetry> for Turns {
    fn get_equivalent(self, sym: &MirrorUDSymmetry) -> Turns {
        let full: FullSymmetry = (*sym).into();
        self.get_equivalent(&full)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash, Sequence)]
#[repr(u8)]
pub enum RotationalSymmetry {
    Identity,
    RotateCounterClock,
    RotateClock,
}

impl Into<usize> for RotationalSymmetry {
    fn into(self) -> usize {
        self as usize
    }
}

impl functional::BinaryOperation<RotationalSymmetry> for RotationalSymmetry {
    fn apply(a: RotationalSymmetry, b: RotationalSymmetry) -> RotationalSymmetry {
        match (a, b) {
            (RotationalSymmetry::Identity, RotationalSymmetry::Identity) => RotationalSymmetry::Identity,
            (RotationalSymmetry::Identity, RotationalSymmetry::RotateCounterClock) => RotationalSymmetry::RotateCounterClock,
            (RotationalSymmetry::Identity, RotationalSymmetry::RotateClock) => RotationalSymmetry::RotateClock,

            (RotationalSymmetry::RotateCounterClock, RotationalSymmetry::Identity) => RotationalSymmetry::RotateCounterClock,
            (RotationalSymmetry::RotateCounterClock, RotationalSymmetry::RotateCounterClock) => RotationalSymmetry::RotateClock,
            (RotationalSymmetry::RotateCounterClock, RotationalSymmetry::RotateClock) => RotationalSymmetry::Identity,

            (RotationalSymmetry::RotateClock, RotationalSymmetry::Identity) => RotationalSymmetry::RotateClock,
            (RotationalSymmetry::RotateClock, RotationalSymmetry::RotateCounterClock) => RotationalSymmetry::Identity,
            (RotationalSymmetry::RotateClock, RotationalSymmetry::RotateClock) => RotationalSymmetry::RotateCounterClock,
        }
    }
}

impl functional::AssociativeOperation<RotationalSymmetry> for RotationalSymmetry { }

impl functional::Monoid<RotationalSymmetry> for RotationalSymmetry {
    fn one() -> RotationalSymmetry {
        RotationalSymmetry::Identity
    }
}

impl Invertable for RotationalSymmetry {
    fn invert(&self) -> RotationalSymmetry {
        match self {
            RotationalSymmetry::Identity => RotationalSymmetry::Identity,
            RotationalSymmetry::RotateCounterClock => RotationalSymmetry::RotateClock,
            RotationalSymmetry::RotateClock => RotationalSymmetry::RotateCounterClock,
        }
    }
}

impl PG for RotationalSymmetry {}

impl Into<FullSymmetry> for RotationalSymmetry {
    fn into(self) -> FullSymmetry {
        match self {
            RotationalSymmetry::Identity => FullSymmetry::Identity,
            RotationalSymmetry::RotateCounterClock => FullSymmetry::RotateCounterClock,
            RotationalSymmetry::RotateClock => FullSymmetry::RotateClock,
        }
    }
}

impl Into<ThreeTriangles> for RotationalSymmetry {
    fn into(self) -> ThreeTriangles {
        let full: FullSymmetry = self.into();
        full.into()
    }
}

impl EquivalenceClass<RotationalSymmetry> for ThreeTriangles {
    fn get_equivalent(self, sym: &RotationalSymmetry) -> ThreeTriangles {
        let x: ThreeTriangles = sym.clone().into();
        x.invert().permute(self).permute(x)
    }
}

impl EquivalenceClass<RotationalSymmetry> for Turns {
    fn get_equivalent(self, sym: &RotationalSymmetry) -> Turns {
        let full: FullSymmetry = (*sym).into();
        self.get_equivalent(&full)
    }
}

// TODO: There are faster algorithms than this
fn to_lehmer(p: ThreeTriangles) -> u8 {
    let mut x = p.0.clone();
    for i in 0..4 {
        for j in (i+1)..4 {
            if x[j] > x[i] {
                x[j] -= 1;
            }
        }
    }
    x[0] + 4 * (x[1] + 3 * (x[2] + 2 * x[3]))
}

// TODO: There are faster algorithms than this
fn from_lehmer(i: u8) -> ThreeTriangles {
    let mut i = i;
    let mut x = [0; 4];
    for j in 0..4 {
        x[j as usize] = i % (4 - j);
        i = i / (4 - j);
    }
    for i in (0..4).rev() {
        for j in (i+1)..4 {
            if x[j as usize] >= x[i] {
                x[j as usize] += 1;
            }
        }
    }
    ThreeTriangles(x)
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
pub struct ThreeTrianglesIndex(u8);

impl Sequence for ThreeTrianglesIndex {
    const CARDINALITY: usize = 24;

    fn next(&self) -> Option<Self> {
        if self.0 == (Self::CARDINALITY - 1) as u8 {
            None
        } else {
            Some(ThreeTrianglesIndex(self.0 + 1))
        }
    }

    fn previous(&self) -> Option<Self> {
        if self.0 == 0 {
            None
        } else {
            Some(ThreeTrianglesIndex(self.0 - 1))
        }
    }

    fn first() -> Option<Self> {
        Some(ThreeTrianglesIndex(0))
    }

    fn last() -> Option<Self> {
        Some(ThreeTrianglesIndex((Self::CARDINALITY - 1) as u8))
    }
}

impl Into<ThreeTriangles> for ThreeTrianglesIndex {
    fn into(self) -> ThreeTriangles {
        from_lehmer(self.0)
    }
}

impl Into<usize> for ThreeTrianglesIndex {
    fn into(self) -> usize {
        self.0 as usize
    }
}

impl TryFrom<usize> for ThreeTrianglesIndex {
    type Error = std::num::TryFromIntError;

    fn try_from(i: usize) -> Result<Self, Self::Error> {
        let j = i.try_into()?;
        Ok(ThreeTrianglesIndex(j))
    }
}

impl Into<ThreeTrianglesIndex> for ThreeTriangles {
    fn into(self) -> ThreeTrianglesIndex {
        ThreeTrianglesIndex(to_lehmer(self))
    }
}

// TODO: There are faster algorithms than this
fn to_lehmer_even(p: ThreeTriangles) -> u8 {
    let mut x = p.0.clone();
    for i in 0..4 {
        for j in (i+1)..4 {
            if x[j] > x[i] {
                x[j] -= 1;
            }
        }
    }
    x[0] + 4 * x[1]
}

// TODO: There are faster algorithms than this
fn from_lehmer_even(i: u8) -> ThreeTriangles {
    let mut i = i;
    let mut x = [0; 4];
    for j in 0..2 {
        x[j as usize] = i % (4 - j);
        i = i / (4 - j);
    }
    for i in (0..4).rev() {
        for j in (i+1)..4 {
            if x[j as usize] >= x[i] {
                x[j as usize] += 1;
            }
        }
    }
    if !is_even_parity(x) {
        let tmp = x[2];
        x[2] = x[3];
        x[3] = tmp;
    }
    ThreeTriangles(x)
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
pub struct ThreeTrianglesEvenIndex(u8);

impl Sequence for ThreeTrianglesEvenIndex {
    const CARDINALITY: usize = 12;

    fn next(&self) -> Option<Self> {
        if self.0 == (Self::CARDINALITY - 1) as u8 {
            None
        } else {
            Some(ThreeTrianglesEvenIndex(self.0 + 1))
        }
    }

    fn previous(&self) -> Option<Self> {
        if self.0 == 0 {
            None
        } else {
            Some(ThreeTrianglesEvenIndex(self.0 - 1))
        }
    }

    fn first() -> Option<Self> {
        Some(ThreeTrianglesEvenIndex(0))
    }

    fn last() -> Option<Self> {
        Some(ThreeTrianglesEvenIndex((Self::CARDINALITY - 1) as u8))
    }
}

impl Into<ThreeTriangles> for ThreeTrianglesEvenIndex {
    fn into(self) -> ThreeTriangles {
        from_lehmer_even(self.0)
    }
}

impl Into<usize> for ThreeTrianglesEvenIndex {
    fn into(self) -> usize {
        self.0 as usize
    }
}

impl TryFrom<usize> for ThreeTrianglesEvenIndex {
    type Error = std::num::TryFromIntError;

    fn try_from(i: usize) -> Result<Self, Self::Error> {
        let j = i.try_into()?;
        Ok(ThreeTrianglesEvenIndex(j))
    }
}

// TODO: This technically can fail.  We don't know that an odd permutation
// wasn't applied.  I have no idea how to handle this though (it should at
// least panic).
impl Into<ThreeTrianglesEvenIndex> for ThreeTriangles {
    fn into(self) -> ThreeTrianglesEvenIndex {
        ThreeTrianglesEvenIndex(to_lehmer_even(self))
    }
}

pub fn moves_to_solve() -> BTreeMap<ThreeTrianglesEvenIndex, usize> {
    let mut queue = VecDeque::new();
    let mut map = BTreeMap::new();

    map.insert(ThreeTriangles::identity().into(), 0);
    queue.push_back((ThreeTriangles::identity(), 1));

    loop {
        match queue.pop_front() {
            None => break,
            Some((p, count)) => {
                for t in all::<Turns>() {
                    let turned = p.clone().permute(t.into());
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
    use enum_iterator::all;

    #[test]
    fn all_odd_lehmer_codes_round_trip() {
        for i in 0..24u8 {
            let t: ThreeTriangles = from_lehmer(i);
            assert_eq!(i, to_lehmer(t));
        }
    }

    #[test]
    fn all_even_lehmer_codes_round_trip() {
        for i in 0..12u8 {
            let t: ThreeTriangles = from_lehmer_even(i);
            assert_eq!(i, to_lehmer_even(t));
        }
    }

    #[test]
    fn half_of_parities_are_even() {
        let mut even_count = 0;
        let mut count = 0;
        for i in all::<ThreeTrianglesIndex>() {
            let t: ThreeTriangles = i.into();
            if is_even_parity(t.0) {
                even_count += 1;
            }
            count += 1;
        }
        assert_eq!(count, 24);
        assert_eq!(even_count, 12);
    }

    #[test]
    fn all_even_parities_are_even() {
        let mut even_count = 0;
        let mut count = 0;
        for i in all::<ThreeTrianglesEvenIndex>() {
            let t: ThreeTriangles = i.into();
            if is_even_parity(t.0) {
                even_count += 1;
            }
            count += 1;
        }
        assert_eq!(even_count, 12);
        assert_eq!(count, 12);
    }

    #[test]
    fn one_swap_is_odd() {
        let t = ThreeTriangles([1, 0, 2, 3]);
        assert_eq!(is_even_parity(t.0), false);
    }

    #[test]
    fn two_independent_swaps_are_even() {
        let t = ThreeTriangles([1, 0, 3, 2]);
        assert_eq!(is_even_parity(t.0), true);
    }

    #[test]
    fn four_cycle_is_odd() {
        let t = ThreeTriangles([1, 2, 3, 0]);
        assert_eq!(is_even_parity(t.0), false);
    }

    #[test]
    fn perm_and_turns_and_sym_invert_round_trips() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for pi in all::<ThreeTrianglesIndex>() {
            let p: ThreeTriangles = pi.into();
            assert_eq!(p, p.invert().invert());

            for t in all::<Turns>() {
                assert_eq!(p, p.permute(t.into()).permute(t.invert().into()));
            }

            for s in all::<FullSymmetry>() {
                assert_eq!(p, p.permute(s.into()).permute(s.invert().into()));
            }

            for s in all::<MirrorUDSymmetry>() {
                assert_eq!(p, p.permute(s.into()).permute(s.invert().into()));
            }

            for s in all::<RotationalSymmetry>() {
                assert_eq!(p, p.permute(s.into()).permute(s.invert().into()));
            }

            for s in all::<FullSymmetry>() {
                assert_eq!(p, p.permute(s.into()).permute(s.invert().into()));
            }
        }
    }

    #[test]
    fn perm_and_turn_no_symmetries_are_equivalent() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s in all::<NoSymmetry>() {
            for pi in all::<ThreeTrianglesIndex>() {
                let p: ThreeTriangles = pi.into();
                for t in all::<Turns>() {
                    let after_permute = p.permute(t.into()).get_equivalent(&s);
                    let before_permute = p.get_equivalent(&s).permute(t.get_equivalent(&s).into());
                    assert_eq!(after_permute, before_permute)
                }
            }
        }
    }

    #[test]
    fn perm_and_turn_mirror_ud_symmetries_are_equivalent() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s in all::<MirrorUDSymmetry>() {
            for pi in all::<ThreeTrianglesIndex>() {
                let p: ThreeTriangles = pi.into();
                for t in all::<Turns>() {
                    let after_permute = p.permute(t.into()).get_equivalent(&s);
                    let before_permute = p.get_equivalent(&s).permute(t.get_equivalent(&s).into());
                    if after_permute != before_permute {
                        println!("{:?} - {:?} - {:?}", s, p, t);
                    }
                    assert_eq!(after_permute, before_permute)
                }
            }
        }
    }

    #[test]
    fn perm_and_turn_rotational_symmetries_are_equivalent() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s in all::<RotationalSymmetry>() {
            for pi in all::<ThreeTrianglesIndex>() {
                let p: ThreeTriangles = pi.into();
                for t in all::<Turns>() {
                    let after_permute = p.permute(t.into()).get_equivalent(&s);
                    let before_permute = p.get_equivalent(&s).permute(t.get_equivalent(&s).into());
                    if after_permute != before_permute {
                        println!("{:?} - {:?} - {:?}", s, p, t);
                    }
                    assert_eq!(after_permute, before_permute)
                }
            }
        }
    }

    #[test]
    fn perm_and_turn_full_symmetries_are_equivalent() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s in all::<FullSymmetry>() {
            for pi in all::<ThreeTrianglesIndex>() {
                let p: ThreeTriangles = pi.into();
                for t in all::<Turns>() {
                    let after_permute = p.permute(t.into()).get_equivalent(&s);
                    let before_permute = p.get_equivalent(&s).permute(t.get_equivalent(&s).into());
                    assert_eq!(after_permute, before_permute)
                }
            }
        }
    }

    #[test]
    fn direct_and_indirect_sym_multiplication_are_equivalent_for_full_symmetry() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s0 in all::<FullSymmetry>() {
            for s1 in all::<FullSymmetry>() {
                for pi in all::<ThreeTrianglesIndex>() {
                    let p: ThreeTriangles = pi.into();
                    let as_tt = p.permute(s0.into()).permute(s1.into());
                    let as_sym = p.permute(s0.permute(s1).into());
                    assert_eq!(as_tt, as_sym);
                }
            }
        }
    }

    #[test]
    fn direct_and_indirect_sym_multiplication_are_equivalent_for_mirror_ud_symmetry() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s0 in all::<MirrorUDSymmetry>() {
            for s1 in all::<MirrorUDSymmetry>() {
                for pi in all::<ThreeTrianglesIndex>() {
                    let p: ThreeTriangles = pi.into();
                    let as_tt = p.permute(s0.into()).permute(s1.into());
                    let as_sym = p.permute(s0.permute(s1).into());
                    assert_eq!(as_tt, as_sym);
                }
            }
        }
    }

    #[test]
    fn direct_and_indirect_sym_multiplication_are_equivalent_for_rotational_symmetry() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s0 in all::<RotationalSymmetry>() {
            for s1 in all::<RotationalSymmetry>() {
                for pi in all::<ThreeTrianglesIndex>() {
                    let p: ThreeTriangles = pi.into();
                    let as_tt = p.permute(s0.into()).permute(s1.into());
                    let as_sym = p.permute(s0.permute(s1).into());
                    assert_eq!(as_tt, as_sym);
                }
            }
        }
    }

    #[test]
    fn direct_and_indirect_sym_multiplication_are_equivalent_for_no_symmetry() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s0 in all::<NoSymmetry>() {
            for s1 in all::<NoSymmetry>() {
                for pi in all::<ThreeTrianglesIndex>() {
                    let p: ThreeTriangles = pi.into();
                    let as_tt = p.permute(s0.into()).permute(s1.into());
                    let as_sym = p.permute(s0.permute(s1).into());
                    assert_eq!(as_tt, as_sym);
                }
            }
        }
    }
}
