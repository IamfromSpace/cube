use permutation_group::PermutationGroup as PG;
use invertable::Invertable;
use equivalence_class::EquivalenceClass;

use std::collections::BTreeMap;
use std::collections::VecDeque;
use std::convert::{TryInto, TryFrom};
use enum_iterator::{all, Sequence};

pub mod inner;
pub mod outer;

// We have a simple little puzzle with three trapezoidal "faces" that all share
// a middle and each share an edge.  we can rotate either clockwise or
// counter-clockwise.  Even though just clockwise turns generate all reachable
// states, we get more symmetries if we include clockwise turns (because the
// turns must also follow the symmetries).
//
//  4
//
//    1
//       0  3
//    2
//
//  5
//
// A clockwise turn of the upper-right face yields:
//
//  1
//
//    0
//       3  4
//    2
//
//  5
//
// We consider symmetry through mirroring top-down and rotation by 120 degrees.
// Combination of the two allows mirroring along any face.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ThreeTrapezoids([u16; 6]);

const fn permute_arr(a: &[u16; 6], b: &[u16; 6]) -> [u16; 6] {
    [
        a[b[0] as usize],
        a[b[1] as usize],
        a[b[2] as usize],
        a[b[3] as usize],
        a[b[4] as usize],
        a[b[5] as usize],
    ]
}

const fn arr_inv(a: &[u16; 6]) -> [u16; 6] {
    let mut r = [0; 6];
    r[a[0] as usize] = 0;
    r[a[1] as usize] = 1;
    r[a[2] as usize] = 2;
    r[a[3] as usize] = 3;
    r[a[4] as usize] = 4;
    r[a[5] as usize] = 5;
    r
}

impl functional::BinaryOperation<ThreeTrapezoids> for ThreeTrapezoids {
    fn apply(a: ThreeTrapezoids, b: ThreeTrapezoids) -> ThreeTrapezoids {
        ThreeTrapezoids(permute_arr(&a.0, &b.0))
    }
}

impl functional::AssociativeOperation<ThreeTrapezoids> for ThreeTrapezoids { }

impl functional::Monoid<ThreeTrapezoids> for ThreeTrapezoids {
    fn one() -> ThreeTrapezoids {
        ThreeTrapezoids([0, 1, 2, 3, 4, 5])
    }
}

impl Invertable for ThreeTrapezoids {
    fn invert(&self) -> ThreeTrapezoids {
        ThreeTrapezoids(arr_inv(&self.0))
    }
}

impl PG for ThreeTrapezoids {}

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

impl Into<ThreeTrapezoids> for Turns {
    fn into(self) -> ThreeTrapezoids {
        match self {
            // 1 -> 4 -> 5 -> 2
            Turns::Left => ThreeTrapezoids([0, 4, 1, 3, 5, 2]),
            Turns::LeftPrime => ThreeTrapezoids([0, 2, 5, 3, 1, 4]),
            // 0 -> 3 -> 4 -> 1
            Turns::UpperRight => ThreeTrapezoids([3, 0, 2, 4, 1, 5]),
            Turns::UpperRightPrime => ThreeTrapezoids([1, 4, 2, 0, 3, 5]),
            // 0 -> 2 -> 5 -> 3
            Turns::LowerRight => ThreeTrapezoids([2, 1, 5, 0, 4, 3]),
            Turns::LowerRightPrime => ThreeTrapezoids([3, 1, 0, 5, 4, 2]),
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

impl Into<ThreeTrapezoids> for FullSymmetry {
    fn into(self) -> ThreeTrapezoids {
        match self {
            FullSymmetry::Identity => ThreeTrapezoids([0, 1, 2, 3, 4, 5]),
            FullSymmetry::RotateCounterClock => ThreeTrapezoids([2, 0, 1, 5, 3, 4]),
            // 0 -> 1 -> 2
            // 3 -> 4 -> 5
            FullSymmetry::RotateClock => ThreeTrapezoids([1, 2, 0, 4, 5, 3]),
            // 4 -> 5
            // 1 -> 2
            FullSymmetry::MirrorLeft => ThreeTrapezoids([0, 2, 1, 3, 5, 4]),
            // 0 -> 1
            // 3 -> 4
            FullSymmetry::MirrorUpperRight => ThreeTrapezoids([1, 0, 2, 4, 3, 5]),
            // 0 -> 2
            // 3 -> 5
            FullSymmetry::MirrorLowerRight => ThreeTrapezoids([2, 1, 0, 5, 4, 3]),
        }
    }
}

impl EquivalenceClass<FullSymmetry> for ThreeTrapezoids {
    fn get_equivalent(self, sym: &FullSymmetry) -> ThreeTrapezoids {
        let x: ThreeTrapezoids = sym.clone().into();
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

impl Into<ThreeTrapezoids> for NoSymmetry {
    fn into(self) -> ThreeTrapezoids {
        let full: FullSymmetry = self.into();
        full.into()
    }
}

impl EquivalenceClass<NoSymmetry> for ThreeTrapezoids {
    fn get_equivalent(self, sym: &NoSymmetry) -> ThreeTrapezoids {
        let x: ThreeTrapezoids = sym.clone().into();
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

impl Into<ThreeTrapezoids> for MirrorUDSymmetry {
    fn into(self) -> ThreeTrapezoids {
        let full: FullSymmetry = self.into();
        full.into()
    }
}

impl EquivalenceClass<MirrorUDSymmetry> for ThreeTrapezoids {
    fn get_equivalent(self, sym: &MirrorUDSymmetry) -> ThreeTrapezoids {
        let x: ThreeTrapezoids = sym.clone().into();
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

impl Into<ThreeTrapezoids> for RotationalSymmetry {
    fn into(self) -> ThreeTrapezoids {
        let full: FullSymmetry = self.into();
        full.into()
    }
}

impl EquivalenceClass<RotationalSymmetry> for ThreeTrapezoids {
    fn get_equivalent(self, sym: &RotationalSymmetry) -> ThreeTrapezoids {
        let x: ThreeTrapezoids = sym.clone().into();
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
fn to_lehmer(p: ThreeTrapezoids) -> u16 {
    let mut x = p.0.clone();
    for i in 0..6 {
        for j in (i+1)..6 {
            if x[j] > x[i] {
                x[j] -= 1;
            }
        }
    }
    x[0] + 6  * (x[1] + 5 * (x[2] + 4 * (x[3] + 3 * (x[4] + 2 * x[5]))))
}

// TODO: There are faster algorithms than this
fn from_lehmer(i: u16) -> ThreeTrapezoids {
    let mut i = i;
    let mut x = [0; 6];
    for j in 0..6 {
        x[j as usize] = i % (6 - j);
        i = i / (6 - j);
    }
    for i in (0..6).rev() {
        for j in (i+1)..6 {
            if x[j as usize] >= x[i] {
                x[j as usize] += 1;
            }
        }
    }
    ThreeTrapezoids(x)
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
pub struct ThreeTrapezoidsIndex(u16);

impl Sequence for ThreeTrapezoidsIndex {
    const CARDINALITY: usize = 720;

    fn next(&self) -> Option<Self> {
        if self.0 == (Self::CARDINALITY - 1) as u16 {
            None
        } else {
            Some(ThreeTrapezoidsIndex(self.0 + 1))
        }
    }

    fn previous(&self) -> Option<Self> {
        if self.0 == 0 {
            None
        } else {
            Some(ThreeTrapezoidsIndex(self.0 - 1))
        }
    }

    fn first() -> Option<Self> {
        Some(ThreeTrapezoidsIndex(0))
    }

    fn last() -> Option<Self> {
        Some(ThreeTrapezoidsIndex((Self::CARDINALITY - 1) as u16))
    }
}

impl Into<ThreeTrapezoids> for ThreeTrapezoidsIndex {
    fn into(self) -> ThreeTrapezoids {
        from_lehmer(self.0)
    }
}

impl Into<usize> for ThreeTrapezoidsIndex {
    fn into(self) -> usize {
        self.0 as usize
    }
}

impl TryFrom<usize> for ThreeTrapezoidsIndex {
    type Error = std::num::TryFromIntError;

    fn try_from(i: usize) -> Result<Self, Self::Error> {
        let j = i.try_into()?;
        Ok(ThreeTrapezoidsIndex(j))
    }
}

impl Into<ThreeTrapezoidsIndex> for ThreeTrapezoids {
    fn into(self) -> ThreeTrapezoidsIndex {
        ThreeTrapezoidsIndex(to_lehmer(self))
    }
}

pub fn moves_to_solve() -> BTreeMap<ThreeTrapezoidsIndex, usize> {
    let mut queue = VecDeque::new();
    let mut map = BTreeMap::new();

    map.insert(ThreeTrapezoids::identity().into(), 0);
    queue.push_back((ThreeTrapezoids::identity(), 1));

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
    use enum_iterator::{all, cardinality};
    use quickcheck::Gen;
    use rand::Rng;

    impl quickcheck::Arbitrary for ThreeTrapezoidsIndex {
        fn arbitrary<G: Gen>(g: &mut G) -> ThreeTrapezoidsIndex {
            ThreeTrapezoidsIndex(*g.choose(&(0..cardinality::<ThreeTrapezoidsIndex>() as u16).collect::<Vec<_>>()).unwrap())
        }
    }

    #[test]
    fn all_odd_lehmer_codes_round_trip() {
        for i in 0..720u16 {
            let t: ThreeTrapezoids = from_lehmer(i);
            assert_eq!(i, to_lehmer(t));
        }
    }

    // Even thugh this puzzle is quite small, it's still too big for exhaustive checking
    quickcheck! {
        fn permutation_is_associative(pi_0: ThreeTrapezoidsIndex, pi_1: ThreeTrapezoidsIndex, pi_2: ThreeTrapezoidsIndex) -> bool {
            let p_0: ThreeTrapezoids = pi_0.into();
            let p_1: ThreeTrapezoids = pi_1.into();
            let p_2: ThreeTrapezoids = pi_2.into();
            p_0.permute(p_1).permute(p_2) == p_0.permute(p_1.permute(p_2))
        }
    }

    #[test]
    fn perm_and_turns_and_sym_invert_round_trips() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for pi in all::<ThreeTrapezoidsIndex>() {
            let p: ThreeTrapezoids = pi.into();
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
            for pi in all::<ThreeTrapezoidsIndex>() {
                let p: ThreeTrapezoids = pi.into();
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
            for pi in all::<ThreeTrapezoidsIndex>() {
                let p: ThreeTrapezoids = pi.into();
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
            for pi in all::<ThreeTrapezoidsIndex>() {
                let p: ThreeTrapezoids = pi.into();
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
            for pi in all::<ThreeTrapezoidsIndex>() {
                let p: ThreeTrapezoids = pi.into();
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
                for pi in all::<ThreeTrapezoidsIndex>() {
                    let p: ThreeTrapezoids = pi.into();
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
                for pi in all::<ThreeTrapezoidsIndex>() {
                    let p: ThreeTrapezoids = pi.into();
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
                for pi in all::<ThreeTrapezoidsIndex>() {
                    let p: ThreeTrapezoids = pi.into();
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
                for pi in all::<ThreeTrapezoidsIndex>() {
                    let p: ThreeTrapezoids = pi.into();
                    let as_tt = p.permute(s0.into()).permute(s1.into());
                    let as_sym = p.permute(s0.permute(s1).into());
                    assert_eq!(as_tt, as_sym);
                }
            }
        }
    }
}
