use permutation_group::PermutationGroup as PG;
use invertable::Invertable;
use equivalence_class::EquivalenceClass;
use three_triangles;

use std::collections::BTreeMap;
use std::collections::VecDeque;
use std::convert::{TryInto, TryFrom};
use enum_iterator::{all, Sequence};

// We have a simple puzzle that's just a stack of two three triangles puzzles.
// We can move the top layer independently or both layers simlutaneously.
//
// 6
//   4 5
// 7
//
// 2
//   0 1
// 3
//
// While there are 576 hypothetical permutations, only 144 of them are
// reachable by moves, because turns always perform an even number of
// swaps--odd permutations are not representable.
//
// We consider symmetry through mirroring top-down and rotation by 120 degrees.
// Combination of the two allows mirroring along any face.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct TopThreeTriangles(three_triangles::ThreeTriangles);

impl functional::BinaryOperation<TopThreeTriangles> for TopThreeTriangles {
    fn apply(a: TopThreeTriangles, b: TopThreeTriangles) -> TopThreeTriangles {
        TopThreeTriangles(three_triangles::ThreeTriangles::apply(a.0, b.0))
    }
}

impl functional::AssociativeOperation<TopThreeTriangles> for TopThreeTriangles { }

impl functional::Monoid<TopThreeTriangles> for TopThreeTriangles {
    fn one() -> TopThreeTriangles {
        TopThreeTriangles(three_triangles::ThreeTriangles::one())
    }
}

impl Invertable for TopThreeTriangles {
    fn invert(&self) -> TopThreeTriangles {
        TopThreeTriangles(self.0.invert())
    }
}

impl PG for TopThreeTriangles {}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct BottomThreeTriangles(three_triangles::ThreeTriangles);

impl functional::BinaryOperation<BottomThreeTriangles> for BottomThreeTriangles {
    fn apply(a: BottomThreeTriangles, b: BottomThreeTriangles) -> BottomThreeTriangles {
        BottomThreeTriangles(three_triangles::ThreeTriangles::apply(a.0, b.0))
    }
}

impl functional::AssociativeOperation<BottomThreeTriangles> for BottomThreeTriangles { }

impl functional::Monoid<BottomThreeTriangles> for BottomThreeTriangles {
    fn one() -> BottomThreeTriangles {
        BottomThreeTriangles(three_triangles::ThreeTriangles::one())
    }
}

impl Invertable for BottomThreeTriangles {
    fn invert(&self) -> BottomThreeTriangles {
        BottomThreeTriangles(self.0.invert())
    }
}

impl PG for BottomThreeTriangles {}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash, Sequence)]
#[repr(u8)]
pub enum Turns {
    TopLeft,
    // Primes are always counter clockwise
    TopLeftPrime,
    TopUpperRight,
    TopUpperRightPrime,
    TopLowerRight,
    TopLowerRightPrime,
    BothLeft,
    // Primes are always counter clockwise
    BothLeftPrime,
    BothUpperRight,
    BothUpperRightPrime,
    BothLowerRight,
    BothLowerRightPrime,
}

impl Invertable for Turns {
    fn invert(&self) -> Turns {
        match self {
            Turns::TopLeft => Turns::TopLeftPrime,
            Turns::TopLeftPrime => Turns::TopLeft,
            Turns::TopUpperRight => Turns::TopUpperRightPrime,
            Turns::TopUpperRightPrime => Turns::TopUpperRight,
            Turns::TopLowerRight => Turns::TopLowerRightPrime,
            Turns::TopLowerRightPrime => Turns::TopLowerRight,
            Turns::BothLeft => Turns::BothLeftPrime,
            Turns::BothLeftPrime => Turns::BothLeft,
            Turns::BothUpperRight => Turns::BothUpperRightPrime,
            Turns::BothUpperRightPrime => Turns::BothUpperRight,
            Turns::BothLowerRight => Turns::BothLowerRightPrime,
            Turns::BothLowerRightPrime => Turns::BothLowerRight,
        }
    }
}

impl Into<usize> for Turns {
    fn into(self) -> usize {
        self as usize
    }
}

impl Into<TopThreeTriangles> for Turns {
    fn into(self) -> TopThreeTriangles {
        match self {
            Turns::TopLeft => TopThreeTriangles(three_triangles::Turns::Left.into()),
            Turns::TopLeftPrime => TopThreeTriangles(three_triangles::Turns::LeftPrime.into()),
            Turns::TopUpperRight => TopThreeTriangles(three_triangles::Turns::UpperRight.into()),
            Turns::TopUpperRightPrime => TopThreeTriangles(three_triangles::Turns::UpperRightPrime.into()),
            Turns::TopLowerRight => TopThreeTriangles(three_triangles::Turns::LowerRight.into()),
            Turns::TopLowerRightPrime => TopThreeTriangles(three_triangles::Turns::LowerRightPrime.into()),

            Turns::BothLeft => TopThreeTriangles(three_triangles::Turns::Left.into()),
            Turns::BothLeftPrime => TopThreeTriangles(three_triangles::Turns::LeftPrime.into()),
            Turns::BothUpperRight => TopThreeTriangles(three_triangles::Turns::UpperRight.into()),
            Turns::BothUpperRightPrime => TopThreeTriangles(three_triangles::Turns::UpperRightPrime.into()),
            Turns::BothLowerRight => TopThreeTriangles(three_triangles::Turns::LowerRight.into()),
            Turns::BothLowerRightPrime => TopThreeTriangles(three_triangles::Turns::LowerRightPrime.into()),
        }
    }
}

impl Into<BottomThreeTriangles> for Turns {
    fn into(self) -> BottomThreeTriangles {
        match self {
            Turns::TopLeft => BottomThreeTriangles(three_triangles::ThreeTriangles::identity()),
            Turns::TopLeftPrime => BottomThreeTriangles(three_triangles::ThreeTriangles::identity()),
            Turns::TopUpperRight => BottomThreeTriangles(three_triangles::ThreeTriangles::identity()),
            Turns::TopUpperRightPrime => BottomThreeTriangles(three_triangles::ThreeTriangles::identity()),
            Turns::TopLowerRight => BottomThreeTriangles(three_triangles::ThreeTriangles::identity()),
            Turns::TopLowerRightPrime => BottomThreeTriangles(three_triangles::ThreeTriangles::identity()),

            Turns::BothLeft => BottomThreeTriangles(three_triangles::Turns::Left.into()),
            Turns::BothLeftPrime => BottomThreeTriangles(three_triangles::Turns::LeftPrime.into()),
            Turns::BothUpperRight => BottomThreeTriangles(three_triangles::Turns::UpperRight.into()),
            Turns::BothUpperRightPrime => BottomThreeTriangles(three_triangles::Turns::UpperRightPrime.into()),
            Turns::BothLowerRight => BottomThreeTriangles(three_triangles::Turns::LowerRight.into()),
            Turns::BothLowerRightPrime => BottomThreeTriangles(three_triangles::Turns::LowerRightPrime.into()),
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

impl Into<TopThreeTriangles> for FullSymmetry {
    fn into(self) -> TopThreeTriangles {
        match self {
            FullSymmetry::Identity => TopThreeTriangles(three_triangles::FullSymmetry::Identity.into()),
            FullSymmetry::RotateCounterClock => TopThreeTriangles(three_triangles::FullSymmetry::RotateCounterClock.into()),
            FullSymmetry::RotateClock => TopThreeTriangles(three_triangles::FullSymmetry::RotateClock.into()),
            FullSymmetry::MirrorLeft => TopThreeTriangles(three_triangles::FullSymmetry::MirrorLeft.into()),
            FullSymmetry::MirrorUpperRight => TopThreeTriangles(three_triangles::FullSymmetry::MirrorUpperRight.into()),
            FullSymmetry::MirrorLowerRight => TopThreeTriangles(three_triangles::FullSymmetry::MirrorLowerRight.into()),
        }
    }
}

impl EquivalenceClass<FullSymmetry> for TopThreeTriangles {
    fn get_equivalent(self, sym: &FullSymmetry) -> TopThreeTriangles {
        let x: TopThreeTriangles = sym.clone().into();
        x.invert().permute(self).permute(x)
    }
}

impl Into<BottomThreeTriangles> for FullSymmetry {
    fn into(self) -> BottomThreeTriangles {
        match self {
            FullSymmetry::Identity => BottomThreeTriangles(three_triangles::FullSymmetry::Identity.into()),
            FullSymmetry::RotateCounterClock => BottomThreeTriangles(three_triangles::FullSymmetry::RotateCounterClock.into()),
            FullSymmetry::RotateClock => BottomThreeTriangles(three_triangles::FullSymmetry::RotateClock.into()),
            FullSymmetry::MirrorLeft => BottomThreeTriangles(three_triangles::FullSymmetry::MirrorLeft.into()),
            FullSymmetry::MirrorUpperRight => BottomThreeTriangles(three_triangles::FullSymmetry::MirrorUpperRight.into()),
            FullSymmetry::MirrorLowerRight => BottomThreeTriangles(three_triangles::FullSymmetry::MirrorLowerRight.into()),
        }
    }
}

impl EquivalenceClass<FullSymmetry> for BottomThreeTriangles {
    fn get_equivalent(self, sym: &FullSymmetry) -> BottomThreeTriangles {
        let x: BottomThreeTriangles = sym.clone().into();
        x.invert().permute(self).permute(x)
    }
}

impl EquivalenceClass<FullSymmetry> for Turns {
    fn get_equivalent(self, sym: &FullSymmetry) -> Turns {
        match (sym, self) {
            (FullSymmetry::Identity, Turns::TopLeft) => Turns::TopLeft,
            (FullSymmetry::Identity, Turns::TopLeftPrime) => Turns::TopLeftPrime,
            (FullSymmetry::Identity, Turns::TopUpperRight) => Turns::TopUpperRight,
            (FullSymmetry::Identity, Turns::TopUpperRightPrime) => Turns::TopUpperRightPrime,
            (FullSymmetry::Identity, Turns::TopLowerRight) => Turns::TopLowerRight,
            (FullSymmetry::Identity, Turns::TopLowerRightPrime) => Turns::TopLowerRightPrime,

            (FullSymmetry::RotateCounterClock, Turns::TopLeft) => Turns::TopLowerRight,
            (FullSymmetry::RotateCounterClock, Turns::TopLeftPrime) => Turns::TopLowerRightPrime,
            (FullSymmetry::RotateCounterClock, Turns::TopUpperRight) => Turns::TopLeft,
            (FullSymmetry::RotateCounterClock, Turns::TopUpperRightPrime) => Turns::TopLeftPrime,
            (FullSymmetry::RotateCounterClock, Turns::TopLowerRight) => Turns::TopUpperRight,
            (FullSymmetry::RotateCounterClock, Turns::TopLowerRightPrime) => Turns::TopUpperRightPrime,

            (FullSymmetry::RotateClock, Turns::TopLeft) => Turns::TopUpperRight,
            (FullSymmetry::RotateClock, Turns::TopLeftPrime) => Turns::TopUpperRightPrime,
            (FullSymmetry::RotateClock, Turns::TopUpperRight) => Turns::TopLowerRight,
            (FullSymmetry::RotateClock, Turns::TopUpperRightPrime) => Turns::TopLowerRightPrime,
            (FullSymmetry::RotateClock, Turns::TopLowerRight) => Turns::TopLeft,
            (FullSymmetry::RotateClock, Turns::TopLowerRightPrime) => Turns::TopLeftPrime,

            (FullSymmetry::MirrorLeft, Turns::TopLeft) => Turns::TopLeftPrime,
            (FullSymmetry::MirrorLeft, Turns::TopLeftPrime) => Turns::TopLeft,
            (FullSymmetry::MirrorLeft, Turns::TopUpperRight) => Turns::TopLowerRightPrime,
            (FullSymmetry::MirrorLeft, Turns::TopUpperRightPrime) => Turns::TopLowerRight,
            (FullSymmetry::MirrorLeft, Turns::TopLowerRight) => Turns::TopUpperRightPrime,
            (FullSymmetry::MirrorLeft, Turns::TopLowerRightPrime) => Turns::TopUpperRight,

            (FullSymmetry::MirrorUpperRight, Turns::TopLeft) => Turns::TopLowerRightPrime,
            (FullSymmetry::MirrorUpperRight, Turns::TopLeftPrime) => Turns::TopLowerRight,
            (FullSymmetry::MirrorUpperRight, Turns::TopUpperRight) => Turns::TopUpperRightPrime,
            (FullSymmetry::MirrorUpperRight, Turns::TopUpperRightPrime) => Turns::TopUpperRight,
            (FullSymmetry::MirrorUpperRight, Turns::TopLowerRight) => Turns::TopLeftPrime,
            (FullSymmetry::MirrorUpperRight, Turns::TopLowerRightPrime) => Turns::TopLeft,

            (FullSymmetry::MirrorLowerRight, Turns::TopLeft) => Turns::TopUpperRightPrime,
            (FullSymmetry::MirrorLowerRight, Turns::TopLeftPrime) => Turns::TopUpperRight,
            (FullSymmetry::MirrorLowerRight, Turns::TopUpperRight) => Turns::TopLeftPrime,
            (FullSymmetry::MirrorLowerRight, Turns::TopUpperRightPrime) => Turns::TopLeft,
            (FullSymmetry::MirrorLowerRight, Turns::TopLowerRight) => Turns::TopLowerRightPrime,
            (FullSymmetry::MirrorLowerRight, Turns::TopLowerRightPrime) => Turns::TopLowerRight,

            (FullSymmetry::Identity, Turns::BothLeft) => Turns::BothLeft,
            (FullSymmetry::Identity, Turns::BothLeftPrime) => Turns::BothLeftPrime,
            (FullSymmetry::Identity, Turns::BothUpperRight) => Turns::BothUpperRight,
            (FullSymmetry::Identity, Turns::BothUpperRightPrime) => Turns::BothUpperRightPrime,
            (FullSymmetry::Identity, Turns::BothLowerRight) => Turns::BothLowerRight,
            (FullSymmetry::Identity, Turns::BothLowerRightPrime) => Turns::BothLowerRightPrime,

            (FullSymmetry::RotateCounterClock, Turns::BothLeft) => Turns::BothLowerRight,
            (FullSymmetry::RotateCounterClock, Turns::BothLeftPrime) => Turns::BothLowerRightPrime,
            (FullSymmetry::RotateCounterClock, Turns::BothUpperRight) => Turns::BothLeft,
            (FullSymmetry::RotateCounterClock, Turns::BothUpperRightPrime) => Turns::BothLeftPrime,
            (FullSymmetry::RotateCounterClock, Turns::BothLowerRight) => Turns::BothUpperRight,
            (FullSymmetry::RotateCounterClock, Turns::BothLowerRightPrime) => Turns::BothUpperRightPrime,

            (FullSymmetry::RotateClock, Turns::BothLeft) => Turns::BothUpperRight,
            (FullSymmetry::RotateClock, Turns::BothLeftPrime) => Turns::BothUpperRightPrime,
            (FullSymmetry::RotateClock, Turns::BothUpperRight) => Turns::BothLowerRight,
            (FullSymmetry::RotateClock, Turns::BothUpperRightPrime) => Turns::BothLowerRightPrime,
            (FullSymmetry::RotateClock, Turns::BothLowerRight) => Turns::BothLeft,
            (FullSymmetry::RotateClock, Turns::BothLowerRightPrime) => Turns::BothLeftPrime,

            (FullSymmetry::MirrorLeft, Turns::BothLeft) => Turns::BothLeftPrime,
            (FullSymmetry::MirrorLeft, Turns::BothLeftPrime) => Turns::BothLeft,
            (FullSymmetry::MirrorLeft, Turns::BothUpperRight) => Turns::BothLowerRightPrime,
            (FullSymmetry::MirrorLeft, Turns::BothUpperRightPrime) => Turns::BothLowerRight,
            (FullSymmetry::MirrorLeft, Turns::BothLowerRight) => Turns::BothUpperRightPrime,
            (FullSymmetry::MirrorLeft, Turns::BothLowerRightPrime) => Turns::BothUpperRight,

            (FullSymmetry::MirrorUpperRight, Turns::BothLeft) => Turns::BothLowerRightPrime,
            (FullSymmetry::MirrorUpperRight, Turns::BothLeftPrime) => Turns::BothLowerRight,
            (FullSymmetry::MirrorUpperRight, Turns::BothUpperRight) => Turns::BothUpperRightPrime,
            (FullSymmetry::MirrorUpperRight, Turns::BothUpperRightPrime) => Turns::BothUpperRight,
            (FullSymmetry::MirrorUpperRight, Turns::BothLowerRight) => Turns::BothLeftPrime,
            (FullSymmetry::MirrorUpperRight, Turns::BothLowerRightPrime) => Turns::BothLeft,

            (FullSymmetry::MirrorLowerRight, Turns::BothLeft) => Turns::BothUpperRightPrime,
            (FullSymmetry::MirrorLowerRight, Turns::BothLeftPrime) => Turns::BothUpperRight,
            (FullSymmetry::MirrorLowerRight, Turns::BothUpperRight) => Turns::BothLeftPrime,
            (FullSymmetry::MirrorLowerRight, Turns::BothUpperRightPrime) => Turns::BothLeft,
            (FullSymmetry::MirrorLowerRight, Turns::BothLowerRight) => Turns::BothLowerRightPrime,
            (FullSymmetry::MirrorLowerRight, Turns::BothLowerRightPrime) => Turns::BothLowerRight,
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

impl Into<TopThreeTriangles> for NoSymmetry {
    fn into(self) -> TopThreeTriangles {
        let full: FullSymmetry = self.into();
        full.into()
    }
}

impl Into<BottomThreeTriangles> for NoSymmetry {
    fn into(self) -> BottomThreeTriangles {
        let full: FullSymmetry = self.into();
        full.into()
    }
}

impl EquivalenceClass<NoSymmetry> for TopThreeTriangles {
    fn get_equivalent(self, sym: &NoSymmetry) -> TopThreeTriangles {
        let x: TopThreeTriangles = sym.clone().into();
        x.invert().permute(self).permute(x)
    }
}

impl EquivalenceClass<NoSymmetry> for BottomThreeTriangles {
    fn get_equivalent(self, sym: &NoSymmetry) -> BottomThreeTriangles {
        let x: BottomThreeTriangles = sym.clone().into();
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

impl Into<TopThreeTriangles> for MirrorUDSymmetry {
    fn into(self) -> TopThreeTriangles {
        let full: FullSymmetry = self.into();
        full.into()
    }
}

impl Into<BottomThreeTriangles> for MirrorUDSymmetry {
    fn into(self) -> BottomThreeTriangles {
        let full: FullSymmetry = self.into();
        full.into()
    }
}

impl EquivalenceClass<MirrorUDSymmetry> for TopThreeTriangles {
    fn get_equivalent(self, sym: &MirrorUDSymmetry) -> TopThreeTriangles {
        let x: TopThreeTriangles = sym.clone().into();
        x.invert().permute(self).permute(x)
    }
}

impl EquivalenceClass<MirrorUDSymmetry> for BottomThreeTriangles {
    fn get_equivalent(self, sym: &MirrorUDSymmetry) -> BottomThreeTriangles {
        let x: BottomThreeTriangles = sym.clone().into();
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

impl Into<TopThreeTriangles> for RotationalSymmetry {
    fn into(self) -> TopThreeTriangles {
        let full: FullSymmetry = self.into();
        full.into()
    }
}

impl Into<BottomThreeTriangles> for RotationalSymmetry {
    fn into(self) -> BottomThreeTriangles {
        let full: FullSymmetry = self.into();
        full.into()
    }
}

impl EquivalenceClass<RotationalSymmetry> for TopThreeTriangles {
    fn get_equivalent(self, sym: &RotationalSymmetry) -> TopThreeTriangles {
        let x: TopThreeTriangles = sym.clone().into();
        x.invert().permute(self).permute(x)
    }
}

impl EquivalenceClass<RotationalSymmetry> for BottomThreeTriangles {
    fn get_equivalent(self, sym: &RotationalSymmetry) -> BottomThreeTriangles {
        let x: BottomThreeTriangles = sym.clone().into();
        x.invert().permute(self).permute(x)
    }
}

impl EquivalenceClass<RotationalSymmetry> for Turns {
    fn get_equivalent(self, sym: &RotationalSymmetry) -> Turns {
        let full: FullSymmetry = (*sym).into();
        self.get_equivalent(&full)
    }
}

impl Into<TopThreeTriangles> for three_triangles::ThreeTrianglesIndex {
    fn into(self) -> TopThreeTriangles {
        TopThreeTriangles(self.into())
    }
}

impl Into<BottomThreeTriangles> for three_triangles::ThreeTrianglesIndex {
    fn into(self) -> BottomThreeTriangles {
        BottomThreeTriangles(self.into())
    }
}

// TODO: This technically can fail.  We don't know that an odd permutation
// wasn't applied.  I have no idea how to handle this though (it should at
// least panic).
impl Into<three_triangles::ThreeTrianglesIndex> for TopThreeTriangles {
    fn into(self) -> three_triangles::ThreeTrianglesIndex {
        self.0.into()
    }
}

// TODO: This technically can fail.  We don't know that an odd permutation
// wasn't applied.  I have no idea how to handle this though (it should at
// least panic).
impl Into<three_triangles::ThreeTrianglesIndex> for BottomThreeTriangles {
    fn into(self) -> three_triangles::ThreeTrianglesIndex {
        self.0.into()
    }
}

impl Into<TopThreeTriangles> for three_triangles::ThreeTrianglesEvenIndex {
    fn into(self) -> TopThreeTriangles {
        TopThreeTriangles(self.into())
    }
}

impl Into<BottomThreeTriangles> for three_triangles::ThreeTrianglesEvenIndex {
    fn into(self) -> BottomThreeTriangles {
        BottomThreeTriangles(self.into())
    }
}

// TODO: This technically can fail.  We don't know that an odd permutation
// wasn't applied.  I have no idea how to handle this though (it should at
// least panic).
impl Into<three_triangles::ThreeTrianglesEvenIndex> for TopThreeTriangles {
    fn into(self) -> three_triangles::ThreeTrianglesEvenIndex {
        self.0.into()
    }
}

// TODO: This technically can fail.  We don't know that an odd permutation
// wasn't applied.  I have no idea how to handle this though (it should at
// least panic).
impl Into<three_triangles::ThreeTrianglesEvenIndex> for BottomThreeTriangles {
    fn into(self) -> three_triangles::ThreeTrianglesEvenIndex {
        self.0.into()
    }
}

pub fn moves_to_solve() -> BTreeMap<(TopThreeTriangles, BottomThreeTriangles), usize> {
    let mut queue = VecDeque::new();
    let mut map = BTreeMap::new();

    map.insert((TopThreeTriangles::identity().into(), BottomThreeTriangles::identity().into()), 0);
    queue.push_back(((TopThreeTriangles::identity(), BottomThreeTriangles::identity()), 1));

    loop {
        match queue.pop_front() {
            None => break,
            Some(((top, bottom), count)) => {
                for t in all::<Turns>() {
                    let top_turned = top.clone().permute(t.into());
                    let bottom_turned = bottom.clone().permute(t.into());
                    let map_turned = (top_turned.into(), bottom_turned.into());
                    let queue_turned = (top_turned, bottom_turned);
                    match map.get(&map_turned) {
                        None => {
                            map.insert(map_turned, count);
                            queue.push_back((queue_turned, count + 1))
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
    fn top_perm_and_turn_no_symmetries_are_equivalent() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s in all::<NoSymmetry>() {
            for pi in all::<three_triangles::ThreeTrianglesIndex>() {
                let p: TopThreeTriangles = pi.into();
                for t in all::<Turns>() {
                    let after_permute = p.permute(t.into()).get_equivalent(&s);
                    let before_permute = p.get_equivalent(&s).permute(t.get_equivalent(&s).into());
                    assert_eq!(after_permute, before_permute)
                }
            }
        }
    }

    #[test]
    fn top_perm_and_turn_mirror_ud_symmetries_are_equivalent() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s in all::<MirrorUDSymmetry>() {
            for pi in all::<three_triangles::ThreeTrianglesIndex>() {
                let p: TopThreeTriangles = pi.into();
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
    fn top_perm_and_turn_rotational_symmetries_are_equivalent() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s in all::<RotationalSymmetry>() {
            for pi in all::<three_triangles::ThreeTrianglesIndex>() {
                let p: TopThreeTriangles = pi.into();
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
    fn top_perm_and_turn_full_symmetries_are_equivalent() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s in all::<FullSymmetry>() {
            for pi in all::<three_triangles::ThreeTrianglesIndex>() {
                let p: TopThreeTriangles = pi.into();
                for t in all::<Turns>() {
                    let after_permute = p.permute(t.into()).get_equivalent(&s);
                    let before_permute = p.get_equivalent(&s).permute(t.get_equivalent(&s).into());
                    assert_eq!(after_permute, before_permute)
                }
            }
        }
    }

    #[test]
    fn top_direct_and_indirect_sym_multiplication_are_equivalent_for_full_symmetry() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s0 in all::<FullSymmetry>() {
            for s1 in all::<FullSymmetry>() {
                for pi in all::<three_triangles::ThreeTrianglesIndex>() {
                    let p: TopThreeTriangles = pi.into();
                    let as_tt = p.permute(s0.into()).permute(s1.into());
                    let as_sym = p.permute(s0.permute(s1).into());
                    assert_eq!(as_tt, as_sym);
                }
            }
        }
    }

    #[test]
    fn top_direct_and_indirect_sym_multiplication_are_equivalent_for_mirror_ud_symmetry() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s0 in all::<MirrorUDSymmetry>() {
            for s1 in all::<MirrorUDSymmetry>() {
                for pi in all::<three_triangles::ThreeTrianglesIndex>() {
                    let p: TopThreeTriangles = pi.into();
                    let as_tt = p.permute(s0.into()).permute(s1.into());
                    let as_sym = p.permute(s0.permute(s1).into());
                    assert_eq!(as_tt, as_sym);
                }
            }
        }
    }

    #[test]
    fn top_direct_and_indirect_sym_multiplication_are_equivalent_for_rotational_symmetry() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s0 in all::<RotationalSymmetry>() {
            for s1 in all::<RotationalSymmetry>() {
                for pi in all::<three_triangles::ThreeTrianglesIndex>() {
                    let p: TopThreeTriangles = pi.into();
                    let as_tt = p.permute(s0.into()).permute(s1.into());
                    let as_sym = p.permute(s0.permute(s1).into());
                    assert_eq!(as_tt, as_sym);
                }
            }
        }
    }

    #[test]
    fn top_direct_and_indirect_sym_multiplication_are_equivalent_for_no_symmetry() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s0 in all::<NoSymmetry>() {
            for s1 in all::<NoSymmetry>() {
                for pi in all::<three_triangles::ThreeTrianglesIndex>() {
                    let p: TopThreeTriangles = pi.into();
                    let as_tt = p.permute(s0.into()).permute(s1.into());
                    let as_sym = p.permute(s0.permute(s1).into());
                    assert_eq!(as_tt, as_sym);
                }
            }
        }
    }

    #[test]
    fn bottom_perm_and_turn_no_symmetries_are_equivalent() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s in all::<NoSymmetry>() {
            for pi in all::<three_triangles::ThreeTrianglesIndex>() {
                let p: BottomThreeTriangles = pi.into();
                for t in all::<Turns>() {
                    let after_permute = p.permute(t.into()).get_equivalent(&s);
                    let before_permute = p.get_equivalent(&s).permute(t.get_equivalent(&s).into());
                    assert_eq!(after_permute, before_permute)
                }
            }
        }
    }

    #[test]
    fn bottom_perm_and_turn_mirror_ud_symmetries_are_equivalent() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s in all::<MirrorUDSymmetry>() {
            for pi in all::<three_triangles::ThreeTrianglesIndex>() {
                let p: BottomThreeTriangles = pi.into();
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
    fn bottom_perm_and_turn_rotational_symmetries_are_equivalent() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s in all::<RotationalSymmetry>() {
            for pi in all::<three_triangles::ThreeTrianglesIndex>() {
                let p: BottomThreeTriangles = pi.into();
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
    fn bottom_perm_and_turn_full_symmetries_are_equivalent() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s in all::<FullSymmetry>() {
            for pi in all::<three_triangles::ThreeTrianglesIndex>() {
                let p: BottomThreeTriangles = pi.into();
                for t in all::<Turns>() {
                    let after_permute = p.permute(t.into()).get_equivalent(&s);
                    let before_permute = p.get_equivalent(&s).permute(t.get_equivalent(&s).into());
                    assert_eq!(after_permute, before_permute)
                }
            }
        }
    }

    #[test]
    fn bottom_direct_and_indirect_sym_multiplication_are_equivalent_for_full_symmetry() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s0 in all::<FullSymmetry>() {
            for s1 in all::<FullSymmetry>() {
                for pi in all::<three_triangles::ThreeTrianglesIndex>() {
                    let p: BottomThreeTriangles = pi.into();
                    let as_tt = p.permute(s0.into()).permute(s1.into());
                    let as_sym = p.permute(s0.permute(s1).into());
                    assert_eq!(as_tt, as_sym);
                }
            }
        }
    }

    #[test]
    fn bottom_direct_and_indirect_sym_multiplication_are_equivalent_for_mirror_ud_symmetry() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s0 in all::<MirrorUDSymmetry>() {
            for s1 in all::<MirrorUDSymmetry>() {
                for pi in all::<three_triangles::ThreeTrianglesIndex>() {
                    let p: BottomThreeTriangles = pi.into();
                    let as_tt = p.permute(s0.into()).permute(s1.into());
                    let as_sym = p.permute(s0.permute(s1).into());
                    assert_eq!(as_tt, as_sym);
                }
            }
        }
    }

    #[test]
    fn bottom_direct_and_indirect_sym_multiplication_are_equivalent_for_rotational_symmetry() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s0 in all::<RotationalSymmetry>() {
            for s1 in all::<RotationalSymmetry>() {
                for pi in all::<three_triangles::ThreeTrianglesIndex>() {
                    let p: BottomThreeTriangles = pi.into();
                    let as_tt = p.permute(s0.into()).permute(s1.into());
                    let as_sym = p.permute(s0.permute(s1).into());
                    assert_eq!(as_tt, as_sym);
                }
            }
        }
    }

    #[test]
    fn bottom_direct_and_indirect_sym_multiplication_are_equivalent_for_no_symmetry() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s0 in all::<NoSymmetry>() {
            for s1 in all::<NoSymmetry>() {
                for pi in all::<three_triangles::ThreeTrianglesIndex>() {
                    let p: BottomThreeTriangles = pi.into();
                    let as_tt = p.permute(s0.into()).permute(s1.into());
                    let as_sym = p.permute(s0.permute(s1).into());
                    assert_eq!(as_tt, as_sym);
                }
            }
        }
    }
}
