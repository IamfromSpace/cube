use permutation_group::PermutationGroup as PG;
use invertable::Invertable;
use equivalence_class::EquivalenceClass;

use std::collections::BTreeMap;
use std::collections::VecDeque;
use std::convert::{TryInto, TryFrom};
use std::sync::LazyLock;
use enum_iterator::{all, Sequence};

// We have a 3D puzzle with six trapezoidal "faces" aranged to form a
// tetrahedron.  This is the 3D version of the simpler ThreeTrapezoids model.
// You can imagine one tetrahedron inside of another, where you can rotate the
// points of two inner vertices and two outer vertices.  We can rotate either
// clockwise or counter-clockwise.  For the vertical tetrahedrons, clockwise is
// always from the top point, along the outer edge, towards the base.   Even
// though just clockwise turns generate all reachable states, we get more
// symmetries if we include counterclockwise turns (because the turns must also
// follow the symmetries).
//
// This puzzle, due to its size, is typically less useful than ThreeTrapezoids
// as it's 14x larger, even accounting for symmetry, but this puzzle is just
// large enough to support a two-phase solver (with ThreeTrapezoids as the
// subgroup in the coset reduction).
//
// Vertices 0, 1, 2, 6 form the inner tetrahedron, and 3, 4, 5, 7 form the
// outer tetrahedron.  Or you can think of 0-5 on a flat plane, and then 6 and
// 7 as the top points extending centrally along the z-axis.
//
//     7
//  4  |
//   \ 6
//    1|
//     * 0--3
//    2
//   /
//  5
//
// A clockwise turn of the upper-right face yields:
// 
//     7
//  4  |
//   \ 6
//    1|
//     * 0--3
//    2
//   /
//  5
//
// A clockwise turn on the z-right face yields:
//
//     6
//  4  |
//   \ 0
//    1|
//     * 3--7
//    2
//   /
//  5
//
// While clockwise on the z-upper-left face yields:
//
//     6
//  7  |
//   \ 1
//    4|
//     * 0--3
//    2
//   /
//  5
//
// We consider symmetry through mirroring top-down and rotation by 120 degrees.
// Combination of the two allows mirroring along any face.  We could support
// full tetrahedral symmetries (adding 4x the variations, but don't currently
// do so, as there's no need).
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct TetrahedralTrapezoids([u16; 8]);

const fn permute_arr(a: &[u16; 8], b: &[u16; 8]) -> [u16; 8] {
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

const fn arr_inv(a: &[u16; 8]) -> [u16; 8] {
    let mut r = [0; 8];
    r[a[0] as usize] = 0;
    r[a[1] as usize] = 1;
    r[a[2] as usize] = 2;
    r[a[3] as usize] = 3;
    r[a[4] as usize] = 4;
    r[a[5] as usize] = 5;
    r[a[6] as usize] = 6;
    r[a[7] as usize] = 7;
    r
}

impl functional::BinaryOperation<TetrahedralTrapezoids> for TetrahedralTrapezoids {
    fn apply(a: TetrahedralTrapezoids, b: TetrahedralTrapezoids) -> TetrahedralTrapezoids {
        TetrahedralTrapezoids(permute_arr(&a.0, &b.0))
    }
}

impl functional::AssociativeOperation<TetrahedralTrapezoids> for TetrahedralTrapezoids { }

impl functional::Monoid<TetrahedralTrapezoids> for TetrahedralTrapezoids {
    fn one() -> TetrahedralTrapezoids {
        TetrahedralTrapezoids([0, 1, 2, 3, 4, 5, 6, 7])
    }
}

impl Invertable for TetrahedralTrapezoids {
    fn invert(&self) -> TetrahedralTrapezoids {
        TetrahedralTrapezoids(arr_inv(&self.0))
    }
}

impl PG for TetrahedralTrapezoids {}

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
    ZRight,
    ZRightPrime,
    ZUpperLeft,
    ZUpperLeftPrime,
    ZLowerLeft,
    ZLowerLeftPrime,
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
            Turns::ZRight => Turns::ZRightPrime,
            Turns::ZRightPrime => Turns::ZRight,
            Turns::ZUpperLeft => Turns::ZUpperLeftPrime,
            Turns::ZUpperLeftPrime => Turns::ZUpperLeft,
            Turns::ZLowerLeft => Turns::ZLowerLeftPrime,
            Turns::ZLowerLeftPrime => Turns::ZLowerLeft,
        }
    }
}

impl Into<usize> for Turns {
    fn into(self) -> usize {
        self as usize
    }
}

impl Into<TetrahedralTrapezoids> for Turns {
    fn into(self) -> TetrahedralTrapezoids {
        match self {
            // 1 -> 4 -> 5 -> 2
            Turns::Left => TetrahedralTrapezoids([0, 2, 5, 3, 1, 4, 6, 7]),
            Turns::LeftPrime => TetrahedralTrapezoids([0, 4, 1, 3, 5, 2, 6, 7]),
            // 0 -> 3 -> 4 -> 1
            Turns::UpperRight => TetrahedralTrapezoids([1, 4, 2, 0, 3, 5, 6, 7]),
            Turns::UpperRightPrime => TetrahedralTrapezoids([3, 0, 2, 4, 1, 5, 6, 7]),
            // 0 -> 2 -> 5 -> 3
            Turns::LowerRight => TetrahedralTrapezoids([3, 1, 0, 5, 4, 2, 6, 7]),
            Turns::LowerRightPrime => TetrahedralTrapezoids([2, 1, 5, 0, 4, 3, 6, 7]),
            // 0 -> 3 -> 7 -> 6
            Turns::ZRight => TetrahedralTrapezoids([6, 1, 2, 0, 4, 5, 7, 3]),
            Turns::ZRightPrime => TetrahedralTrapezoids([3, 1, 2, 7, 4, 5, 0, 6]),
            // 1 -> 4 -> 7 -> 6
            Turns::ZUpperLeft => TetrahedralTrapezoids([0, 6, 2, 3, 1, 5, 7, 4]),
            Turns::ZUpperLeftPrime => TetrahedralTrapezoids([0, 4, 2, 3, 7, 5, 1, 6]),
            // 2 -> 5 -> 7 -> 6
            Turns::ZLowerLeft => TetrahedralTrapezoids([0, 1, 6, 3, 4, 2, 7, 5]),
            Turns::ZLowerLeftPrime => TetrahedralTrapezoids([0, 1, 5, 3, 4, 7, 2, 6]),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash, Sequence)]
#[repr(u8)]
pub enum PlanarSymmetry {
    Identity,
    RotateCounterClock,
    RotateClock,
    // Indicates mirroring along the middle of the face, or along the edge not of this face.
    MirrorLeft,
    MirrorUpperRight,
    MirrorLowerRight,
}

impl Into<usize> for PlanarSymmetry {
    fn into(self) -> usize {
        self as usize
    }
}

impl functional::BinaryOperation<PlanarSymmetry> for PlanarSymmetry {
    fn apply(a: PlanarSymmetry, b: PlanarSymmetry) -> PlanarSymmetry {
        match (a, b) {
            (PlanarSymmetry::Identity, PlanarSymmetry::Identity) => PlanarSymmetry::Identity,
            (PlanarSymmetry::Identity, PlanarSymmetry::RotateCounterClock) => PlanarSymmetry::RotateCounterClock,
            (PlanarSymmetry::Identity, PlanarSymmetry::RotateClock) => PlanarSymmetry::RotateClock,
            (PlanarSymmetry::Identity, PlanarSymmetry::MirrorLeft) => PlanarSymmetry::MirrorLeft,
            (PlanarSymmetry::Identity, PlanarSymmetry::MirrorUpperRight) => PlanarSymmetry::MirrorUpperRight,
            (PlanarSymmetry::Identity, PlanarSymmetry::MirrorLowerRight) => PlanarSymmetry::MirrorLowerRight,

            (PlanarSymmetry::RotateCounterClock, PlanarSymmetry::Identity) => PlanarSymmetry::RotateCounterClock,
            (PlanarSymmetry::RotateCounterClock, PlanarSymmetry::RotateCounterClock) => PlanarSymmetry::RotateClock,
            (PlanarSymmetry::RotateCounterClock, PlanarSymmetry::RotateClock) => PlanarSymmetry::Identity,
            (PlanarSymmetry::RotateCounterClock, PlanarSymmetry::MirrorLeft) => PlanarSymmetry::MirrorLowerRight,
            (PlanarSymmetry::RotateCounterClock, PlanarSymmetry::MirrorUpperRight) => PlanarSymmetry::MirrorLeft,
            (PlanarSymmetry::RotateCounterClock, PlanarSymmetry::MirrorLowerRight) => PlanarSymmetry::MirrorUpperRight,

            (PlanarSymmetry::RotateClock, PlanarSymmetry::Identity) => PlanarSymmetry::RotateClock,
            (PlanarSymmetry::RotateClock, PlanarSymmetry::RotateCounterClock) => PlanarSymmetry::Identity,
            (PlanarSymmetry::RotateClock, PlanarSymmetry::RotateClock) => PlanarSymmetry::RotateCounterClock,
            (PlanarSymmetry::RotateClock, PlanarSymmetry::MirrorLeft) => PlanarSymmetry::MirrorUpperRight,
            (PlanarSymmetry::RotateClock, PlanarSymmetry::MirrorUpperRight) => PlanarSymmetry::MirrorLowerRight,
            (PlanarSymmetry::RotateClock, PlanarSymmetry::MirrorLowerRight) => PlanarSymmetry::MirrorLeft,

            (PlanarSymmetry::MirrorLeft, PlanarSymmetry::Identity) => PlanarSymmetry::MirrorLeft,
            (PlanarSymmetry::MirrorLeft, PlanarSymmetry::RotateCounterClock) => PlanarSymmetry::MirrorUpperRight,
            (PlanarSymmetry::MirrorLeft, PlanarSymmetry::RotateClock) => PlanarSymmetry::MirrorLowerRight,
            (PlanarSymmetry::MirrorLeft, PlanarSymmetry::MirrorLeft) => PlanarSymmetry::Identity,
            (PlanarSymmetry::MirrorLeft, PlanarSymmetry::MirrorUpperRight) => PlanarSymmetry::RotateCounterClock,
            (PlanarSymmetry::MirrorLeft, PlanarSymmetry::MirrorLowerRight) => PlanarSymmetry::RotateClock,

            (PlanarSymmetry::MirrorUpperRight, PlanarSymmetry::Identity) => PlanarSymmetry::MirrorUpperRight,
            (PlanarSymmetry::MirrorUpperRight, PlanarSymmetry::RotateCounterClock) => PlanarSymmetry::MirrorLowerRight,
            (PlanarSymmetry::MirrorUpperRight, PlanarSymmetry::RotateClock) => PlanarSymmetry::MirrorLeft,
            (PlanarSymmetry::MirrorUpperRight, PlanarSymmetry::MirrorLeft) => PlanarSymmetry::RotateClock,
            (PlanarSymmetry::MirrorUpperRight, PlanarSymmetry::MirrorUpperRight) => PlanarSymmetry::Identity,
            (PlanarSymmetry::MirrorUpperRight, PlanarSymmetry::MirrorLowerRight) => PlanarSymmetry::RotateCounterClock,

            (PlanarSymmetry::MirrorLowerRight, PlanarSymmetry::Identity) => PlanarSymmetry::MirrorLowerRight,
            (PlanarSymmetry::MirrorLowerRight, PlanarSymmetry::RotateCounterClock) => PlanarSymmetry::MirrorLeft,
            (PlanarSymmetry::MirrorLowerRight, PlanarSymmetry::RotateClock) => PlanarSymmetry::MirrorUpperRight,
            (PlanarSymmetry::MirrorLowerRight, PlanarSymmetry::MirrorLeft) => PlanarSymmetry::RotateCounterClock,
            (PlanarSymmetry::MirrorLowerRight, PlanarSymmetry::MirrorUpperRight) => PlanarSymmetry::RotateClock,
            (PlanarSymmetry::MirrorLowerRight, PlanarSymmetry::MirrorLowerRight) => PlanarSymmetry::Identity,
        }
    }
}

impl functional::AssociativeOperation<PlanarSymmetry> for PlanarSymmetry { }

impl functional::Monoid<PlanarSymmetry> for PlanarSymmetry {
    fn one() -> PlanarSymmetry {
        PlanarSymmetry::Identity
    }
}

impl Invertable for PlanarSymmetry {
    fn invert(&self) -> PlanarSymmetry {
        match self {
            PlanarSymmetry::Identity => PlanarSymmetry::Identity,
            PlanarSymmetry::RotateCounterClock => PlanarSymmetry::RotateClock,
            PlanarSymmetry::RotateClock => PlanarSymmetry::RotateCounterClock,
            PlanarSymmetry::MirrorLeft => PlanarSymmetry::MirrorLeft,
            PlanarSymmetry::MirrorUpperRight => PlanarSymmetry::MirrorUpperRight,
            PlanarSymmetry::MirrorLowerRight => PlanarSymmetry::MirrorLowerRight,
        }
    }
}

impl PG for PlanarSymmetry {}

impl Into<TetrahedralTrapezoids> for PlanarSymmetry {
    fn into(self) -> TetrahedralTrapezoids {
        match self {
            PlanarSymmetry::Identity => TetrahedralTrapezoids([0, 1, 2, 3, 4, 5, 6, 7]),
            PlanarSymmetry::RotateCounterClock => TetrahedralTrapezoids([1, 2, 0, 4, 5, 3, 6, 7]),
            // 0 -> 1 -> 2
            // 3 -> 4 -> 5
            PlanarSymmetry::RotateClock => TetrahedralTrapezoids([2, 0, 1, 5, 3, 4, 6, 7]),
            // 4 -> 5
            // 1 -> 2
            PlanarSymmetry::MirrorLeft => TetrahedralTrapezoids([0, 2, 1, 3, 5, 4, 6, 7]),
            // 0 -> 1
            // 3 -> 4
            PlanarSymmetry::MirrorUpperRight => TetrahedralTrapezoids([1, 0, 2, 4, 3, 5, 6, 7]),
            // 0 -> 2
            // 3 -> 5
            PlanarSymmetry::MirrorLowerRight => TetrahedralTrapezoids([2, 1, 0, 5, 4, 3, 6, 7]),
        }
    }
}

impl EquivalenceClass<PlanarSymmetry> for TetrahedralTrapezoids {
    fn get_equivalent(self, sym: &PlanarSymmetry) -> TetrahedralTrapezoids {
        let x: TetrahedralTrapezoids = sym.clone().into();
        x.invert().permute(self).permute(x)
    }
}

impl EquivalenceClass<PlanarSymmetry> for Turns {
    fn get_equivalent(self, sym: &PlanarSymmetry) -> Turns {
        match (sym, self) {
            (PlanarSymmetry::Identity, Turns::Left) => Turns::Left,
            (PlanarSymmetry::Identity, Turns::LeftPrime) => Turns::LeftPrime,
            (PlanarSymmetry::Identity, Turns::UpperRight) => Turns::UpperRight,
            (PlanarSymmetry::Identity, Turns::UpperRightPrime) => Turns::UpperRightPrime,
            (PlanarSymmetry::Identity, Turns::LowerRight) => Turns::LowerRight,
            (PlanarSymmetry::Identity, Turns::LowerRightPrime) => Turns::LowerRightPrime,
            (PlanarSymmetry::Identity, Turns::ZRight) => Turns::ZRight,
            (PlanarSymmetry::Identity, Turns::ZRightPrime) => Turns::ZRightPrime,
            (PlanarSymmetry::Identity, Turns::ZUpperLeft) => Turns::ZUpperLeft,
            (PlanarSymmetry::Identity, Turns::ZUpperLeftPrime) => Turns::ZUpperLeftPrime,
            (PlanarSymmetry::Identity, Turns::ZLowerLeft) => Turns::ZLowerLeft,
            (PlanarSymmetry::Identity, Turns::ZLowerLeftPrime) => Turns::ZLowerLeftPrime,

            (PlanarSymmetry::RotateCounterClock, Turns::Left) => Turns::LowerRight,
            (PlanarSymmetry::RotateCounterClock, Turns::LeftPrime) => Turns::LowerRightPrime,
            (PlanarSymmetry::RotateCounterClock, Turns::UpperRight) => Turns::Left,
            (PlanarSymmetry::RotateCounterClock, Turns::UpperRightPrime) => Turns::LeftPrime,
            (PlanarSymmetry::RotateCounterClock, Turns::LowerRight) => Turns::UpperRight,
            (PlanarSymmetry::RotateCounterClock, Turns::LowerRightPrime) => Turns::UpperRightPrime,
            (PlanarSymmetry::RotateCounterClock, Turns::ZRight) => Turns::ZUpperLeft,
            (PlanarSymmetry::RotateCounterClock, Turns::ZRightPrime) => Turns::ZUpperLeftPrime,
            (PlanarSymmetry::RotateCounterClock, Turns::ZUpperLeft) => Turns::ZLowerLeft,
            (PlanarSymmetry::RotateCounterClock, Turns::ZUpperLeftPrime) => Turns::ZLowerLeftPrime,
            (PlanarSymmetry::RotateCounterClock, Turns::ZLowerLeft) => Turns::ZRight,
            (PlanarSymmetry::RotateCounterClock, Turns::ZLowerLeftPrime) => Turns::ZRightPrime,

            (PlanarSymmetry::RotateClock, Turns::Left) => Turns::UpperRight,
            (PlanarSymmetry::RotateClock, Turns::LeftPrime) => Turns::UpperRightPrime,
            (PlanarSymmetry::RotateClock, Turns::UpperRight) => Turns::LowerRight,
            (PlanarSymmetry::RotateClock, Turns::UpperRightPrime) => Turns::LowerRightPrime,
            (PlanarSymmetry::RotateClock, Turns::LowerRight) => Turns::Left,
            (PlanarSymmetry::RotateClock, Turns::LowerRightPrime) => Turns::LeftPrime,
            (PlanarSymmetry::RotateClock, Turns::ZRight) => Turns::ZLowerLeft,
            (PlanarSymmetry::RotateClock, Turns::ZRightPrime) => Turns::ZLowerLeftPrime,
            (PlanarSymmetry::RotateClock, Turns::ZUpperLeft) => Turns::ZRight,
            (PlanarSymmetry::RotateClock, Turns::ZUpperLeftPrime) => Turns::ZRightPrime,
            (PlanarSymmetry::RotateClock, Turns::ZLowerLeft) => Turns::ZUpperLeft,
            (PlanarSymmetry::RotateClock, Turns::ZLowerLeftPrime) => Turns::ZUpperLeftPrime,

            (PlanarSymmetry::MirrorLeft, Turns::Left) => Turns::LeftPrime,
            (PlanarSymmetry::MirrorLeft, Turns::LeftPrime) => Turns::Left,
            (PlanarSymmetry::MirrorLeft, Turns::UpperRight) => Turns::LowerRightPrime,
            (PlanarSymmetry::MirrorLeft, Turns::UpperRightPrime) => Turns::LowerRight,
            (PlanarSymmetry::MirrorLeft, Turns::LowerRight) => Turns::UpperRightPrime,
            (PlanarSymmetry::MirrorLeft, Turns::LowerRightPrime) => Turns::UpperRight,
            (PlanarSymmetry::MirrorLeft, Turns::ZRight) => Turns::ZRight,
            (PlanarSymmetry::MirrorLeft, Turns::ZRightPrime) => Turns::ZRightPrime,
            (PlanarSymmetry::MirrorLeft, Turns::ZUpperLeft) => Turns::ZLowerLeft,
            (PlanarSymmetry::MirrorLeft, Turns::ZUpperLeftPrime) => Turns::ZLowerLeftPrime,
            (PlanarSymmetry::MirrorLeft, Turns::ZLowerLeft) => Turns::ZUpperLeft,
            (PlanarSymmetry::MirrorLeft, Turns::ZLowerLeftPrime) => Turns::ZUpperLeftPrime,

            (PlanarSymmetry::MirrorUpperRight, Turns::Left) => Turns::LowerRightPrime,
            (PlanarSymmetry::MirrorUpperRight, Turns::LeftPrime) => Turns::LowerRight,
            (PlanarSymmetry::MirrorUpperRight, Turns::UpperRight) => Turns::UpperRightPrime,
            (PlanarSymmetry::MirrorUpperRight, Turns::UpperRightPrime) => Turns::UpperRight,
            (PlanarSymmetry::MirrorUpperRight, Turns::LowerRight) => Turns::LeftPrime,
            (PlanarSymmetry::MirrorUpperRight, Turns::LowerRightPrime) => Turns::Left,
            (PlanarSymmetry::MirrorUpperRight, Turns::ZRight) => Turns::ZUpperLeft,
            (PlanarSymmetry::MirrorUpperRight, Turns::ZRightPrime) => Turns::ZUpperLeftPrime,
            (PlanarSymmetry::MirrorUpperRight, Turns::ZUpperLeft) => Turns::ZRight,
            (PlanarSymmetry::MirrorUpperRight, Turns::ZUpperLeftPrime) => Turns::ZRightPrime,
            (PlanarSymmetry::MirrorUpperRight, Turns::ZLowerLeft) => Turns::ZLowerLeft,
            (PlanarSymmetry::MirrorUpperRight, Turns::ZLowerLeftPrime) => Turns::ZLowerLeftPrime,

            (PlanarSymmetry::MirrorLowerRight, Turns::Left) => Turns::UpperRightPrime,
            (PlanarSymmetry::MirrorLowerRight, Turns::LeftPrime) => Turns::UpperRight,
            (PlanarSymmetry::MirrorLowerRight, Turns::UpperRight) => Turns::LeftPrime,
            (PlanarSymmetry::MirrorLowerRight, Turns::UpperRightPrime) => Turns::Left,
            (PlanarSymmetry::MirrorLowerRight, Turns::LowerRight) => Turns::LowerRightPrime,
            (PlanarSymmetry::MirrorLowerRight, Turns::LowerRightPrime) => Turns::LowerRight,
            (PlanarSymmetry::MirrorLowerRight, Turns::ZRight) => Turns::ZLowerLeft,
            (PlanarSymmetry::MirrorLowerRight, Turns::ZRightPrime) => Turns::ZLowerLeftPrime,
            (PlanarSymmetry::MirrorLowerRight, Turns::ZUpperLeft) => Turns::ZUpperLeft,
            (PlanarSymmetry::MirrorLowerRight, Turns::ZUpperLeftPrime) => Turns::ZUpperLeftPrime,
            (PlanarSymmetry::MirrorLowerRight, Turns::ZLowerLeft) => Turns::ZRight,
            (PlanarSymmetry::MirrorLowerRight, Turns::ZLowerLeftPrime) => Turns::ZRightPrime,
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

impl Into<MirrorUDSymmetry> for NoSymmetry {
    fn into(self) -> MirrorUDSymmetry {
        match self {
            NoSymmetry::Identity => MirrorUDSymmetry::Identity,
        }
    }
}

impl Into<PlanarSymmetry> for NoSymmetry {
    fn into(self) -> PlanarSymmetry {
        match self {
            NoSymmetry::Identity => PlanarSymmetry::Identity,
        }
    }
}

impl Into<TetrahedralTrapezoids> for NoSymmetry {
    fn into(self) -> TetrahedralTrapezoids {
        let planar: PlanarSymmetry = self.into();
        planar.into()
    }
}

impl EquivalenceClass<NoSymmetry> for TetrahedralTrapezoids {
    fn get_equivalent(self, sym: &NoSymmetry) -> TetrahedralTrapezoids {
        let x: TetrahedralTrapezoids = sym.clone().into();
        x.invert().permute(self).permute(x)
    }
}

impl EquivalenceClass<NoSymmetry> for Turns {
    fn get_equivalent(self, sym: &NoSymmetry) -> Turns {
        let planar: PlanarSymmetry = (*sym).into();
        self.get_equivalent(&planar)
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

impl Into<PlanarSymmetry> for MirrorUDSymmetry {
    fn into(self) -> PlanarSymmetry {
        match self {
            MirrorUDSymmetry::Identity => PlanarSymmetry::Identity,
            MirrorUDSymmetry::Mirror => PlanarSymmetry::MirrorLeft,
        }
    }
}

impl Into<TetrahedralTrapezoids> for MirrorUDSymmetry {
    fn into(self) -> TetrahedralTrapezoids {
        let planar: PlanarSymmetry = self.into();
        planar.into()
    }
}

impl EquivalenceClass<MirrorUDSymmetry> for TetrahedralTrapezoids {
    fn get_equivalent(self, sym: &MirrorUDSymmetry) -> TetrahedralTrapezoids {
        let x: TetrahedralTrapezoids = sym.clone().into();
        x.invert().permute(self).permute(x)
    }
}

impl EquivalenceClass<MirrorUDSymmetry> for Turns {
    fn get_equivalent(self, sym: &MirrorUDSymmetry) -> Turns {
        let planar: PlanarSymmetry = (*sym).into();
        self.get_equivalent(&planar)
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

impl Into<PlanarSymmetry> for RotationalSymmetry {
    fn into(self) -> PlanarSymmetry {
        match self {
            RotationalSymmetry::Identity => PlanarSymmetry::Identity,
            RotationalSymmetry::RotateCounterClock => PlanarSymmetry::RotateCounterClock,
            RotationalSymmetry::RotateClock => PlanarSymmetry::RotateClock,
        }
    }
}

impl Into<TetrahedralTrapezoids> for RotationalSymmetry {
    fn into(self) -> TetrahedralTrapezoids {
        let planar: PlanarSymmetry = self.into();
        planar.into()
    }
}

impl EquivalenceClass<RotationalSymmetry> for TetrahedralTrapezoids {
    fn get_equivalent(self, sym: &RotationalSymmetry) -> TetrahedralTrapezoids {
        let x: TetrahedralTrapezoids = sym.clone().into();
        x.invert().permute(self).permute(x)
    }
}

impl EquivalenceClass<RotationalSymmetry> for Turns {
    fn get_equivalent(self, sym: &RotationalSymmetry) -> Turns {
        let planar: PlanarSymmetry = (*sym).into();
        self.get_equivalent(&planar)
    }
}

// TODO: There are faster algorithms than this
fn to_lehmer(p: TetrahedralTrapezoids) -> u16 {
    let mut x = p.0.clone();
    for i in 0..8 {
        for j in (i+1)..8 {
            if x[j] > x[i] {
                x[j] -= 1;
            }
        }
    }
    x[0] + 8 * (x[1] + 7 * (x[2] + 6  * (x[3] + 5 * (x[4] + 4 * (x[5] + 3 * (x[6] + 2 * x[7]))))))
}

// TODO: There are faster algorithms than this
fn from_lehmer(i: u16) -> TetrahedralTrapezoids {
    let mut i = i;
    let mut x = [0; 8];
    for j in 0..8 {
        x[j as usize] = i % (8 - j);
        i = i / (8 - j);
    }
    for i in (0..8).rev() {
        for j in (i+1)..8 {
            if x[j as usize] >= x[i] {
                x[j as usize] += 1;
            }
        }
    }
    TetrahedralTrapezoids(x)
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
pub struct TetrahedralTrapezoidsIndex(u16);

impl Sequence for TetrahedralTrapezoidsIndex {
    const CARDINALITY: usize = 40_320;

    fn next(&self) -> Option<Self> {
        if self.0 == (Self::CARDINALITY - 1) as u16 {
            None
        } else {
            Some(TetrahedralTrapezoidsIndex(self.0 + 1))
        }
    }

    fn previous(&self) -> Option<Self> {
        if self.0 == 0 {
            None
        } else {
            Some(TetrahedralTrapezoidsIndex(self.0 - 1))
        }
    }

    fn first() -> Option<Self> {
        Some(TetrahedralTrapezoidsIndex(0))
    }

    fn last() -> Option<Self> {
        Some(TetrahedralTrapezoidsIndex((Self::CARDINALITY - 1) as u16))
    }
}

impl Into<TetrahedralTrapezoids> for TetrahedralTrapezoidsIndex {
    fn into(self) -> TetrahedralTrapezoids {
        from_lehmer(self.0)
    }
}

impl Into<usize> for TetrahedralTrapezoidsIndex {
    fn into(self) -> usize {
        self.0 as usize
    }
}

impl TryFrom<usize> for TetrahedralTrapezoidsIndex {
    type Error = std::num::TryFromIntError;

    fn try_from(i: usize) -> Result<Self, Self::Error> {
        let j = i.try_into()?;
        Ok(TetrahedralTrapezoidsIndex(j))
    }
}

impl Into<TetrahedralTrapezoidsIndex> for TetrahedralTrapezoids {
    fn into(self) -> TetrahedralTrapezoidsIndex {
        TetrahedralTrapezoidsIndex(to_lehmer(self))
    }
}

fn gen_pruning_table() -> BTreeMap<TetrahedralTrapezoidsIndex, usize> {
    let mut queue = VecDeque::new();
    let mut map = BTreeMap::new();

    map.insert(TetrahedralTrapezoids::identity().into(), 0);
    queue.push_back((TetrahedralTrapezoids::identity(), 1));

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

static PRUNING_TABLE: LazyLock<BTreeMap<TetrahedralTrapezoidsIndex, usize>> = LazyLock::new(gen_pruning_table);

pub fn moves_to_solve(i: &TetrahedralTrapezoidsIndex) -> usize {
    let pruning_table = &*PRUNING_TABLE;
    *pruning_table.get(i).unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;
    use enum_iterator::{all, cardinality};
    use quickcheck::Gen;
    use rand::prelude::IteratorRandom;

    impl quickcheck::Arbitrary for TetrahedralTrapezoidsIndex {
        fn arbitrary<G: Gen>(g: &mut G) -> TetrahedralTrapezoidsIndex {
            all::<TetrahedralTrapezoidsIndex>().choose(g).unwrap()
        }
    }

    #[test]
    fn all_odd_lehmer_codes_round_trip() {
        for i in 0..(cardinality::<TetrahedralTrapezoidsIndex>() as u16) {
            let t: TetrahedralTrapezoids = from_lehmer(i);
            assert_eq!(i, to_lehmer(t));
        }
    }

    #[test]
    fn move_counts_are_total_and_sensible() {
        for i in all::<TetrahedralTrapezoidsIndex>() {
            let c = moves_to_solve(&i);
            if TetrahedralTrapezoids::identity() == i.into() {
                assert_eq!(c, 0)
            } else {
                assert!(c > 0 && c <= 8)
            }
        }
    }

    // Even thugh this puzzle is quite small, it's still too big for exhaustive checking
    quickcheck! {
        fn permutation_is_associative(pi_0: TetrahedralTrapezoidsIndex, pi_1: TetrahedralTrapezoidsIndex, pi_2: TetrahedralTrapezoidsIndex) -> bool {
            let p_0: TetrahedralTrapezoids = pi_0.into();
            let p_1: TetrahedralTrapezoids = pi_1.into();
            let p_2: TetrahedralTrapezoids = pi_2.into();
            p_0.permute(p_1).permute(p_2) == p_0.permute(p_1.permute(p_2))
        }
    }

    #[test]
    fn perm_and_turns_and_sym_invert_round_trips() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for pi in all::<TetrahedralTrapezoidsIndex>() {
            let p: TetrahedralTrapezoids = pi.into();
            assert_eq!(p, p.invert().invert());

            for t in all::<Turns>() {
                assert_eq!(p, p.permute(t.into()).permute(t.invert().into()));
            }

            for s in all::<PlanarSymmetry>() {
                assert_eq!(p, p.permute(s.into()).permute(s.invert().into()));
            }

            for s in all::<MirrorUDSymmetry>() {
                assert_eq!(p, p.permute(s.into()).permute(s.invert().into()));
            }

            for s in all::<RotationalSymmetry>() {
                assert_eq!(p, p.permute(s.into()).permute(s.invert().into()));
            }

            for s in all::<PlanarSymmetry>() {
                assert_eq!(p, p.permute(s.into()).permute(s.invert().into()));
            }
        }
    }

    #[test]
    fn perm_and_turn_no_symmetries_are_equivalent() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s in all::<NoSymmetry>() {
            for pi in all::<TetrahedralTrapezoidsIndex>() {
                let p: TetrahedralTrapezoids = pi.into();
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
            for pi in all::<TetrahedralTrapezoidsIndex>() {
                let p: TetrahedralTrapezoids = pi.into();
                for t in all::<Turns>() {
                    let after_permute = p.permute(t.into()).get_equivalent(&s);
                    let before_permute = p.get_equivalent(&s).permute(t.get_equivalent(&s).into());
                    assert_eq!(after_permute, before_permute)
                }
            }
        }
    }

    #[test]
    fn perm_and_turn_rotational_symmetries_are_equivalent() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s in all::<RotationalSymmetry>() {
            for pi in all::<TetrahedralTrapezoidsIndex>() {
                let p: TetrahedralTrapezoids = pi.into();
                for t in all::<Turns>() {
                    let after_permute = p.permute(t.into()).get_equivalent(&s);
                    let before_permute = p.get_equivalent(&s).permute(t.get_equivalent(&s).into());
                    assert_eq!(after_permute, before_permute)
                }
            }
        }
    }

    #[test]
    fn perm_and_turn_planar_symmetries_are_equivalent() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s in all::<PlanarSymmetry>() {
            for pi in all::<TetrahedralTrapezoidsIndex>() {
                let p: TetrahedralTrapezoids = pi.into();
                for t in all::<Turns>() {
                    let after_permute = p.permute(t.into()).get_equivalent(&s);
                    let before_permute = p.get_equivalent(&s).permute(t.get_equivalent(&s).into());
                    assert_eq!(after_permute, before_permute)
                }
            }
        }
    }

    #[test]
    fn direct_and_indirect_sym_multiplication_are_equivalent_for_planar_symmetry() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s0 in all::<PlanarSymmetry>() {
            for s1 in all::<PlanarSymmetry>() {
                for pi in all::<TetrahedralTrapezoidsIndex>() {
                    let p: TetrahedralTrapezoids = pi.into();
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
                for pi in all::<TetrahedralTrapezoidsIndex>() {
                    let p: TetrahedralTrapezoids = pi.into();
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
                for pi in all::<TetrahedralTrapezoidsIndex>() {
                    let p: TetrahedralTrapezoids = pi.into();
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
                for pi in all::<TetrahedralTrapezoidsIndex>() {
                    let p: TetrahedralTrapezoids = pi.into();
                    let as_tt = p.permute(s0.into()).permute(s1.into());
                    let as_sym = p.permute(s0.permute(s1).into());
                    assert_eq!(as_tt, as_sym);
                }
            }
        }
    }

    #[test]
    fn into_and_planar_symmetry_commute_on_turns() {
        for s in all::<PlanarSymmetry>() {
            for t in all::<Turns>() {
                assert_eq!(Into::<TetrahedralTrapezoids>::into(t).get_equivalent(&s), t.get_equivalent(&s).into())
            }
        }
    }

    #[test]
    fn into_and_mirror_ud_symmetry_commute_on_turns() {
        for s in all::<MirrorUDSymmetry>() {
            for t in all::<Turns>() {
                assert_eq!(Into::<TetrahedralTrapezoids>::into(t).get_equivalent(&s), t.get_equivalent(&s).into())
            }
        }
    }

    #[test]
    fn into_and_rotational_symmetry_commute_on_turns() {
        for s in all::<RotationalSymmetry>() {
            for t in all::<Turns>() {
                assert_eq!(Into::<TetrahedralTrapezoids>::into(t).get_equivalent(&s), t.get_equivalent(&s).into())
            }
        }
    }

    #[test]
    fn into_and_no_symmetry_commute_on_turns() {
        for s in all::<NoSymmetry>() {
            for t in all::<Turns>() {
                assert_eq!(Into::<TetrahedralTrapezoids>::into(t).get_equivalent(&s), t.get_equivalent(&s).into())
            }
        }
    }

    #[test]
    fn no_symmetry_permutation_is_associative() {
        for p0 in all::<NoSymmetry>() {
            for p1 in all::<NoSymmetry>() {
                for p2 in all::<NoSymmetry>() {
                    assert_eq!(p0.permute(p1).permute(p2), p0.permute(p1.permute(p2)))
                }
            }
        }
    }

    #[test]
    fn no_symmetry_identity_has_no_effect() {
        for p in all::<NoSymmetry>() {
            assert_eq!(p.permute(NoSymmetry::identity()) == p, NoSymmetry::identity().permute(p) == p)
        }
    }

    #[test]
    fn no_symmetry_inversion_is_identity() {
        for p in all::<NoSymmetry>() {
            assert_eq!(p.permute(p.invert()), NoSymmetry::identity())
        }
    }

    #[test]
    fn mirror_ud_symmetry_permutation_is_associative() {
        for p0 in all::<MirrorUDSymmetry>() {
            for p1 in all::<MirrorUDSymmetry>() {
                for p2 in all::<MirrorUDSymmetry>() {
                    assert_eq!(p0.permute(p1).permute(p2), p0.permute(p1.permute(p2)))
                }
            }
        }
    }

    #[test]
    fn mirror_ud_symmetry_identity_has_no_effect() {
        for p in all::<MirrorUDSymmetry>() {
            assert_eq!(p.permute(MirrorUDSymmetry::identity()) == p, MirrorUDSymmetry::identity().permute(p) == p)
        }
    }

    #[test]
    fn mirror_ud_symmetry_inversion_is_identity() {
        for p in all::<MirrorUDSymmetry>() {
            assert_eq!(p.permute(p.invert()), MirrorUDSymmetry::identity())
        }
    }

    #[test]
    fn rotational_symmetry_permutation_is_associative() {
        for p0 in all::<RotationalSymmetry>() {
            for p1 in all::<RotationalSymmetry>() {
                for p2 in all::<RotationalSymmetry>() {
                    assert_eq!(p0.permute(p1).permute(p2), p0.permute(p1.permute(p2)))
                }
            }
        }
    }

    #[test]
    fn rotational_symmetry_identity_has_no_effect() {
        for p in all::<RotationalSymmetry>() {
            assert_eq!(p.permute(RotationalSymmetry::identity()) == p, RotationalSymmetry::identity().permute(p) == p)
        }
    }

    #[test]
    fn rotational_symmetry_inversion_is_identity() {
        for p in all::<RotationalSymmetry>() {
            assert_eq!(p.permute(p.invert()), RotationalSymmetry::identity())
        }
    }

    #[test]
    fn planar_symmetry_permutation_is_associative() {
        for p0 in all::<PlanarSymmetry>() {
            for p1 in all::<PlanarSymmetry>() {
                for p2 in all::<PlanarSymmetry>() {
                    assert_eq!(p0.permute(p1).permute(p2), p0.permute(p1.permute(p2)))
                }
            }
        }
    }

    #[test]
    fn planar_symmetry_identity_has_no_effect() {
        for p in all::<PlanarSymmetry>() {
            assert_eq!(p.permute(PlanarSymmetry::identity()) == p, PlanarSymmetry::identity().permute(p) == p)
        }
    }

    #[test]
    fn planar_symmetry_inversion_is_identity() {
        for p in all::<PlanarSymmetry>() {
            assert_eq!(p.permute(p.invert()), PlanarSymmetry::identity())
        }
    }
}
