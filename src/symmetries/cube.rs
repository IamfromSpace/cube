use permutation_group::PermutationGroup as PG;
use invertable::Invertable;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash, Sequence)]
#[repr(u8)]
pub enum MRLSymmetry {
    Identity,
    Mirror,
}

impl Into<usize> for MRLSymmetry {
    fn into(self) -> usize {
        self as usize
    }
}

impl functional::BinaryOperation<MRLSymmetry> for MRLSymmetry {
    fn apply(a: MRLSymmetry, b: MRLSymmetry) -> MRLSymmetry {
        match (a, b) {
            (MRLSymmetry::Identity, MRLSymmetry::Identity) => MRLSymmetry::Identity,
            (MRLSymmetry::Identity, MRLSymmetry::Mirror) => MRLSymmetry::Mirror,
            (MRLSymmetry::Mirror, MRLSymmetry::Identity) => MRLSymmetry::Mirror,
            (MRLSymmetry::Mirror, MRLSymmetry::Mirror) => MRLSymmetry::Identity,
        }
    }
}

impl functional::AssociativeOperation<MRLSymmetry> for MRLSymmetry { }

impl functional::Monoid<MRLSymmetry> for MRLSymmetry {
    fn one() -> MRLSymmetry {
        MRLSymmetry::Identity
    }
}

impl Invertable for MRLSymmetry {
    fn invert(&self) -> MRLSymmetry {
        match self {
            MRLSymmetry::Identity => MRLSymmetry::Identity,
            MRLSymmetry::Mirror => MRLSymmetry::Mirror,
        }
    }
}

impl PG for MRLSymmetry {}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash, Sequence)]
#[repr(u8)]
pub enum U2Symmetry {
    Identity,
    U2,
}

impl Into<usize> for U2Symmetry {
    fn into(self) -> usize {
        self as usize
    }
}

impl functional::BinaryOperation<U2Symmetry> for U2Symmetry {
    fn apply(a: U2Symmetry, b: U2Symmetry) -> U2Symmetry {
        match (a, b) {
            (U2Symmetry::Identity, U2Symmetry::Identity) => U2Symmetry::Identity,
            (U2Symmetry::Identity, U2Symmetry::U2) => U2Symmetry::U2,
            (U2Symmetry::U2, U2Symmetry::Identity) => U2Symmetry::U2,
            (U2Symmetry::U2, U2Symmetry::U2) => U2Symmetry::Identity,
        }
    }
}

impl functional::AssociativeOperation<U2Symmetry> for U2Symmetry { }

impl functional::Monoid<U2Symmetry> for U2Symmetry {
    fn one() -> U2Symmetry {
        U2Symmetry::Identity
    }
}

impl Invertable for U2Symmetry {
    fn invert(&self) -> U2Symmetry {
        match self {
            U2Symmetry::Identity => U2Symmetry::Identity,
            U2Symmetry::U2 => U2Symmetry::U2,
        }
    }
}

impl PG for U2Symmetry {}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash, Sequence)]
#[repr(u8)]
pub enum U2F2Symmetry {
    Identity,
    U2,
    F2,
    U2F2,
}

impl Into<usize> for U2F2Symmetry {
    fn into(self) -> usize {
        self as usize
    }
}

impl functional::BinaryOperation<U2F2Symmetry> for U2F2Symmetry {
    fn apply(a: U2F2Symmetry, b: U2F2Symmetry) -> U2F2Symmetry {
        match (a, b) {
            (U2F2Symmetry::Identity, U2F2Symmetry::Identity) => U2F2Symmetry::Identity,
            (U2F2Symmetry::Identity, U2F2Symmetry::U2) => U2F2Symmetry::U2,
            (U2F2Symmetry::Identity, U2F2Symmetry::F2) => U2F2Symmetry::F2,
            (U2F2Symmetry::Identity, U2F2Symmetry::U2F2) => U2F2Symmetry::U2F2,

            (U2F2Symmetry::U2, U2F2Symmetry::Identity) => U2F2Symmetry::U2,
            (U2F2Symmetry::U2, U2F2Symmetry::U2) => U2F2Symmetry::Identity,
            (U2F2Symmetry::U2, U2F2Symmetry::F2) => U2F2Symmetry::U2F2,
            (U2F2Symmetry::U2, U2F2Symmetry::U2F2) => U2F2Symmetry::F2,

            (U2F2Symmetry::F2, U2F2Symmetry::Identity) => U2F2Symmetry::F2,
            (U2F2Symmetry::F2, U2F2Symmetry::U2) => U2F2Symmetry::U2F2,
            (U2F2Symmetry::F2, U2F2Symmetry::F2) => U2F2Symmetry::Identity,
            (U2F2Symmetry::F2, U2F2Symmetry::U2F2) => U2F2Symmetry::U2,

            (U2F2Symmetry::U2F2, U2F2Symmetry::Identity) => U2F2Symmetry::U2F2,
            (U2F2Symmetry::U2F2, U2F2Symmetry::U2) => U2F2Symmetry::F2,
            (U2F2Symmetry::U2F2, U2F2Symmetry::F2) => U2F2Symmetry::U2,
            (U2F2Symmetry::U2F2, U2F2Symmetry::U2F2) => U2F2Symmetry::Identity,
        }
    }
}

impl functional::AssociativeOperation<U2F2Symmetry> for U2F2Symmetry { }

impl functional::Monoid<U2F2Symmetry> for U2F2Symmetry {
    fn one() -> U2F2Symmetry {
        U2F2Symmetry::Identity
    }
}

impl Invertable for U2F2Symmetry {
    fn invert(&self) -> U2F2Symmetry {
        match self {
            U2F2Symmetry::Identity => U2F2Symmetry::Identity,
            U2F2Symmetry::U2 => U2F2Symmetry::U2,
            U2F2Symmetry::F2 => U2F2Symmetry::F2,
            U2F2Symmetry::U2F2 => U2F2Symmetry::U2F2,
        }
    }
}

impl PG for U2F2Symmetry {}


#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash, Sequence)]
#[repr(u8)]
pub enum UF2Symmetry {
    Identity,
    U,
    U2,
    UPrime,
    F2,
    UF2,
    U2F2,
    UPrimeF2,
}

impl Into<usize> for UF2Symmetry {
    fn into(self) -> usize {
        self as usize
    }
}

impl functional::BinaryOperation<UF2Symmetry> for UF2Symmetry {
    fn apply(a: UF2Symmetry, b: UF2Symmetry) -> UF2Symmetry {
        match (a, b) {
            (UF2Symmetry::Identity, UF2Symmetry::Identity) => UF2Symmetry::Identity,
            (UF2Symmetry::Identity, UF2Symmetry::U) => UF2Symmetry::U,
            (UF2Symmetry::Identity, UF2Symmetry::U2) => UF2Symmetry::U2,
            (UF2Symmetry::Identity, UF2Symmetry::UPrime) => UF2Symmetry::UPrime,
            (UF2Symmetry::Identity, UF2Symmetry::F2) => UF2Symmetry::F2,
            (UF2Symmetry::Identity, UF2Symmetry::UF2) => UF2Symmetry::UF2,
            (UF2Symmetry::Identity, UF2Symmetry::U2F2) => UF2Symmetry::U2F2,
            (UF2Symmetry::Identity, UF2Symmetry::UPrimeF2) => UF2Symmetry::UPrimeF2,

            (UF2Symmetry::U, UF2Symmetry::Identity) => UF2Symmetry::U,
            (UF2Symmetry::U, UF2Symmetry::U) => UF2Symmetry::U2,
            (UF2Symmetry::U, UF2Symmetry::U2) => UF2Symmetry::UPrime,
            (UF2Symmetry::U, UF2Symmetry::UPrime) => UF2Symmetry::Identity,
            (UF2Symmetry::U, UF2Symmetry::F2) => UF2Symmetry::UF2,
            (UF2Symmetry::U, UF2Symmetry::UF2) => UF2Symmetry::U2F2,
            (UF2Symmetry::U, UF2Symmetry::U2F2) => UF2Symmetry::UPrimeF2,
            (UF2Symmetry::U, UF2Symmetry::UPrimeF2) => UF2Symmetry::F2,

            (UF2Symmetry::U2, UF2Symmetry::Identity) => UF2Symmetry::U2,
            (UF2Symmetry::U2, UF2Symmetry::U) => UF2Symmetry::UPrime,
            (UF2Symmetry::U2, UF2Symmetry::U2) => UF2Symmetry::Identity,
            (UF2Symmetry::U2, UF2Symmetry::UPrime) => UF2Symmetry::U,
            (UF2Symmetry::U2, UF2Symmetry::F2) => UF2Symmetry::U2F2,
            (UF2Symmetry::U2, UF2Symmetry::UF2) => UF2Symmetry::UPrimeF2,
            (UF2Symmetry::U2, UF2Symmetry::U2F2) => UF2Symmetry::F2,
            (UF2Symmetry::U2, UF2Symmetry::UPrimeF2) => UF2Symmetry::UF2,

            (UF2Symmetry::UPrime, UF2Symmetry::Identity) => UF2Symmetry::UPrime,
            (UF2Symmetry::UPrime, UF2Symmetry::U) => UF2Symmetry::Identity,
            (UF2Symmetry::UPrime, UF2Symmetry::U2) => UF2Symmetry::U,
            (UF2Symmetry::UPrime, UF2Symmetry::UPrime) => UF2Symmetry::U2,
            (UF2Symmetry::UPrime, UF2Symmetry::F2) => UF2Symmetry::UPrimeF2,
            (UF2Symmetry::UPrime, UF2Symmetry::UF2) => UF2Symmetry::F2,
            (UF2Symmetry::UPrime, UF2Symmetry::U2F2) => UF2Symmetry::UF2,
            (UF2Symmetry::UPrime, UF2Symmetry::UPrimeF2) => UF2Symmetry::U2F2,

            (UF2Symmetry::F2, UF2Symmetry::Identity) => UF2Symmetry::F2,
            (UF2Symmetry::F2, UF2Symmetry::U) => UF2Symmetry::UPrimeF2,
            (UF2Symmetry::F2, UF2Symmetry::U2) => UF2Symmetry::U2F2,
            (UF2Symmetry::F2, UF2Symmetry::UPrime) => UF2Symmetry::UF2,
            (UF2Symmetry::F2, UF2Symmetry::F2) => UF2Symmetry::Identity,
            (UF2Symmetry::F2, UF2Symmetry::UF2) => UF2Symmetry::UPrime,
            (UF2Symmetry::F2, UF2Symmetry::U2F2) => UF2Symmetry::U2,
            (UF2Symmetry::F2, UF2Symmetry::UPrimeF2) => UF2Symmetry::U,

            (UF2Symmetry::UF2, UF2Symmetry::Identity) => UF2Symmetry::UF2,
            (UF2Symmetry::UF2, UF2Symmetry::U) => UF2Symmetry::F2,
            (UF2Symmetry::UF2, UF2Symmetry::U2) => UF2Symmetry::UPrimeF2,
            (UF2Symmetry::UF2, UF2Symmetry::UPrime) => UF2Symmetry::U2F2,
            (UF2Symmetry::UF2, UF2Symmetry::F2) => UF2Symmetry::U,
            (UF2Symmetry::UF2, UF2Symmetry::UF2) => UF2Symmetry::Identity,
            (UF2Symmetry::UF2, UF2Symmetry::U2F2) => UF2Symmetry::UPrime,
            (UF2Symmetry::UF2, UF2Symmetry::UPrimeF2) => UF2Symmetry::U2,

            (UF2Symmetry::U2F2, UF2Symmetry::Identity) => UF2Symmetry::U2F2,
            (UF2Symmetry::U2F2, UF2Symmetry::U) => UF2Symmetry::UF2,
            (UF2Symmetry::U2F2, UF2Symmetry::U2) => UF2Symmetry::F2,
            (UF2Symmetry::U2F2, UF2Symmetry::UPrime) => UF2Symmetry::UPrimeF2,
            (UF2Symmetry::U2F2, UF2Symmetry::F2) => UF2Symmetry::U2,
            (UF2Symmetry::U2F2, UF2Symmetry::UF2) => UF2Symmetry::U,
            (UF2Symmetry::U2F2, UF2Symmetry::U2F2) => UF2Symmetry::Identity,
            (UF2Symmetry::U2F2, UF2Symmetry::UPrimeF2) => UF2Symmetry::UPrime,

            (UF2Symmetry::UPrimeF2, UF2Symmetry::Identity) => UF2Symmetry::UPrimeF2,
            (UF2Symmetry::UPrimeF2, UF2Symmetry::U) => UF2Symmetry::U2F2,
            (UF2Symmetry::UPrimeF2, UF2Symmetry::U2) => UF2Symmetry::UF2,
            (UF2Symmetry::UPrimeF2, UF2Symmetry::UPrime) => UF2Symmetry::F2,
            (UF2Symmetry::UPrimeF2, UF2Symmetry::F2) => UF2Symmetry::UPrime,
            (UF2Symmetry::UPrimeF2, UF2Symmetry::UF2) => UF2Symmetry::U2,
            (UF2Symmetry::UPrimeF2, UF2Symmetry::U2F2) => UF2Symmetry::U,
            (UF2Symmetry::UPrimeF2, UF2Symmetry::UPrimeF2) => UF2Symmetry::Identity,
        }
    }
}

impl functional::AssociativeOperation<UF2Symmetry> for UF2Symmetry { }

impl functional::Monoid<UF2Symmetry> for UF2Symmetry {
    fn one() -> UF2Symmetry {
        UF2Symmetry::Identity
    }
}

impl Invertable for UF2Symmetry {
    fn invert(&self) -> UF2Symmetry {
        match self {
            UF2Symmetry::Identity => UF2Symmetry::Identity,
            UF2Symmetry::U => UF2Symmetry::UPrime,
            UF2Symmetry::U2 => UF2Symmetry::U2,
            UF2Symmetry::UPrime => UF2Symmetry::U,
            UF2Symmetry::F2 => UF2Symmetry::F2,
            UF2Symmetry::UF2 => UF2Symmetry::UF2,
            UF2Symmetry::U2F2 => UF2Symmetry::U2F2,
            UF2Symmetry::UPrimeF2 => UF2Symmetry::UPrimeF2,
        }
    }
}

impl PG for UF2Symmetry {}

#[cfg(test)]
mod tests {
    use super::*;
    use enum_iterator::all;
    use quickcheck::Gen;
    use rand::Rng;

    impl quickcheck::Arbitrary for MRLSymmetry {
        fn arbitrary<G: Gen>(g: &mut G) -> MRLSymmetry {
            *g.choose(&all::<MRLSymmetry>().collect::<Vec<_>>()).unwrap()
        }
    }

    impl quickcheck::Arbitrary for U2Symmetry {
        fn arbitrary<G: Gen>(g: &mut G) -> U2Symmetry {
            *g.choose(&all::<U2Symmetry>().collect::<Vec<_>>()).unwrap()
        }
    }

    impl quickcheck::Arbitrary for U2F2Symmetry {
        fn arbitrary<G: Gen>(g: &mut G) -> U2F2Symmetry {
            *g.choose(&all::<U2F2Symmetry>().collect::<Vec<_>>()).unwrap()
        }
    }

    impl quickcheck::Arbitrary for UF2Symmetry {
        fn arbitrary<G: Gen>(g: &mut G) -> UF2Symmetry {
            *g.choose(&all::<UF2Symmetry>().collect::<Vec<_>>()).unwrap()
        }
    }

    quickcheck! {
        fn mrl_symmetry_permutation_is_associative(p0: MRLSymmetry, p1: MRLSymmetry, p2: MRLSymmetry) -> bool {
            p0.permute(p1).permute(p2) == p0.permute(p1.permute(p2))
        }
    }

    quickcheck! {
        fn mrl_symmetry_identity_has_no_effect(p: MRLSymmetry) -> bool {
            p.permute(MRLSymmetry::identity()) == p
                && MRLSymmetry::identity().permute(p) == p
        }
    }

    quickcheck! {
        fn mrl_symmetry_inversion_is_identity(p: MRLSymmetry) -> bool {
            p.permute(p.invert()) == MRLSymmetry::identity()
        }
    }

    quickcheck! {
        fn u2_symmetry_permutation_is_associative(p0: U2Symmetry, p1: U2Symmetry, p2: U2Symmetry) -> bool {
            p0.permute(p1).permute(p2) == p0.permute(p1.permute(p2))
        }
    }

    quickcheck! {
        fn u2_symmetry_identity_has_no_effect(p: U2Symmetry) -> bool {
            p.permute(U2Symmetry::identity()) == p
                && U2Symmetry::identity().permute(p) == p
        }
    }

    quickcheck! {
        fn u2_symmetry_inversion_is_identity(p: U2Symmetry) -> bool {
            p.permute(p.invert()) == U2Symmetry::identity()
        }
    }

    quickcheck! {
        fn uf2_symmetry_permutation_is_associative(p0: UF2Symmetry, p1: UF2Symmetry, p2: UF2Symmetry) -> bool {
            p0.permute(p1).permute(p2) == p0.permute(p1.permute(p2))
        }
    }

    quickcheck! {
        fn uf2_symmetry_identity_has_no_effect(p: UF2Symmetry) -> bool {
            p.permute(UF2Symmetry::identity()) == p
                && UF2Symmetry::identity().permute(p) == p
        }
    }

    quickcheck! {
        fn uf2_symmetry_inversion_is_identity(p: UF2Symmetry) -> bool {
            p.permute(p.invert()) == UF2Symmetry::identity()
        }
    }

    quickcheck! {
        fn u2f2_symmetry_permutation_is_associative(p0: U2F2Symmetry, p1: U2F2Symmetry, p2: U2F2Symmetry) -> bool {
            p0.permute(p1).permute(p2) == p0.permute(p1.permute(p2))
        }
    }

    quickcheck! {
        fn u2f2_symmetry_identity_has_no_effect(p: U2F2Symmetry) -> bool {
            p.permute(U2F2Symmetry::identity()) == p
                && U2F2Symmetry::identity().permute(p) == p
        }
    }

    quickcheck! {
        fn u2f2_symmetry_inversion_is_identity(p: U2F2Symmetry) -> bool {
            p.permute(p.invert()) == U2F2Symmetry::identity()
        }
    }
}
