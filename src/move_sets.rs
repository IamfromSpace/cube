extern crate im;

pub mod quarter_turns {
    #[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
    pub enum QuarterTurn {
        U,
        UPrime,
        F,
        FPrime,
        R,
        RPrime,
        B,
        BPrime,
        L,
        LPrime,
        D,
        DPrime,
    }

    use super::super::equivalence_class::EquivalenceClass;
    use super::symmetry_generators::SymmetryGenerator;
    impl EquivalenceClass<SymmetryGenerator> for QuarterTurn {
        fn get_equivalent(self, sym: &SymmetryGenerator) -> QuarterTurn {
            match self {
                QuarterTurn::U => match sym {
                    SymmetryGenerator::SUrf => QuarterTurn::R,
                    SymmetryGenerator::SF => QuarterTurn::D,
                    SymmetryGenerator::SU => QuarterTurn::U,
                    SymmetryGenerator::SMrl => QuarterTurn::UPrime,
                },
                QuarterTurn::UPrime => match sym {
                    SymmetryGenerator::SUrf => QuarterTurn::RPrime,
                    SymmetryGenerator::SF => QuarterTurn::DPrime,
                    SymmetryGenerator::SU => QuarterTurn::UPrime,
                    SymmetryGenerator::SMrl => QuarterTurn::U,
                },
                QuarterTurn::F => match sym {
                    SymmetryGenerator::SUrf => QuarterTurn::U,
                    SymmetryGenerator::SF => QuarterTurn::F,
                    SymmetryGenerator::SU => QuarterTurn::L,
                    SymmetryGenerator::SMrl => QuarterTurn::FPrime,
                },
                QuarterTurn::FPrime => match sym {
                    SymmetryGenerator::SUrf => QuarterTurn::UPrime,
                    SymmetryGenerator::SF => QuarterTurn::FPrime,
                    SymmetryGenerator::SU => QuarterTurn::LPrime,
                    SymmetryGenerator::SMrl => QuarterTurn::F,
                },
                QuarterTurn::R => match sym {
                    SymmetryGenerator::SUrf => QuarterTurn::F,
                    SymmetryGenerator::SF => QuarterTurn::L,
                    SymmetryGenerator::SU => QuarterTurn::F,
                    SymmetryGenerator::SMrl => QuarterTurn::LPrime,
                },
                QuarterTurn::RPrime => match sym {
                    SymmetryGenerator::SUrf => QuarterTurn::FPrime,
                    SymmetryGenerator::SF => QuarterTurn::LPrime,
                    SymmetryGenerator::SU => QuarterTurn::FPrime,
                    SymmetryGenerator::SMrl => QuarterTurn::L,
                },
                QuarterTurn::B => match sym {
                    SymmetryGenerator::SUrf => QuarterTurn::D,
                    SymmetryGenerator::SF => QuarterTurn::B,
                    SymmetryGenerator::SU => QuarterTurn::R,
                    SymmetryGenerator::SMrl => QuarterTurn::BPrime,
                },
                QuarterTurn::BPrime => match sym {
                    SymmetryGenerator::SUrf => QuarterTurn::DPrime,
                    SymmetryGenerator::SF => QuarterTurn::BPrime,
                    SymmetryGenerator::SU => QuarterTurn::RPrime,
                    SymmetryGenerator::SMrl => QuarterTurn::B,
                },
                QuarterTurn::L => match sym {
                    SymmetryGenerator::SUrf => QuarterTurn::B,
                    SymmetryGenerator::SF => QuarterTurn::R,
                    SymmetryGenerator::SU => QuarterTurn::B,
                    SymmetryGenerator::SMrl => QuarterTurn::RPrime,
                },
                QuarterTurn::LPrime => match sym {
                    SymmetryGenerator::SUrf => QuarterTurn::BPrime,
                    SymmetryGenerator::SF => QuarterTurn::RPrime,
                    SymmetryGenerator::SU => QuarterTurn::BPrime,
                    SymmetryGenerator::SMrl => QuarterTurn::R,
                },
                QuarterTurn::D => match sym {
                    SymmetryGenerator::SUrf => QuarterTurn::L,
                    SymmetryGenerator::SF => QuarterTurn::U,
                    SymmetryGenerator::SU => QuarterTurn::D,
                    SymmetryGenerator::SMrl => QuarterTurn::DPrime,
                },
                QuarterTurn::DPrime => match sym {
                    SymmetryGenerator::SUrf => QuarterTurn::LPrime,
                    SymmetryGenerator::SF => QuarterTurn::UPrime,
                    SymmetryGenerator::SU => QuarterTurn::DPrime,
                    SymmetryGenerator::SMrl => QuarterTurn::D,
                },
            }
        }
    }

    use super::g1_symmetry_generators::G1SymmetryGenerator;
    impl EquivalenceClass<G1SymmetryGenerator> for QuarterTurn {
        fn get_equivalent(self, sym: &G1SymmetryGenerator) -> QuarterTurn {
            match self {
                QuarterTurn::U => match sym {
                    G1SymmetryGenerator::SF => QuarterTurn::D,
                    G1SymmetryGenerator::SU => QuarterTurn::U,
                    G1SymmetryGenerator::SMrl => QuarterTurn::UPrime,
                },
                QuarterTurn::UPrime => match sym {
                    G1SymmetryGenerator::SF => QuarterTurn::DPrime,
                    G1SymmetryGenerator::SU => QuarterTurn::UPrime,
                    G1SymmetryGenerator::SMrl => QuarterTurn::U,
                },
                QuarterTurn::F => match sym {
                    G1SymmetryGenerator::SF => QuarterTurn::F,
                    G1SymmetryGenerator::SU => QuarterTurn::L,
                    G1SymmetryGenerator::SMrl => QuarterTurn::FPrime,
                },
                QuarterTurn::FPrime => match sym {
                    G1SymmetryGenerator::SF => QuarterTurn::FPrime,
                    G1SymmetryGenerator::SU => QuarterTurn::LPrime,
                    G1SymmetryGenerator::SMrl => QuarterTurn::F,
                },
                QuarterTurn::R => match sym {
                    G1SymmetryGenerator::SF => QuarterTurn::L,
                    G1SymmetryGenerator::SU => QuarterTurn::F,
                    G1SymmetryGenerator::SMrl => QuarterTurn::LPrime,
                },
                QuarterTurn::RPrime => match sym {
                    G1SymmetryGenerator::SF => QuarterTurn::LPrime,
                    G1SymmetryGenerator::SU => QuarterTurn::FPrime,
                    G1SymmetryGenerator::SMrl => QuarterTurn::L,
                },
                QuarterTurn::B => match sym {
                    G1SymmetryGenerator::SF => QuarterTurn::B,
                    G1SymmetryGenerator::SU => QuarterTurn::R,
                    G1SymmetryGenerator::SMrl => QuarterTurn::BPrime,
                },
                QuarterTurn::BPrime => match sym {
                    G1SymmetryGenerator::SF => QuarterTurn::BPrime,
                    G1SymmetryGenerator::SU => QuarterTurn::RPrime,
                    G1SymmetryGenerator::SMrl => QuarterTurn::B,
                },
                QuarterTurn::L => match sym {
                    G1SymmetryGenerator::SF => QuarterTurn::R,
                    G1SymmetryGenerator::SU => QuarterTurn::B,
                    G1SymmetryGenerator::SMrl => QuarterTurn::RPrime,
                },
                QuarterTurn::LPrime => match sym {
                    G1SymmetryGenerator::SF => QuarterTurn::RPrime,
                    G1SymmetryGenerator::SU => QuarterTurn::BPrime,
                    G1SymmetryGenerator::SMrl => QuarterTurn::R,
                },
                QuarterTurn::D => match sym {
                    G1SymmetryGenerator::SF => QuarterTurn::U,
                    G1SymmetryGenerator::SU => QuarterTurn::D,
                    G1SymmetryGenerator::SMrl => QuarterTurn::DPrime,
                },
                QuarterTurn::DPrime => match sym {
                    G1SymmetryGenerator::SF => QuarterTurn::UPrime,
                    G1SymmetryGenerator::SU => QuarterTurn::DPrime,
                    G1SymmetryGenerator::SMrl => QuarterTurn::D,
                },
            }
        }
    }

    use super::super::invertable::Invertable;
    impl Invertable for QuarterTurn {
        fn invert(&self) -> QuarterTurn {
            match self {
                QuarterTurn::U => QuarterTurn::UPrime,
                QuarterTurn::UPrime => QuarterTurn::U,
                QuarterTurn::F => QuarterTurn::FPrime,
                QuarterTurn::FPrime => QuarterTurn::F,
                QuarterTurn::R => QuarterTurn::RPrime,
                QuarterTurn::RPrime => QuarterTurn::R,
                QuarterTurn::B => QuarterTurn::BPrime,
                QuarterTurn::BPrime => QuarterTurn::B,
                QuarterTurn::L => QuarterTurn::LPrime,
                QuarterTurn::LPrime => QuarterTurn::L,
                QuarterTurn::D => QuarterTurn::DPrime,
                QuarterTurn::DPrime => QuarterTurn::D,
            }
        }
    }

    use super::symmetry_generators::SymGenList;
    impl EquivalenceClass<SymGenList> for QuarterTurn {
        fn get_equivalent(self, sym_gen_list: &SymGenList) -> QuarterTurn {
            let mut t = self;
            for x in &sym_gen_list.0 {
                t = t.get_equivalent(x);
            }
            t
        }
    }

    use super::g1_symmetry_generators::G1SymGenList;
    impl EquivalenceClass<G1SymGenList> for QuarterTurn {
        fn get_equivalent(self, sym_gen_list: &G1SymGenList) -> QuarterTurn {
            let mut t = self;
            for x in &sym_gen_list.0 {
                t = t.get_equivalent(x);
            }
            t
        }
    }
}

pub mod g1_turns {
    #[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
    pub enum G1Turn {
        U,
        UPrime,
        F2,
        R2,
        B2,
        L2,
        D,
        DPrime,
    }

    use super::super::equivalence_class::EquivalenceClass;
    use super::g1_symmetry_generators::G1SymmetryGenerator;
    impl EquivalenceClass<G1SymmetryGenerator> for G1Turn {
        fn get_equivalent(self, sym: &G1SymmetryGenerator) -> G1Turn {
            match self {
                G1Turn::U => match sym {
                    G1SymmetryGenerator::SF => G1Turn::D,
                    G1SymmetryGenerator::SU => G1Turn::U,
                    G1SymmetryGenerator::SMrl => G1Turn::UPrime,
                },
                G1Turn::UPrime => match sym {
                    G1SymmetryGenerator::SF => G1Turn::DPrime,
                    G1SymmetryGenerator::SU => G1Turn::UPrime,
                    G1SymmetryGenerator::SMrl => G1Turn::U,
                },
                G1Turn::F2 => match sym {
                    G1SymmetryGenerator::SF => G1Turn::F2,
                    G1SymmetryGenerator::SU => G1Turn::L2,
                    G1SymmetryGenerator::SMrl => G1Turn::F2,
                },
                G1Turn::R2 => match sym {
                    G1SymmetryGenerator::SF => G1Turn::L2,
                    G1SymmetryGenerator::SU => G1Turn::F2,
                    G1SymmetryGenerator::SMrl => G1Turn::L2,
                },
                G1Turn::B2 => match sym {
                    G1SymmetryGenerator::SF => G1Turn::B2,
                    G1SymmetryGenerator::SU => G1Turn::R2,
                    G1SymmetryGenerator::SMrl => G1Turn::B2,
                },
                G1Turn::L2 => match sym {
                    G1SymmetryGenerator::SF => G1Turn::R2,
                    G1SymmetryGenerator::SU => G1Turn::B2,
                    G1SymmetryGenerator::SMrl => G1Turn::R2,
                },
                G1Turn::D => match sym {
                    G1SymmetryGenerator::SF => G1Turn::U,
                    G1SymmetryGenerator::SU => G1Turn::D,
                    G1SymmetryGenerator::SMrl => G1Turn::DPrime,
                },
                G1Turn::DPrime => match sym {
                    G1SymmetryGenerator::SF => G1Turn::UPrime,
                    G1SymmetryGenerator::SU => G1Turn::DPrime,
                    G1SymmetryGenerator::SMrl => G1Turn::D,
                },
            }
        }
    }

    use super::super::invertable::Invertable;
    impl Invertable for G1Turn {
        fn invert(&self) -> G1Turn {
            match self {
                G1Turn::U => G1Turn::UPrime,
                G1Turn::UPrime => G1Turn::U,
                G1Turn::F2 => G1Turn::F2,
                G1Turn::R2 => G1Turn::R2,
                G1Turn::B2 => G1Turn::B2,
                G1Turn::L2 => G1Turn::L2,
                G1Turn::D => G1Turn::DPrime,
                G1Turn::DPrime => G1Turn::D,
            }
        }
    }

    use super::g1_symmetry_generators::G1SymGenList;
    impl EquivalenceClass<G1SymGenList> for G1Turn {
        fn get_equivalent(self, g1_sym_gen_list: &G1SymGenList) -> G1Turn {
            let mut t = self;
            for x in &g1_sym_gen_list.0 {
                t = t.get_equivalent(x);
            }
            t
        }
    }
}

pub mod symmetry_generators {
    #[derive(Copy, Clone, Debug)]
    pub enum SymmetryGenerator {
        // Create a Clockwise turn of the whole cube on the axis from URF to DBL
        SUrf,
        // Create a 180deg turn of the whole cube on the F face
        SF,
        // Create a Clockwise turn of the whole cube on the U face
        SU,
        // Create a mirror of the whole cube from the left to right side
        SMrl,
    }

    use super::im::Vector;

    // TODO: Ideally the inner would not be public at all
    #[derive(Clone, Debug)]
    pub struct SymGenList(pub Vector<SymmetryGenerator>);

    extern crate functional;
    use super::super::invertable::Invertable;
    use super::super::permutation_group::PermutationGroup;

    impl functional::BinaryOperation<SymGenList> for SymGenList {
        fn apply(a: SymGenList, b: SymGenList) -> SymGenList {
            let mut r = a.0.clone();
            r.extend(b.0);
            SymGenList(r)
        }
    }

    impl functional::AssociativeOperation<SymGenList> for SymGenList { }

    impl functional::Monoid<SymGenList> for SymGenList {
        fn one() -> SymGenList {
            SymGenList(Vector::new())
        }
    }

    impl Invertable for SymGenList {
        fn invert(&self) -> SymGenList {
            let mut r = Vector::new();
            for x in &self.0 {
                match x {
                    SymmetryGenerator::SUrf => {
                        r.push_back(SymmetryGenerator::SUrf);
                        r.push_back(SymmetryGenerator::SUrf);
                    },
                    SymmetryGenerator::SF => {
                        r.push_back(SymmetryGenerator::SF);
                    },
                    SymmetryGenerator::SU => {
                        r.push_back(SymmetryGenerator::SU);
                        r.push_back(SymmetryGenerator::SU);
                        r.push_back(SymmetryGenerator::SU);
                    },
                    SymmetryGenerator::SMrl => {
                        r.push_back(SymmetryGenerator::SMrl);
                    },
                }
            }
            SymGenList(r)
        }
    }

    impl PermutationGroup for SymGenList {}

    impl From<SymmetryGenerator> for SymGenList {
        fn from(x: SymmetryGenerator) -> SymGenList {
            let mut r = Vector::new();
            r.push_front(x);
            SymGenList(r)
        }
    }
}

pub mod g1_symmetry_generators {
    #[derive(Copy, Clone, Debug)]
    pub enum G1SymmetryGenerator {
        // Create a 180deg turn of the whole cube on the F face
        SF,
        // Create a Clockwise turn of the whole cube on the U face
        SU,
        // Create a mirror of the whole cube from the left to right side
        SMrl,
    }

    use super::im::Vector;

    // TODO: Ideally the inner would not be public at all
    #[derive(Clone, Debug)]
    pub struct G1SymGenList(pub Vector<G1SymmetryGenerator>);

    extern crate functional;
    use super::super::invertable::Invertable;
    use super::super::permutation_group::PermutationGroup;

    // This is not a great representation because this list could grow to be huge.
    // We use immutable Vector to take advantage of structural sharing as we
    // concat lists together.
    impl functional::BinaryOperation<G1SymGenList> for G1SymGenList {
        fn apply(a: G1SymGenList, b: G1SymGenList) -> G1SymGenList {
            let mut r = a.0.clone();
            r.extend(b.0);
            G1SymGenList(r)
        }
    }

    impl functional::AssociativeOperation<G1SymGenList> for G1SymGenList { }

    impl functional::Monoid<G1SymGenList> for G1SymGenList {
        fn one() -> G1SymGenList {
            G1SymGenList(Vector::new())
        }
    }

    impl Invertable for G1SymGenList {
        fn invert(&self) -> G1SymGenList {
            let mut r = Vector::new();
            for x in &self.0 {
                match x {
                    G1SymmetryGenerator::SF => {
                        r.push_back(G1SymmetryGenerator::SF);
                    },
                    G1SymmetryGenerator::SU => {
                        r.push_back(G1SymmetryGenerator::SU);
                        r.push_back(G1SymmetryGenerator::SU);
                        r.push_back(G1SymmetryGenerator::SU);
                    },
                    G1SymmetryGenerator::SMrl => {
                        r.push_back(G1SymmetryGenerator::SMrl);
                    },
                }
            }
            G1SymGenList(r)
        }
    }

    impl PermutationGroup for G1SymGenList {}

    impl From<G1SymmetryGenerator> for G1SymGenList {
        fn from(x: G1SymmetryGenerator) -> G1SymGenList {
            let mut r = Vector::new();
            r.push_front(x);
            G1SymGenList(r)
        }
    }
}
