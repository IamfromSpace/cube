pub mod quarter_turns {
    #[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash, Serialize, Deserialize)]
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

pub mod face_turns {
    #[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash, Serialize, Deserialize)]
    pub enum FaceTurn {
        U,
        U2,
        UPrime,
        F,
        F2,
        FPrime,
        R,
        R2,
        RPrime,
        B,
        B2,
        BPrime,
        L,
        L2,
        LPrime,
        D,
        D2,
        DPrime,
    }

    use super::super::equivalence_class::EquivalenceClass;
    use super::symmetry_generators::SymmetryGenerator;
    impl EquivalenceClass<SymmetryGenerator> for FaceTurn {
        fn get_equivalent(self, sym: &SymmetryGenerator) -> FaceTurn {
            match self {
                FaceTurn::U => match sym {
                    SymmetryGenerator::SUrf => FaceTurn::R,
                    SymmetryGenerator::SF => FaceTurn::D,
                    SymmetryGenerator::SU => FaceTurn::U,
                    SymmetryGenerator::SMrl => FaceTurn::UPrime,
                },
                FaceTurn::U2 => match sym {
                    SymmetryGenerator::SUrf => FaceTurn::R2,
                    SymmetryGenerator::SF => FaceTurn::D2,
                    SymmetryGenerator::SU => FaceTurn::U2,
                    SymmetryGenerator::SMrl => FaceTurn::U2,
                },
                FaceTurn::UPrime => match sym {
                    SymmetryGenerator::SUrf => FaceTurn::RPrime,
                    SymmetryGenerator::SF => FaceTurn::DPrime,
                    SymmetryGenerator::SU => FaceTurn::UPrime,
                    SymmetryGenerator::SMrl => FaceTurn::U,
                },
                FaceTurn::F => match sym {
                    SymmetryGenerator::SUrf => FaceTurn::U,
                    SymmetryGenerator::SF => FaceTurn::F,
                    SymmetryGenerator::SU => FaceTurn::L,
                    SymmetryGenerator::SMrl => FaceTurn::FPrime,
                },
                FaceTurn::F2 => match sym {
                    SymmetryGenerator::SUrf => FaceTurn::U2,
                    SymmetryGenerator::SF => FaceTurn::F2,
                    SymmetryGenerator::SU => FaceTurn::L2,
                    SymmetryGenerator::SMrl => FaceTurn::F2,
                },
                FaceTurn::FPrime => match sym {
                    SymmetryGenerator::SUrf => FaceTurn::UPrime,
                    SymmetryGenerator::SF => FaceTurn::FPrime,
                    SymmetryGenerator::SU => FaceTurn::LPrime,
                    SymmetryGenerator::SMrl => FaceTurn::F,
                },
                FaceTurn::R => match sym {
                    SymmetryGenerator::SUrf => FaceTurn::F,
                    SymmetryGenerator::SF => FaceTurn::L,
                    SymmetryGenerator::SU => FaceTurn::F,
                    SymmetryGenerator::SMrl => FaceTurn::LPrime,
                },
                FaceTurn::R2 => match sym {
                    SymmetryGenerator::SUrf => FaceTurn::F2,
                    SymmetryGenerator::SF => FaceTurn::L2,
                    SymmetryGenerator::SU => FaceTurn::F2,
                    SymmetryGenerator::SMrl => FaceTurn::L2,
                },
                FaceTurn::RPrime => match sym {
                    SymmetryGenerator::SUrf => FaceTurn::FPrime,
                    SymmetryGenerator::SF => FaceTurn::LPrime,
                    SymmetryGenerator::SU => FaceTurn::FPrime,
                    SymmetryGenerator::SMrl => FaceTurn::L,
                },
                FaceTurn::B => match sym {
                    SymmetryGenerator::SUrf => FaceTurn::D,
                    SymmetryGenerator::SF => FaceTurn::B,
                    SymmetryGenerator::SU => FaceTurn::R,
                    SymmetryGenerator::SMrl => FaceTurn::BPrime,
                },
                FaceTurn::B2 => match sym {
                    SymmetryGenerator::SUrf => FaceTurn::D2,
                    SymmetryGenerator::SF => FaceTurn::B2,
                    SymmetryGenerator::SU => FaceTurn::R2,
                    SymmetryGenerator::SMrl => FaceTurn::B2,
                },
                FaceTurn::BPrime => match sym {
                    SymmetryGenerator::SUrf => FaceTurn::DPrime,
                    SymmetryGenerator::SF => FaceTurn::BPrime,
                    SymmetryGenerator::SU => FaceTurn::RPrime,
                    SymmetryGenerator::SMrl => FaceTurn::B,
                },
                FaceTurn::L => match sym {
                    SymmetryGenerator::SUrf => FaceTurn::B,
                    SymmetryGenerator::SF => FaceTurn::R,
                    SymmetryGenerator::SU => FaceTurn::B,
                    SymmetryGenerator::SMrl => FaceTurn::RPrime,
                },
                FaceTurn::L2 => match sym {
                    SymmetryGenerator::SUrf => FaceTurn::B2,
                    SymmetryGenerator::SF => FaceTurn::R2,
                    SymmetryGenerator::SU => FaceTurn::B2,
                    SymmetryGenerator::SMrl => FaceTurn::R2,
                },
                FaceTurn::LPrime => match sym {
                    SymmetryGenerator::SUrf => FaceTurn::BPrime,
                    SymmetryGenerator::SF => FaceTurn::RPrime,
                    SymmetryGenerator::SU => FaceTurn::BPrime,
                    SymmetryGenerator::SMrl => FaceTurn::R,
                },
                FaceTurn::D => match sym {
                    SymmetryGenerator::SUrf => FaceTurn::L,
                    SymmetryGenerator::SF => FaceTurn::U,
                    SymmetryGenerator::SU => FaceTurn::D,
                    SymmetryGenerator::SMrl => FaceTurn::DPrime,
                },
                FaceTurn::D2 => match sym {
                    SymmetryGenerator::SUrf => FaceTurn::L2,
                    SymmetryGenerator::SF => FaceTurn::U2,
                    SymmetryGenerator::SU => FaceTurn::D2,
                    SymmetryGenerator::SMrl => FaceTurn::D2,
                },
                FaceTurn::DPrime => match sym {
                    SymmetryGenerator::SUrf => FaceTurn::LPrime,
                    SymmetryGenerator::SF => FaceTurn::UPrime,
                    SymmetryGenerator::SU => FaceTurn::DPrime,
                    SymmetryGenerator::SMrl => FaceTurn::D,
                },
            }
        }
    }

    use super::g1_symmetry_generators::G1SymmetryGenerator;
    impl EquivalenceClass<G1SymmetryGenerator> for FaceTurn {
        fn get_equivalent(self, sym: &G1SymmetryGenerator) -> FaceTurn {
            match self {
                FaceTurn::U => match sym {
                    G1SymmetryGenerator::SF => FaceTurn::D,
                    G1SymmetryGenerator::SU => FaceTurn::U,
                    G1SymmetryGenerator::SMrl => FaceTurn::UPrime,
                },
                FaceTurn::U2 => match sym {
                    G1SymmetryGenerator::SF => FaceTurn::D2,
                    G1SymmetryGenerator::SU => FaceTurn::U2,
                    G1SymmetryGenerator::SMrl => FaceTurn::U2,
                },
                FaceTurn::UPrime => match sym {
                    G1SymmetryGenerator::SF => FaceTurn::DPrime,
                    G1SymmetryGenerator::SU => FaceTurn::UPrime,
                    G1SymmetryGenerator::SMrl => FaceTurn::U,
                },
                FaceTurn::F => match sym {
                    G1SymmetryGenerator::SF => FaceTurn::F,
                    G1SymmetryGenerator::SU => FaceTurn::L,
                    G1SymmetryGenerator::SMrl => FaceTurn::FPrime,
                },
                FaceTurn::F2 => match sym {
                    G1SymmetryGenerator::SF => FaceTurn::F2,
                    G1SymmetryGenerator::SU => FaceTurn::L2,
                    G1SymmetryGenerator::SMrl => FaceTurn::F2,
                },
                FaceTurn::FPrime => match sym {
                    G1SymmetryGenerator::SF => FaceTurn::FPrime,
                    G1SymmetryGenerator::SU => FaceTurn::LPrime,
                    G1SymmetryGenerator::SMrl => FaceTurn::F,
                },
                FaceTurn::R => match sym {
                    G1SymmetryGenerator::SF => FaceTurn::L,
                    G1SymmetryGenerator::SU => FaceTurn::F,
                    G1SymmetryGenerator::SMrl => FaceTurn::LPrime,
                },
                FaceTurn::R2 => match sym {
                    G1SymmetryGenerator::SF => FaceTurn::L2,
                    G1SymmetryGenerator::SU => FaceTurn::F2,
                    G1SymmetryGenerator::SMrl => FaceTurn::L2,
                },
                FaceTurn::RPrime => match sym {
                    G1SymmetryGenerator::SF => FaceTurn::LPrime,
                    G1SymmetryGenerator::SU => FaceTurn::FPrime,
                    G1SymmetryGenerator::SMrl => FaceTurn::L,
                },
                FaceTurn::B => match sym {
                    G1SymmetryGenerator::SF => FaceTurn::B,
                    G1SymmetryGenerator::SU => FaceTurn::R,
                    G1SymmetryGenerator::SMrl => FaceTurn::BPrime,
                },
                FaceTurn::B2 => match sym {
                    G1SymmetryGenerator::SF => FaceTurn::B2,
                    G1SymmetryGenerator::SU => FaceTurn::R2,
                    G1SymmetryGenerator::SMrl => FaceTurn::B2,
                },
                FaceTurn::BPrime => match sym {
                    G1SymmetryGenerator::SF => FaceTurn::BPrime,
                    G1SymmetryGenerator::SU => FaceTurn::RPrime,
                    G1SymmetryGenerator::SMrl => FaceTurn::B,
                },
                FaceTurn::L => match sym {
                    G1SymmetryGenerator::SF => FaceTurn::R,
                    G1SymmetryGenerator::SU => FaceTurn::B,
                    G1SymmetryGenerator::SMrl => FaceTurn::RPrime,
                },
                FaceTurn::L2 => match sym {
                    G1SymmetryGenerator::SF => FaceTurn::R2,
                    G1SymmetryGenerator::SU => FaceTurn::B2,
                    G1SymmetryGenerator::SMrl => FaceTurn::R2,
                },
                FaceTurn::LPrime => match sym {
                    G1SymmetryGenerator::SF => FaceTurn::RPrime,
                    G1SymmetryGenerator::SU => FaceTurn::BPrime,
                    G1SymmetryGenerator::SMrl => FaceTurn::R,
                },
                FaceTurn::D => match sym {
                    G1SymmetryGenerator::SF => FaceTurn::U,
                    G1SymmetryGenerator::SU => FaceTurn::D,
                    G1SymmetryGenerator::SMrl => FaceTurn::DPrime,
                },
                FaceTurn::D2 => match sym {
                    G1SymmetryGenerator::SF => FaceTurn::U2,
                    G1SymmetryGenerator::SU => FaceTurn::D2,
                    G1SymmetryGenerator::SMrl => FaceTurn::D2,
                },
                FaceTurn::DPrime => match sym {
                    G1SymmetryGenerator::SF => FaceTurn::UPrime,
                    G1SymmetryGenerator::SU => FaceTurn::DPrime,
                    G1SymmetryGenerator::SMrl => FaceTurn::D,
                },
            }
        }
    }

    use super::super::invertable::Invertable;
    impl Invertable for FaceTurn {
        fn invert(&self) -> FaceTurn {
            match self {
                FaceTurn::U => FaceTurn::UPrime,
                FaceTurn::U2 => FaceTurn::U2,
                FaceTurn::UPrime => FaceTurn::U,
                FaceTurn::F => FaceTurn::FPrime,
                FaceTurn::F2 => FaceTurn::F2,
                FaceTurn::FPrime => FaceTurn::F,
                FaceTurn::R => FaceTurn::RPrime,
                FaceTurn::R2 => FaceTurn::R2,
                FaceTurn::RPrime => FaceTurn::R,
                FaceTurn::B => FaceTurn::BPrime,
                FaceTurn::B2 => FaceTurn::B2,
                FaceTurn::BPrime => FaceTurn::B,
                FaceTurn::L => FaceTurn::LPrime,
                FaceTurn::L2 => FaceTurn::L2,
                FaceTurn::LPrime => FaceTurn::L,
                FaceTurn::D => FaceTurn::DPrime,
                FaceTurn::D2 => FaceTurn::D2,
                FaceTurn::DPrime => FaceTurn::D,
            }
        }
    }

    use super::symmetry_generators::SymGenList;
    impl EquivalenceClass<SymGenList> for FaceTurn {
        fn get_equivalent(self, sym_gen_list: &SymGenList) -> FaceTurn {
            let mut t = self;
            for x in &sym_gen_list.0 {
                t = t.get_equivalent(x);
            }
            t
        }
    }

    use super::g1_symmetry_generators::G1SymGenList;
    impl EquivalenceClass<G1SymGenList> for FaceTurn {
        fn get_equivalent(self, sym_gen_list: &G1SymGenList) -> FaceTurn {
            let mut t = self;
            for x in &sym_gen_list.0 {
                t = t.get_equivalent(x);
            }
            t
        }
    }
}

pub mod g1_turns {
    #[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash, Serialize, Deserialize)]
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
    #[derive(Copy, Clone, Debug, Serialize, Deserialize)]
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

    use std::collections::VecDeque;

    // TODO: Ideally the inner would not be public at all
    #[derive(Clone, Debug, Serialize, Deserialize)]
    pub struct SymGenList(pub VecDeque<SymmetryGenerator>);

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
            SymGenList(VecDeque::new())
        }
    }

    impl Invertable for SymGenList {
        fn invert(&self) -> SymGenList {
            let mut r = VecDeque::new();
            for x in &self.0 {
                match x {
                    SymmetryGenerator::SUrf => {
                        r.push_front(SymmetryGenerator::SUrf);
                        r.push_front(SymmetryGenerator::SUrf);
                    },
                    SymmetryGenerator::SF => {
                        r.push_front(SymmetryGenerator::SF);
                    },
                    SymmetryGenerator::SU => {
                        r.push_front(SymmetryGenerator::SU);
                        r.push_front(SymmetryGenerator::SU);
                        r.push_front(SymmetryGenerator::SU);
                    },
                    SymmetryGenerator::SMrl => {
                        r.push_front(SymmetryGenerator::SMrl);
                    },
                }
            }
            SymGenList(r)
        }
    }

    impl PermutationGroup for SymGenList {}

    impl From<SymmetryGenerator> for SymGenList {
        fn from(x: SymmetryGenerator) -> SymGenList {
            let mut r = VecDeque::new();
            r.push_front(x);
            SymGenList(r)
        }
    }
}

pub mod g1_symmetry_generators {
    #[derive(Copy, Clone, Debug, Serialize, Deserialize)]
    pub enum G1SymmetryGenerator {
        // Create a 180deg turn of the whole cube on the F face
        SF,
        // Create a Clockwise turn of the whole cube on the U face
        SU,
        // Create a mirror of the whole cube from the left to right side
        SMrl,
    }

    use std::collections::VecDeque;

    // TODO: Ideally the inner would not be public at all
    #[derive(Clone, Debug, Serialize, Deserialize)]
    pub struct G1SymGenList(pub VecDeque<G1SymmetryGenerator>);

    extern crate functional;
    use super::super::invertable::Invertable;
    use super::super::permutation_group::PermutationGroup;

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
            G1SymGenList(VecDeque::new())
        }
    }

    impl Invertable for G1SymGenList {
        fn invert(&self) -> G1SymGenList {
            let mut r = VecDeque::new();
            for x in &self.0 {
                match x {
                    G1SymmetryGenerator::SF => {
                        r.push_front(G1SymmetryGenerator::SF);
                    },
                    G1SymmetryGenerator::SU => {
                        r.push_front(G1SymmetryGenerator::SU);
                        r.push_front(G1SymmetryGenerator::SU);
                        r.push_front(G1SymmetryGenerator::SU);
                    },
                    G1SymmetryGenerator::SMrl => {
                        r.push_front(G1SymmetryGenerator::SMrl);
                    },
                }
            }
            G1SymGenList(r)
        }
    }

    impl PermutationGroup for G1SymGenList {}

    impl From<G1SymmetryGenerator> for G1SymGenList {
        fn from(x: G1SymmetryGenerator) -> G1SymGenList {
            let mut r = VecDeque::new();
            r.push_front(x);
            G1SymGenList(r)
        }
    }
}
