pub mod quarter_turns {
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
}

pub mod g1_turns {
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
}

pub mod symmetry_generators {
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
}

pub mod g1_symmetry_generators {
    pub enum G1SymmetryGenerator {
        // Create a 180deg turn of the whole cube on the F face
        SF,
        // Create a Clockwise turn of the whole cube on the U face
        SU,
        // Create a mirror of the whole cube from the left to right side
        SMrl,
    }
}
