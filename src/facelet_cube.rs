extern crate ansi_term;
extern crate functional;

use super::permutation_group::PermutationGroup;
use super::invertable::Invertable;

use std::fmt;

/* Basically everything about a cube is a permutation.
 * A cube position is a permutation.  A move is a permutation.
 * An algorithm is a permutation.  Rotating the whole cube
 * is a permutation.
 *
 * Permutations put simply: the thing in position X moves to position Y.
 *
 * So when we turn the top face clockwise, the facelet (sticker) in the
 * upper right corner moves to the bottom right corner.  If we descibe
 * how all the facelets move, then we've describe the permutation that
 * is the turning of the top face.
 *
 * The "solved" position is simply the identity permutation: everything
 * moves from its current position to that same position.
 *
 * When viewed in terms of permutations, "solving" the cube becomes
 * somewhat non-sensical.  How does one "solve" a position?  One simply
 * takes the inverse of the "scramble" permutation, done!
 *
 * I = X * X'
 *
 * It's fairly boring!
 *
 * When we talk about "solving" a cube, what we really mean is de-composing
 * a permutation into some restricted set of other permutations.
 *
 * so:
 * QuarterTurns = { U, U', F, F', ... }
 * HalfTurns = { U, U2, U', F, F2, F', ... }
 * SliceTurns = { U, U2, U', M, F, F2, F', ... }
 *
 * Permutations compose nicely, because if:
 * A moves X to Y
 * B moves Y to Z
 * C = A * B => C moves X to Z
 *
 * so we can combine our restricted sets to make other permutations:
 * X = U * R * F' * ...
 *
 * D moves Z to X
 * D' = A * B * C
 *
 * So really the question is: can we express one permutation as a series
 * of another from a restricted set?
 *
 *
 * The permutations of the cube can be represented in many ways, this
 * particular encodes the facelets.  Each edge facelet and corner facelet
 * position are given indexes, and then we have an array of that holds
 * and index for each index.
 *
 * We can read the resulting structure to mean:
 * The facelet at position edges[i] moves to position i.
 * The facelet at position corners[i] moves to position i.
 *
 * This is a fairly fast way to represent the cube!  But it's not as
 * memory efficient as ones that compact the positions more.
 *
 * Minimum space in memory for cube positions is 66bits,
 * which rounds up to 9bytes when byte addressable.
 * To compact this more in a still fairly usable way we can do:
 * edge position as u32 (3.2 "wasted" bits)
 * edge orientation as u16 (4 "wasted" bits)
 * corner position as u16 (0.7 "wasted" bits)
 * corner orientation as u16 (3.3 "wasted" bits)
 *
 * This only costs 10 bytes on memory, which is only 1 more than optimal
 *
 * A theoretically optimal way:
 * bottom/middle edge orientations as u8 (no "wasted" bytes)
 * everything else: u64 (6.5 "wasted" bytes)
 *
 * To handle all the ways that cubes can be represented an permuted,
 * it may make sense to make this a "Permute" trait.
 * It seems like requirements would be: identity, invert, permute
 */
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct FaceletCube {
    // u8 benchmarked as fastest for permuting
    pub corners: [u8; 24],
    pub edges: [u8; 24],
}

impl fmt::Display for FaceletCube {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::ansi_term::Colour::*;
        let w = White.paint("██");
        let g = Green.paint("██");
        let r = Red.paint("██");
        let b = Blue.paint("██");
        let o = RGB(250, 48, 11).paint("██");
        let y = Yellow.paint("██");
        let c = |id: usize| match self.corners[id] >> 2 {
            0 => &w,
            1 => &g,
            2 => &r,
            3 => &b,
            4 => &o,
            5 => &y,
            _ => panic!("could not match color"),
        };
        let e = |id: usize| match self.edges[id] >> 2 {
            0 => &w,
            1 => &g,
            2 => &r,
            3 => &b,
            4 => &o,
            5 => &y,
            _ => panic!("could not match color"),
        };
        write!(
            f,
            "\n         ┌──┬──┬──┐\
             \n         │{}│{}│{}│\
             \n         ├──┼──┼──┤\
             \n         │{}│{}│{}│\
             \n         ├──┼──┼──┤\
             \n         │{}│{}│{}│\
             \n┌──┬──┬──┼──┼──┼──┼──┬──┬──┬──┬──┬──┐\
             \n│{}│{}│{}│{}│{}│{}│{}│{}│{}│{}│{}│{}│\
             \n├──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┤\
             \n│{}│{}│{}│{}│{}│{}│{}│{}│{}│{}│{}│{}│\
             \n├──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┤\
             \n│{}│{}│{}│{}│{}│{}│{}│{}│{}│{}│{}│{}│\
             \n└──┴──┴──┼──┼──┼──┼──┴──┴──┴──┴──┴──┘\
             \n         │{}│{}│{}│\
             \n         ├──┼──┼──┤\
             \n         │{}│{}│{}│\
             \n         ├──┼──┼──┤\
             \n         │{}│{}│{}│\
             \n         └──┴──┴──┘",
            c(1), e(1), c(0),
            e(2), w, e(0),
            c(2), e(3), c(3),

            c(17), e(17), c(16),
            c(5), e(5), c(4),
            c(9), e(9), c(8),
            c(13), e(13), c(12),

            e(18), o, e(16),
            e(6), g, e(4),
            e(10), r, e(8),
            e(14), b, e(12),

            c(18), e(19), c(19),
            c(6), e(7), c(7),
            c(10), e(11), c(11),
            c(14), e(15), c(15),

            c(21), e(21), c(20),
            e(22), y, e(20),
            c(22), e(23), c(23),
        )
    }
}

impl functional::BinaryOperation<FaceletCube> for FaceletCube {
    fn apply(a: FaceletCube, b: FaceletCube) -> FaceletCube {
        FaceletCube {
            edges: permute_arr(&a.edges, &b.edges),
            corners: permute_arr(&a.corners, &b.corners),
        }
    }
}

impl functional::AssociativeOperation<FaceletCube> for FaceletCube { }

impl functional::Monoid<FaceletCube> for FaceletCube {
    fn one() -> FaceletCube {
        FaceletCube {
            edges: arr_identity(),
            corners: arr_identity(),
        }
    }
}

impl Invertable for FaceletCube {
    fn invert(&self) -> FaceletCube {
        FaceletCube {
            edges: arr_inv(&self.edges),
            corners: arr_inv(&self.corners),
        }
    }
}

impl PermutationGroup for FaceletCube {}

const U: FaceletCube = FaceletCube {
    corners: [1, 2, 3, 0, 8, 9, 6, 7, 12, 13, 10, 11, 16, 17, 14, 15, 4, 5, 18, 19, 20, 21, 22, 23],
    edges: [1, 2, 3, 0, 4, 9, 6, 7, 8, 13, 10, 11, 12, 17, 14, 15, 16, 5, 18, 19, 20, 21, 22, 23]
};

const U_PRIME: FaceletCube = FaceletCube {
    corners: [3, 0, 1, 2, 16, 17, 6, 7, 4, 5, 10, 11, 8, 9, 14, 15, 12, 13, 18, 19, 20, 21, 22, 23],
    edges: [3, 0, 1, 2, 4, 17, 6, 7, 8, 5, 10, 11, 12, 9, 14, 15, 16, 13, 18, 19, 20, 21, 22, 23]
};

const F: FaceletCube = FaceletCube {
    corners: [0, 1, 19, 16, 5, 6, 7, 4, 8, 2, 3, 11, 12, 13, 14, 15, 21, 17, 18, 20, 9, 10, 22, 23],
    edges: [0, 1, 2, 16, 5, 6, 7, 4, 8, 9, 3, 11, 12, 13, 14, 15, 21, 17, 18, 19, 20, 10, 22, 23]
};

const F_PRIME: FaceletCube = FaceletCube {
    corners: [0, 1, 9, 10, 7, 4, 5, 6, 8, 20, 21, 11, 12, 13, 14, 15, 3, 17, 18, 2, 19, 16, 22, 23],
    edges: [0, 1, 2, 10, 7, 4, 5, 6, 8, 9, 21, 11, 12, 13, 14, 15, 3, 17, 18, 19, 20, 16, 22, 23]
};

const R: FaceletCube = FaceletCube {
    corners: [4, 1, 2, 7, 20, 5, 6, 23, 9, 10, 11, 8, 12, 3, 0, 15, 16, 17, 18, 19, 14, 21, 22, 13],
    edges: [4, 1, 2, 3, 20, 5, 6, 7, 9, 10, 11, 8, 12, 13, 0, 15, 16, 17, 18, 19, 14, 21, 22, 23]
};

const R_PRIME: FaceletCube = FaceletCube {
    corners: [14, 1, 2, 13, 0, 5, 6, 3, 11, 8, 9, 10, 12, 23, 20, 15, 16, 17, 18, 19, 4, 21, 22, 7],
    edges: [14, 1, 2, 3, 0, 5, 6, 7, 11, 8, 9, 10, 12, 13, 20, 15, 16, 17, 18, 19, 4, 21, 22, 23]
};

const B: FaceletCube = FaceletCube {
    corners: [11, 8, 2, 3, 4, 5, 6, 7, 23, 9, 10, 22, 13, 14, 15, 12, 16, 0, 1, 19, 20, 21, 17, 18],
    edges: [0, 8, 2, 3, 4, 5, 6, 7, 23, 9, 10, 11, 13, 14, 15, 12, 16, 17, 1, 19, 20, 21, 22, 18]
};

const B_PRIME: FaceletCube = FaceletCube {
    corners: [17, 18, 2, 3, 4, 5, 6, 7, 1, 9, 10, 0, 15, 12, 13, 14, 16, 22, 23, 19, 20, 21, 11, 8],
    edges: [0, 18, 2, 3, 4, 5, 6, 7, 1, 9, 10, 11, 15, 12, 13, 14, 16, 17, 23, 19, 20, 21, 22, 8]
};

const L: FaceletCube = FaceletCube {
    corners: [0, 15, 12, 3, 4, 1, 2, 7, 8, 9, 10, 11, 22, 13, 14, 21, 17, 18, 19, 16, 20, 5, 6, 23],
    edges: [0, 1, 12, 3, 4, 5, 2, 7, 8, 9, 10, 11, 22, 13, 14, 15, 17, 18, 19, 16, 20, 21, 6, 23]
};

const L_PRIME: FaceletCube = FaceletCube {
    corners: [0, 5, 6, 3, 4, 21, 22, 7, 8, 9, 10, 11, 2, 13, 14, 1, 19, 16, 17, 18, 20, 15, 12, 23],
    edges: [0, 1, 6, 3, 4, 5, 22, 7, 8, 9, 10, 11, 2, 13, 14, 15, 19, 16, 17, 18, 20, 21, 12, 23]
};

const D: FaceletCube = FaceletCube {
    corners: [0, 1, 2, 3, 4, 5, 18, 19, 8, 9, 6, 7, 12, 13, 10, 11, 16, 17, 14, 15, 21, 22, 23, 20],
    edges: [0, 1, 2, 3, 4, 5, 6, 19, 8, 9, 10, 7, 12, 13, 14, 11, 16, 17, 18, 15, 21, 22, 23, 20]
};

const D_PRIME: FaceletCube = FaceletCube {
    corners: [0, 1, 2, 3, 4, 5, 10, 11, 8, 9, 14, 15, 12, 13, 18, 19, 16, 17, 6, 7, 23, 20, 21, 22],
    edges: [0, 1, 2, 3, 4, 5, 6, 11, 8, 9, 10, 15, 12, 13, 14, 19, 16, 17, 18, 7, 23, 20, 21, 22]
};

use super::move_sets::quarter_turns::QuarterTurn;
impl From<QuarterTurn> for FaceletCube {
    fn from(qt: QuarterTurn) -> FaceletCube {
        match qt {
            QuarterTurn::U => U,
            QuarterTurn::UPrime => U_PRIME,
            QuarterTurn::F => F,
            QuarterTurn::FPrime => F_PRIME,
            QuarterTurn::R => R,
            QuarterTurn::RPrime => R_PRIME,
            QuarterTurn::B => B,
            QuarterTurn::BPrime => B_PRIME,
            QuarterTurn::L => L,
            QuarterTurn::LPrime => L_PRIME,
            QuarterTurn::D => D,
            QuarterTurn::DPrime => D_PRIME,
        }
    }
}

const S_URF: FaceletCube = FaceletCube {
  corners: [5, 6, 7, 4, 9, 10, 11, 8, 2, 3, 0, 1, 19, 16, 17, 18, 20, 21, 22, 23, 13, 14, 15, 12],
  edges: [5, 6, 7, 4, 9, 10, 11, 8, 2, 3, 0, 1, 19, 16, 17, 18, 20, 21, 22, 23, 13, 14, 15, 12]
};

const S_F: FaceletCube = FaceletCube {
    corners: [22, 23, 20, 21, 6, 7, 4, 5, 18, 19, 16, 17, 14, 15, 12, 13, 10, 11, 8, 9, 2, 3, 0, 1],
    edges: [22, 23, 20, 21, 6, 7, 4, 5, 18, 19, 16, 17, 14, 15, 12, 13, 10, 11, 8, 9, 2, 3, 0, 1]
};

const S_U: FaceletCube = FaceletCube {
    corners: [1, 2, 3, 0, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 4, 5, 6, 7, 23, 20, 21, 22],
    edges: [1, 2, 3, 0, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 4, 5, 6, 7, 23, 20, 21, 22]
};

const S_MRL: FaceletCube = FaceletCube {
    corners: [1, 0, 3, 2, 5, 4, 7, 6, 17, 16, 19, 18, 13, 12, 15, 14, 9, 8, 11, 10, 21, 20, 23, 22],
    edges: [2, 1, 0, 3, 6, 5, 4, 7, 18, 17, 16, 19, 14, 13, 12, 15, 10, 9, 8, 11, 22, 21, 20, 23]
};

use super::move_sets::symmetry_generators::SymmetryGenerator;
impl From<SymmetryGenerator> for FaceletCube {
    fn from(sg: SymmetryGenerator) -> FaceletCube {
        match sg {
            SymmetryGenerator::SUrf => S_URF,
            SymmetryGenerator::SF => S_F,
            SymmetryGenerator::SU => S_U,
            SymmetryGenerator::SMrl => S_MRL,
        }
    }
}

fn arr_identity() -> [u8; 24] {
    let mut r = [0; 24];
    //silly looking, but twice as fast ;)
    r[0] = 0;
    r[1] = 1;
    r[2] = 2;
    r[3] = 3;
    r[4] = 4;
    r[5] = 5;
    r[6] = 6;
    r[7] = 7;
    r[8] = 8;
    r[9] = 9;
    r[10] = 10;
    r[11] = 11;
    r[12] = 12;
    r[13] = 13;
    r[14] = 14;
    r[15] = 15;
    r[16] = 16;
    r[17] = 17;
    r[18] = 18;
    r[19] = 19;
    r[20] = 20;
    r[21] = 21;
    r[22] = 22;
    r[23] = 23;
    r
}

fn arr_inv(a: &[u8; 24]) -> [u8; 24] {
    let mut r = [0; 24];
    //silly looking, but twice as fast ;)
    r[a[0] as usize] = 0;
    r[a[1] as usize] = 1;
    r[a[2] as usize] = 2;
    r[a[3] as usize] = 3;
    r[a[4] as usize] = 4;
    r[a[5] as usize] = 5;
    r[a[6] as usize] = 6;
    r[a[7] as usize] = 7;
    r[a[8] as usize] = 8;
    r[a[9] as usize] = 9;
    r[a[10] as usize] = 10;
    r[a[11] as usize] = 11;
    r[a[12] as usize] = 12;
    r[a[13] as usize] = 13;
    r[a[14] as usize] = 14;
    r[a[15] as usize] = 15;
    r[a[16] as usize] = 16;
    r[a[17] as usize] = 17;
    r[a[18] as usize] = 18;
    r[a[19] as usize] = 19;
    r[a[20] as usize] = 20;
    r[a[21] as usize] = 21;
    r[a[22] as usize] = 22;
    r[a[23] as usize] = 23;
    r
}

fn permute_arr(a: &[u8; 24], b: &[u8; 24]) -> [u8; 24] {
    [
        a[b[0] as usize],
        a[b[1] as usize],
        a[b[2] as usize],
        a[b[3] as usize],
        a[b[4] as usize],
        a[b[5] as usize],
        a[b[6] as usize],
        a[b[7] as usize],
        a[b[8] as usize],
        a[b[9] as usize],
        a[b[10] as usize],
        a[b[11] as usize],
        a[b[12] as usize],
        a[b[13] as usize],
        a[b[14] as usize],
        a[b[15] as usize],
        a[b[16] as usize],
        a[b[17] as usize],
        a[b[18] as usize],
        a[b[19] as usize],
        a[b[20] as usize],
        a[b[21] as usize],
        a[b[22] as usize],
        a[b[23] as usize],
    ]
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;

    const CLEAN_ARR: [u8; 24] = [
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23
    ];
    const ONE_ZERO_ARR: [u8; 24] = [
        1, 0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23
    ];
    const LOTS_ARR: [u8; 24] = [
        22, 4, 7, 3, 18, 10, 2, 1, 9, 0, 14, 11, 8, 13, 15, 5, 6, 19, 17, 12, 16, 23, 21, 20
    ];

    #[test]
    fn permuting_identity_with_identity_is_identity() {
        assert_eq!(CLEAN_ARR, permute_arr(&CLEAN_ARR, &CLEAN_ARR));
    }

    #[test]
    fn permuting_identity_with_a_perm_is_that_same_perm() {
        assert_eq!(LOTS_ARR, permute_arr(&CLEAN_ARR, &LOTS_ARR));
    }

    #[test]
    fn applying_a_perm_and_then_its_inverse_or_vice_versa_is_the_original_perm() {
        /*
         * TODO:
        assert_eq!(LOTS, permute_inv(permute(LOTS, ONE_ZERO), ONE_ZERO));
        assert_eq!(LOTS, permute(permute_inv(LOTS, ONE_ZERO), ONE_ZERO));
        assert_eq!(CLEAN, permute_inv(permute(CLEAN, LOTS), LOTS));
        assert_eq!(CLEAN, permute(permute_inv(CLEAN, LOTS), LOTS));
        */
        assert_eq!(
            LOTS_ARR,
            permute_arr(&permute_arr(&LOTS_ARR, &ONE_ZERO_ARR), &arr_inv(&ONE_ZERO_ARR))
        );
        assert_eq!(
            LOTS_ARR,
            permute_arr(&permute_arr(&LOTS_ARR, &arr_inv(&ONE_ZERO_ARR)), &ONE_ZERO_ARR)
        );
        assert_eq!(
            CLEAN_ARR,
            permute_arr(&permute_arr(&CLEAN_ARR, &LOTS_ARR), &arr_inv(&LOTS_ARR))
        );
        assert_eq!(
            CLEAN_ARR,
            permute_arr(&permute_arr(&CLEAN_ARR, &arr_inv(&LOTS_ARR)), &LOTS_ARR)
        );
    }

    #[test]
    fn u_is_not_identity() {
        assert_ne!(FaceletCube::from(QuarterTurn::U), FaceletCube::identity());
    }

    #[test]
    fn u_invert_is_its_prime() {
        assert_eq!(FaceletCube::from(QuarterTurn::U).invert(), FaceletCube::from(QuarterTurn::UPrime));
    }

    #[test]
    fn f_invert_is_its_prime() {
        assert_eq!(FaceletCube::from(QuarterTurn::F).invert(), FaceletCube::from(QuarterTurn::FPrime));
    }

    #[test]
    fn r_invert_is_its_prime() {
        assert_eq!(FaceletCube::from(QuarterTurn::R).invert(), FaceletCube::from(QuarterTurn::RPrime));
    }

    #[test]
    fn b_invert_is_its_prime() {
        assert_eq!(FaceletCube::from(QuarterTurn::B).invert(), FaceletCube::from(QuarterTurn::BPrime));
    }

    #[test]
    fn l_invert_is_its_prime() {
        assert_eq!(FaceletCube::from(QuarterTurn::L).invert(), FaceletCube::from(QuarterTurn::LPrime));
    }

    #[test]
    fn d_invert_is_its_prime() {
        assert_eq!(FaceletCube::from(QuarterTurn::D).invert(), FaceletCube::from(QuarterTurn::DPrime));
    }

    #[test]
    fn u_3_times_is_its_prime() {
        let u_prime = FaceletCube::from(QuarterTurn::U)
            .permute(QuarterTurn::U.into())
            .permute(QuarterTurn::U.into());
        assert_eq!(u_prime, FaceletCube::from(QuarterTurn::UPrime));
    }

    #[test]
    fn f_3_times_is_its_prime() {
        let f_prime = FaceletCube::from(QuarterTurn::F)
            .permute(QuarterTurn::F.into())
            .permute(QuarterTurn::F.into());
        assert_eq!(f_prime, FaceletCube::from(QuarterTurn::FPrime));
    }

    #[test]
    fn r_3_times_is_its_prime() {
        let r_prime = FaceletCube::from(QuarterTurn::R)
            .permute(QuarterTurn::R.into())
            .permute(QuarterTurn::R.into());
        assert_eq!(r_prime, FaceletCube::from(QuarterTurn::RPrime));
    }

    #[test]
    fn b_3_times_is_its_prime() {
        let b_prime = FaceletCube::from(QuarterTurn::B)
            .permute(QuarterTurn::B.into())
            .permute(QuarterTurn::B.into());
        assert_eq!(b_prime, FaceletCube::from(QuarterTurn::BPrime));
    }

    #[test]
    fn l_3_times_is_its_prime() {
        let l_prime = FaceletCube::from(QuarterTurn::L)
            .permute(QuarterTurn::L.into())
            .permute(QuarterTurn::L.into());
        assert_eq!(l_prime, FaceletCube::from(QuarterTurn::LPrime));
    }

    #[test]
    fn d_3_times_is_its_prime() {
        let d_prime = FaceletCube::from(QuarterTurn::D)
            .permute(QuarterTurn::D.into())
            .permute(QuarterTurn::D.into());
        assert_eq!(d_prime, FaceletCube::from(QuarterTurn::DPrime));
    }

    #[test]
    fn should_be_able_to_generate_one_turn_with_a_combination_of_the_other_five() {
        let d = FaceletCube::from(QuarterTurn::R)
            .permute(QuarterTurn::L.into())
            .permute(QuarterTurn::F.into())
            .permute(QuarterTurn::F.into())
            .permute(QuarterTurn::B.into())
            .permute(QuarterTurn::B.into())
            .permute(QuarterTurn::RPrime.into())
            .permute(QuarterTurn::LPrime.into())
            .permute(QuarterTurn::U.into())
            .permute(QuarterTurn::R.into())
            .permute(QuarterTurn::L.into())
            .permute(QuarterTurn::FPrime.into())
            .permute(QuarterTurn::FPrime.into())
            .permute(QuarterTurn::BPrime.into())
            .permute(QuarterTurn::BPrime.into())
            .permute(QuarterTurn::RPrime.into())
            .permute(QuarterTurn::LPrime.into());
        assert_eq!(d, QuarterTurn::D.into());
    }

    #[test]
    fn d_and_d_prime_should_be_the_identity() {
        let d = FaceletCube::from(QuarterTurn::D)
            .permute(QuarterTurn::DPrime.into());
        assert_eq!(d, FaceletCube::identity());
    }

    #[test]
    fn u_and_u_prime_should_be_the_identity() {
        let u = FaceletCube::from(QuarterTurn::U)
            .permute(QuarterTurn::UPrime.into());
        assert_eq!(u, FaceletCube::identity());
    }

    #[test]
    fn f_and_f_prime_should_be_the_identity() {
        let f = FaceletCube::from(QuarterTurn::F)
            .permute(QuarterTurn::FPrime.into());
        assert_eq!(f, FaceletCube::identity());
    }

    #[test]
    fn r_and_r_prime_should_be_the_identity() {
        let r = FaceletCube::from(QuarterTurn::R)
            .permute(QuarterTurn::RPrime.into());
        assert_eq!(r, FaceletCube::identity());
    }

    #[test]
    fn b_and_b_prime_should_be_the_identity() {
        let b = FaceletCube::from(QuarterTurn::B)
            .permute(QuarterTurn::BPrime.into());
        assert_eq!(b, FaceletCube::identity());
    }

    #[test]
    fn l_and_l_prime_should_be_the_identity() {
        let l = FaceletCube::from(QuarterTurn::L)
            .permute(QuarterTurn::LPrime.into());
        assert_eq!(l, FaceletCube::identity());
    }

    #[test]
    fn s_urf_3_times_is_the_identity() {
        let id = FaceletCube::from(SymmetryGenerator::SUrf)
            .permute(SymmetryGenerator::SUrf.into())
            .permute(SymmetryGenerator::SUrf.into());
        assert_eq!(id, FaceletCube::identity());
    }

    #[test]
    fn s_f_2_times_is_the_identity() {
        let id = FaceletCube::from(SymmetryGenerator::SF)
            .permute(SymmetryGenerator::SF.into());
        assert_eq!(id, FaceletCube::identity());
    }

    #[test]
    fn s_u_4_times_is_the_identity() {
        let id = FaceletCube::from(SymmetryGenerator::SU)
            .permute(SymmetryGenerator::SU.into())
            .permute(SymmetryGenerator::SU.into())
            .permute(SymmetryGenerator::SU.into());
        assert_eq!(id, FaceletCube::identity());
    }

    #[test]
    fn s_mrl_2_times_is_the_identity() {
        let id = FaceletCube::from(SymmetryGenerator::SMrl)
            .permute(SymmetryGenerator::SMrl.into());
        assert_eq!(id, FaceletCube::identity());
    }

    #[test]
    fn u_is_symmetrical_to_f() {
        let f = FaceletCube::from(SymmetryGenerator::SUrf)
            .permute(QuarterTurn::U.into())
            .permute(SymmetryGenerator::SUrf.into())
            .permute(SymmetryGenerator::SUrf.into());
        assert_eq!(f, QuarterTurn::F.into());
    }

    #[test]
    fn u_is_symmetrical_to_d() {
        let d = FaceletCube::from(SymmetryGenerator::SF)
            .permute(QuarterTurn::U.into())
            .permute(SymmetryGenerator::SF.into());
        assert_eq!(d, QuarterTurn::D.into());
    }

    #[test]
    fn f_is_symmetrical_to_r() {
        let r = FaceletCube::from(SymmetryGenerator::SU)
            .permute(QuarterTurn::F.into())
            .permute(SymmetryGenerator::SU.into())
            .permute(SymmetryGenerator::SU.into())
            .permute(SymmetryGenerator::SU.into());
        assert_eq!(r, QuarterTurn::R.into());
    }

    #[test]
    fn r_is_symmetrical_to_b() {
        let r = FaceletCube::from(SymmetryGenerator::SU)
            .permute(SymmetryGenerator::SU.into())
            .permute(SymmetryGenerator::SU.into())
            .permute(QuarterTurn::B.into())
            .permute(SymmetryGenerator::SU.into());
        assert_eq!(r, QuarterTurn::R.into());
    }

    #[test]
    fn b_is_symmetrical_to_l() {
        let r = FaceletCube::from(SymmetryGenerator::SU)
            .permute(SymmetryGenerator::SU.into())
            .permute(SymmetryGenerator::SU.into())
            .permute(QuarterTurn::L.into())
            .permute(SymmetryGenerator::SU.into());
        assert_eq!(r, QuarterTurn::B.into());
    }

    #[test]
    fn u_is_symmetrical_to_u_prime() {
        let u_prime = FaceletCube::from(SymmetryGenerator::SMrl)
            .permute(QuarterTurn::U.into())
            .permute(SymmetryGenerator::SMrl.into());
        assert_eq!(u_prime, QuarterTurn::UPrime.into());
    }

    #[test]
    fn r_is_symmetrical_to_l_prime() {
        let l_prime = FaceletCube::from(SymmetryGenerator::SMrl)
            .permute(QuarterTurn::R.into())
            .permute(SymmetryGenerator::SMrl.into());
        assert_eq!(l_prime, QuarterTurn::LPrime.into());
    }

    #[bench]
    fn repeatedly_perform_1000_turns_via_complex_arr_permutation(b: &mut Bencher) {
        let mut cleanc = CLEAN_ARR;
        b.iter(|| {
            for _ in 0..2_000 {
                cleanc = permute_arr(&cleanc, &LOTS_ARR)
            }
            cleanc
        });
    }

    #[bench]
    fn repeatedly_perform_1000_inv_turns_via_complex_arr_permutation(b: &mut Bencher) {
        let mut cleanc = CLEAN_ARR;
        b.iter(|| {
            for _ in 0..2_000 {
                cleanc = permute_arr(&cleanc, &arr_inv(&LOTS_ARR))
            }
            cleanc
        });
    }

    #[bench]
    fn repeat_all_turn_identity_sequence(b: &mut Bencher) {
        b.iter(|| {
            FaceletCube::from(QuarterTurn::R)
                .permute(QuarterTurn::L.into())
                .permute(QuarterTurn::F.into())
                .permute(QuarterTurn::F.into())
                .permute(QuarterTurn::B.into())
                .permute(QuarterTurn::B.into())
                .permute(QuarterTurn::RPrime.into())
                .permute(QuarterTurn::LPrime.into())
                .permute(QuarterTurn::U.into())
                .permute(QuarterTurn::R.into())
                .permute(QuarterTurn::L.into())
                .permute(QuarterTurn::FPrime.into())
                .permute(QuarterTurn::FPrime.into())
                .permute(QuarterTurn::BPrime.into())
                .permute(QuarterTurn::BPrime.into())
                .permute(QuarterTurn::RPrime.into())
                .permute(QuarterTurn::LPrime.into())
                .permute(QuarterTurn::D.into());
        });
    }
}
