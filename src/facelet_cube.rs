extern crate functional;

use super::permutation_group::PermutationGroup;

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
    //TODO: These should not be public
    pub corners: [u8; 24],
    pub edges: [u8; 24],
}

impl fmt::Display for FaceletCube {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ansi_term::Colour::*;
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

impl PermutationGroup for FaceletCube {
    fn invert(&self) -> FaceletCube {
        FaceletCube {
            edges: arr_inv(&self.edges),
            corners: arr_inv(&self.corners),
        }
    }
}

// TODO: These should not be public
pub fn arr_identity() -> [u8; 24] {
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
}
