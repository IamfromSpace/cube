extern crate ansi_term;
extern crate functional;

use super::permutation_group::PermutationGroup;
use super::invertable::Invertable;

use std::fmt;

/* The wing edge representation of a cube is for a 4x4+, which defines a single
 * orbit of wing edges.  The 4x4 and 5x5 cube have one, a 6x6 and 7x7 have two,
 * and so on.  Wing edges don't truly have orientation just position, because
 * they cannot be in the correct position and flipped.  Each color pairing has
 * two wing edges, but they cannot be exchanged without flipping.
 *
 * If edges are truly solved in isolation, then we really only have 46
 * facelets, as we can just arbitrarily pick one to be correct, in the same way
 * we can just arbitrarily pick a correct corner on a 2x2 cube.  However, we
 * model all 48 facelets, because on odd cubes, wing edges must match centers,
 * and on even cubes, wing edges must match other facelets--it's possible there
 * are none that can be considered arbitrarily correct.
 *
 * Even though we can't reduce the state space by imagining one piece as
 * arbitrarily correct, we can still use our full 24 types of symmetry we get
 * on a 3x3.  If we consider one wing edge correct there is only one symmetric
 * position (mirrored through the fixed edge).  So we lose little by imagining
 * that centers are fixed.
 *
 * At the moment, the real intention of this module is to help generate truly
 * random scrambles for big cubes.  The idea is to generate a random position
 * for each orbit, solve each indepdently, and then concattenate each solve to
 * scramble all wing edges.  Even though each scramble affects the other, since
 * each is affected by a truly random permutation, we still get a truly random
 * scramble for each orbit.  This is akin to how OTP can mask a structured
 * message by XORing a random mask (I think).
 */

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)] //, Serialize, Deserialize)] 48 elements not derivable
pub struct FaceletWingEdges(pub [u8; 48]);

impl fmt::Display for FaceletWingEdges {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::ansi_term::Colour::*;
        let w = White.paint("██");
        let g = Green.paint("██");
        let r = Red.paint("██");
        let b = Blue.paint("██");
        let o = RGB(250, 48, 11).paint("██");
        let y = Yellow.paint("██");
        let c = |id: usize| match self.0[id] >> 3 {
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
            "\n               ┌──┬──┐\
             \n               │{}│{}│\
             \n            ┌──┼──┴──┼──┐\
             \n            │{}│     │{}│\
             \n            ├──┤     ├──┤\
             \n            │{}│     │{}│\
             \n            └──┼──┬──┼──┘\
             \n               │{}│{}│\
             \n   ┌──┬──┐     ├──┼──┤     ┌──┬──┐     ┌──┬──┐\
             \n   │{}│{}│     │{}│{}│     │{}│{}│     │{}│{}│\
             \n┌──┼──┴──┼──┬──┼──┴──┼──┬──┼──┴──┼──┬──┼──┴──┼──┐\
             \n│{}│     │{}│{}│     │{}│{}│     │{}│{}│     │{}│\
             \n├──┤     ├──┼──┤     ├──┼──┤     ├──┼──┤     ├──┤\
             \n│{}│     │{}│{}│     │{}│{}│     │{}│{}│     │{}│\
             \n└──┼──┬──┼──┴──┼──┬──┼──┴──┼──┬──┼──┴──┼──┬──┼──┘\
             \n   │{}│{}│     │{}│{}│     │{}│{}│     │{}│{}│\
             \n   └──┴──┘     ├──┼──┤     └──┴──┘     └──┴──┘\
             \n               │{}│{}│\
             \n            ┌──┼──┴──┼──┐\
             \n            │{}│     │{}│\
             \n            ├──┤     ├──┤\
             \n            │{}│     │{}│\
             \n            └──┼──┬──┼──┘\
             \n               │{}│{}│\
             \n               └──┴──┘",
                                         c(2), c(1),
                                     c(3),         c(0),
                                     c(4),         c(7),
                                         c(5), c(6),

                 c(34), c(33),           c(10), c(9),           c(18), c(17),           c(26), c(25),
            c(35),           c(32), c(11),          c(8),  c(19),           c(16), c(27),           c(24),
            c(36),           c(39), c(12),          c(15), c(20),           c(23), c(28),           c(31),
                 c(37), c(38),           c(13), c(14),          c(21), c(22),           c(29), c(30),

                                         c(42), c(41),
                                    c(43),           c(40),
                                    c(44),           c(47),
                                         c(45), c(46),
        )
    }
}

impl functional::BinaryOperation<FaceletWingEdges> for FaceletWingEdges {
    fn apply(a: FaceletWingEdges, b: FaceletWingEdges) -> FaceletWingEdges {
        FaceletWingEdges(permute_arr(&a.0, &b.0))
    }
}

impl functional::AssociativeOperation<FaceletWingEdges> for FaceletWingEdges { }

impl functional::Monoid<FaceletWingEdges> for FaceletWingEdges {
    fn one() -> FaceletWingEdges {
        FaceletWingEdges(arr_identity())
    }
}

impl Invertable for FaceletWingEdges {
    fn invert(&self) -> FaceletWingEdges {
        FaceletWingEdges(arr_inv(&self.0))
    }
}

impl PermutationGroup for FaceletWingEdges {}

const U: FaceletWingEdges = FaceletWingEdges([2, 3, 4, 5, 6, 7, 0, 1, 8, 17, 18, 11, 12, 13, 14, 15, 16, 25, 26, 19, 20, 21, 22, 23, 24, 33, 34, 27, 28, 29, 30, 31, 32, 9, 10, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47]);

const U2: FaceletWingEdges = FaceletWingEdges(permute_arr(&U.0, &U.0));

const U_PRIME: FaceletWingEdges = FaceletWingEdges(arr_inv(&U.0));

#[allow(non_upper_case_globals)]
const u: FaceletWingEdges = FaceletWingEdges([0, 1, 2, 3, 4, 5, 6, 7, 16, 9, 10, 19, 12, 13, 14, 15, 24, 17, 18, 27, 20, 21, 22, 23, 32, 25, 26, 35, 28, 29, 30, 31, 8, 33, 34, 11, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47]);

#[allow(non_upper_case_globals)]
const u_PRIME: FaceletWingEdges = FaceletWingEdges(arr_inv(&u.0));

#[allow(non_upper_case_globals)]
const Uw: FaceletWingEdges = FaceletWingEdges(permute_arr(&U.0, &u.0));

#[allow(non_upper_case_globals)]
const Uw2: FaceletWingEdges = FaceletWingEdges(permute_arr(&Uw.0, &Uw.0));

#[allow(non_upper_case_globals)]
const Uw_PRIME: FaceletWingEdges = FaceletWingEdges(arr_inv(&Uw.0));

const F: FaceletWingEdges = FaceletWingEdges(arr_inv(&F_PRIME.0));

const F2: FaceletWingEdges = FaceletWingEdges(permute_arr(&F.0, &F.0));

const F_PRIME: FaceletWingEdges = FaceletWingEdges([0, 1, 2, 3, 4, 19, 20, 7, 10, 11, 12, 13, 14, 15, 8, 9, 16, 17, 18, 41, 42, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 6, 33, 34, 35, 36, 37, 38, 5, 40, 39, 32, 43, 44, 45, 46, 47]);

#[allow(non_upper_case_globals)]
const f_SLICE: FaceletWingEdges = FaceletWingEdges([0, 1, 2, 3, 38, 5, 6, 33, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 4, 19, 20, 7, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 43, 34, 35, 36, 37, 40, 39, 18, 41, 42, 21, 44, 45, 46, 47]);

#[allow(non_upper_case_globals)]
const Fw: FaceletWingEdges = FaceletWingEdges(permute_arr(&F.0, &f_SLICE.0));

#[allow(non_upper_case_globals)]
const Fw2: FaceletWingEdges = FaceletWingEdges(permute_arr(&Fw.0, &Fw.0));

#[allow(non_upper_case_globals)]
const Fw_PRIME: FaceletWingEdges = FaceletWingEdges(arr_inv(&Fw.0));

const R: FaceletWingEdges = FaceletWingEdges([8, 1, 2, 3, 4, 5, 6, 15, 40, 9, 10, 11, 12, 13, 14, 47, 18, 19, 20, 21, 22, 23, 16, 17, 24, 25, 26, 7, 0, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 28, 41, 42, 43, 44, 45, 46, 27]);

const R2: FaceletWingEdges = FaceletWingEdges(permute_arr(&R.0, &R.0));

const R_PRIME: FaceletWingEdges = FaceletWingEdges(arr_inv(&R.0));

#[allow(non_upper_case_globals)]
const r_SLICE: FaceletWingEdges = FaceletWingEdges([0, 9, 2, 3, 4, 5, 14, 7, 8, 41, 10, 11, 12, 13, 46, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 6, 27, 28, 1, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 29, 42, 43, 44, 45, 26, 47]);

#[allow(non_upper_case_globals)]
const Rw: FaceletWingEdges = FaceletWingEdges(permute_arr(&R.0, &r_SLICE.0));

#[allow(non_upper_case_globals)]
const Rw2: FaceletWingEdges = FaceletWingEdges(permute_arr(&Rw.0, &Rw.0));

#[allow(non_upper_case_globals)]
const Rw_PRIME: FaceletWingEdges = FaceletWingEdges(arr_inv(&Rw.0));

const B: FaceletWingEdges = FaceletWingEdges([0, 23, 16, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 46, 17, 18, 19, 20, 21, 22, 45, 26, 27, 28, 29, 30, 31, 24, 25, 32, 33, 34, 1, 2, 37, 38, 39, 40, 41, 42, 43, 44, 35, 36, 47]);

const B2: FaceletWingEdges = FaceletWingEdges(permute_arr(&B.0, &B.0));

const B_PRIME: FaceletWingEdges = FaceletWingEdges(arr_inv(&B.0));

#[allow(non_upper_case_globals)]
const b_SLICE: FaceletWingEdges = FaceletWingEdges([22, 1, 2, 17, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 47, 18, 19, 20, 21, 44, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 0, 35, 36, 3, 38, 39, 40, 41, 42, 43, 34, 45, 46, 37]);

#[allow(non_upper_case_globals)]
const Bw: FaceletWingEdges = FaceletWingEdges(permute_arr(&B.0, &b_SLICE.0));

#[allow(non_upper_case_globals)]
const Bw2: FaceletWingEdges = FaceletWingEdges(permute_arr(&Bw.0, &Bw.0));

#[allow(non_upper_case_globals)]
const Bw_PRIME: FaceletWingEdges = FaceletWingEdges(arr_inv(&Bw.0));

const L: FaceletWingEdges = FaceletWingEdges([0, 1, 2, 31, 24, 5, 6, 7, 8, 9, 10, 3, 4, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 44, 25, 26, 27, 28, 29, 30, 43, 34, 35, 36, 37, 38, 39, 32, 33, 40, 41, 42, 11, 12, 45, 46, 47]);

const L2: FaceletWingEdges = FaceletWingEdges(permute_arr(&L.0, &L.0));

const L_PRIME: FaceletWingEdges = FaceletWingEdges(arr_inv(&L.0));

#[allow(non_upper_case_globals)]
const l_SLICE: FaceletWingEdges = FaceletWingEdges([0, 1, 30, 3, 4, 25, 6, 7, 8, 9, 2, 11, 12, 5, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 45, 26, 27, 28, 29, 42, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 10, 43, 44, 13, 46, 47]);

#[allow(non_upper_case_globals)]
const Lw: FaceletWingEdges = FaceletWingEdges(permute_arr(&L.0, &l_SLICE.0));

#[allow(non_upper_case_globals)]
const Lw2: FaceletWingEdges = FaceletWingEdges(permute_arr(&Lw.0, &Lw.0));

#[allow(non_upper_case_globals)]
const Lw_PRIME: FaceletWingEdges = FaceletWingEdges(arr_inv(&Lw.0));

const D: FaceletWingEdges = FaceletWingEdges([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 37, 38, 15, 16, 17, 18, 19, 20, 13, 14, 23, 24, 25, 26, 27, 28, 21, 22, 31, 32, 33, 34, 35, 36, 29, 30, 39, 42, 43, 44, 45, 46, 47, 40, 41]);
// 40 -> 42 -> 44 -> 46
// 41 -> 43 -> 45 -> 47
// 29 -> 21 -> 13 -> 37
// 30 -> 22 -> 14 -> 38

const D2: FaceletWingEdges = FaceletWingEdges(permute_arr(&D.0, &D.0));

const D_PRIME: FaceletWingEdges = FaceletWingEdges(arr_inv(&D.0));

#[allow(non_upper_case_globals)]
const d_SLICE: FaceletWingEdges = FaceletWingEdges([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 36, 13, 14, 39, 16, 17, 18, 19, 12, 21, 22, 15, 24, 25, 26, 27, 20, 29, 30, 23, 32, 33, 34, 35, 28, 37, 38, 31, 40, 41, 42, 43, 44, 45, 46, 47]);
// 28 -> 20 -> 12 -> 36
// 31 -> 23 -> 15 -> 39

#[allow(non_upper_case_globals)]
const Dw: FaceletWingEdges = FaceletWingEdges(permute_arr(&D.0, &d_SLICE.0));

#[allow(non_upper_case_globals)]
const Dw2: FaceletWingEdges = FaceletWingEdges(permute_arr(&Dw.0, &Dw.0));

#[allow(non_upper_case_globals)]
const Dw_PRIME: FaceletWingEdges = FaceletWingEdges(arr_inv(&Dw.0));

use super::move_sets::wide_turns::WideTurn;
impl From<WideTurn> for FaceletWingEdges {
    fn from(wt: WideTurn) -> FaceletWingEdges {
        match wt {
            WideTurn::U => U,
            WideTurn::U2 => U2,
            WideTurn::UPrime => U_PRIME,
            WideTurn::Uw => Uw,
            WideTurn::Uw2 => Uw2,
            WideTurn::UwPrime => Uw_PRIME,
            WideTurn::F => F,
            WideTurn::F2 => F2,
            WideTurn::FPrime => F_PRIME,
            WideTurn::Fw => Fw,
            WideTurn::Fw2 => Fw2,
            WideTurn::FwPrime => Fw_PRIME,
            WideTurn::R => R,
            WideTurn::R2 => R2,
            WideTurn::RPrime => R_PRIME,
            WideTurn::Rw => Rw,
            WideTurn::Rw2 => Rw2,
            WideTurn::RwPrime => Rw_PRIME,
            WideTurn::B => B,
            WideTurn::B2 => B2,
            WideTurn::BPrime => B_PRIME,
            WideTurn::Bw => Bw,
            WideTurn::Bw2 => Bw2,
            WideTurn::BwPrime => Bw_PRIME,
            WideTurn::L => L,
            WideTurn::L2 => L2,
            WideTurn::LPrime => L_PRIME,
            WideTurn::Lw => Lw,
            WideTurn::Lw2 => Lw2,
            WideTurn::LwPrime => Lw_PRIME,
            WideTurn::D => D,
            WideTurn::D2 => D2,
            WideTurn::DPrime => D_PRIME,
            WideTurn::Dw => Dw,
            WideTurn::Dw2 => Dw2,
            WideTurn::DwPrime => Dw_PRIME,
        }
    }
}

fn arr_identity() -> [u8; 48] {
    let mut r = [0; 48];
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
    r[24] = 24;
    r[25] = 25;
    r[26] = 26;
    r[27] = 27;
    r[28] = 28;
    r[29] = 29;
    r[30] = 30;
    r[31] = 31;
    r[32] = 32;
    r[33] = 33;
    r[34] = 34;
    r[35] = 35;
    r[36] = 36;
    r[37] = 37;
    r[38] = 38;
    r[39] = 39;
    r[40] = 40;
    r[41] = 41;
    r[42] = 42;
    r[43] = 43;
    r[44] = 44;
    r[45] = 45;
    r[46] = 46;
    r[47] = 47;
    r
}

const fn arr_inv(a: &[u8; 48]) -> [u8; 48] {
    let mut r = [0; 48];
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
    r[a[24] as usize] = 24;
    r[a[25] as usize] = 25;
    r[a[26] as usize] = 26;
    r[a[27] as usize] = 27;
    r[a[28] as usize] = 28;
    r[a[29] as usize] = 29;
    r[a[30] as usize] = 30;
    r[a[31] as usize] = 31;
    r[a[32] as usize] = 32;
    r[a[33] as usize] = 33;
    r[a[34] as usize] = 34;
    r[a[35] as usize] = 35;
    r[a[36] as usize] = 36;
    r[a[37] as usize] = 37;
    r[a[38] as usize] = 38;
    r[a[39] as usize] = 39;
    r[a[40] as usize] = 40;
    r[a[41] as usize] = 41;
    r[a[42] as usize] = 42;
    r[a[43] as usize] = 43;
    r[a[44] as usize] = 44;
    r[a[45] as usize] = 45;
    r[a[46] as usize] = 46;
    r[a[47] as usize] = 47;
    r
}

const fn permute_arr(a: &[u8; 48], b: &[u8; 48]) -> [u8; 48] {
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
        a[b[24] as usize],
        a[b[25] as usize],
        a[b[26] as usize],
        a[b[27] as usize],
        a[b[28] as usize],
        a[b[29] as usize],
        a[b[30] as usize],
        a[b[31] as usize],
        a[b[32] as usize],
        a[b[33] as usize],
        a[b[34] as usize],
        a[b[35] as usize],
        a[b[36] as usize],
        a[b[37] as usize],
        a[b[38] as usize],
        a[b[39] as usize],
        a[b[40] as usize],
        a[b[41] as usize],
        a[b[42] as usize],
        a[b[43] as usize],
        a[b[44] as usize],
        a[b[45] as usize],
        a[b[46] as usize],
        a[b[47] as usize],
    ]
}
