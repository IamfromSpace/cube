extern crate functional;

use std::fmt;
use super::permutation_group::PermutationGroup;
use super::invertable::Invertable;

/*
 *  Looking at the top:
 *          B
 *      9         8
 *         2  1
 *       3      0
 *       4      7
 *         5  6
 *     10        11
 *          F
 *
 *  Looking at the bottom:
 *          F
 *     13        12
 *        18 17
 *      19     16
 *      20     23
 *        21 22
 *     14        15
 *          B
 */

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash, Serialize, Deserialize)]
pub struct CoordWingEdges([u8; 24]);

impl functional::BinaryOperation<CoordWingEdges> for CoordWingEdges {
    fn apply(a: CoordWingEdges, b: CoordWingEdges) -> CoordWingEdges {
        CoordWingEdges(permute_arr(&a.0, &b.0))
    }
}

impl functional::AssociativeOperation<CoordWingEdges> for CoordWingEdges { }

impl functional::Monoid<CoordWingEdges> for CoordWingEdges {
    fn one() -> CoordWingEdges {
        CoordWingEdges(arr_identity())
    }
}

impl Invertable for CoordWingEdges {
    fn invert(&self) -> CoordWingEdges {
        CoordWingEdges(arr_inv(&self.0))
    }
}

impl PermutationGroup for CoordWingEdges {}

// 1 -> 3 -> 5 -> 7
// 2 -> 4 -> 6 -> 0
const U: CoordWingEdges = CoordWingEdges([2, 3, 4, 5, 6, 7, 0, 1, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23]);

const U2: CoordWingEdges = CoordWingEdges(permute_arr(&U.0, &U.0));

const U_PRIME: CoordWingEdges = CoordWingEdges(arr_inv(&U.0));

// 8 -> 9 -> 10 -> 11
#[allow(non_upper_case_globals)]
const u_SLICE: CoordWingEdges = CoordWingEdges([0, 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 8, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23]);

#[allow(non_upper_case_globals)]
const Uw: CoordWingEdges = CoordWingEdges(permute_arr(&U.0, &u_SLICE.0));

#[allow(non_upper_case_globals)]
const Uw2: CoordWingEdges = CoordWingEdges(permute_arr(&Uw.0, &Uw.0));

#[allow(non_upper_case_globals)]
const Uw_PRIME: CoordWingEdges = CoordWingEdges(arr_inv(&Uw.0));

// 11 -> 5 -> 13 -> 17
// 12 -> 6 -> 10 -> 18
const F: CoordWingEdges = CoordWingEdges([0, 1, 2, 3, 4, 13, 10, 7, 8, 9, 18, 5, 6, 17, 14, 15, 16, 11, 12, 19, 20, 21, 22, 23]);

const F2: CoordWingEdges = CoordWingEdges(permute_arr(&F.0, &F.0));

const F_PRIME: CoordWingEdges = CoordWingEdges(arr_inv(&F.0));

// 7 -> 4 -> 19 -> 16
#[allow(non_upper_case_globals)]
const f_SLICE: CoordWingEdges = CoordWingEdges([0, 1, 2, 3, 19, 5, 6, 4, 8, 9, 10, 11, 12, 13, 14, 15, 7, 17, 18, 16, 20, 21, 22, 23]);

#[allow(non_upper_case_globals)]
const Fw: CoordWingEdges = CoordWingEdges(permute_arr(&F.0, &f_SLICE.0));

#[allow(non_upper_case_globals)]
const Fw2: CoordWingEdges = CoordWingEdges(permute_arr(&Fw.0, &Fw.0));

#[allow(non_upper_case_globals)]
const Fw_PRIME: CoordWingEdges = CoordWingEdges(arr_inv(&Fw.0));

// 8 -> 7 -> 12 -> 23
// 15 -> 0 -> 11 -> 16
const R: CoordWingEdges = CoordWingEdges([11, 1, 2, 3, 4, 5, 6, 12, 7, 9, 10, 16, 23, 13, 14, 0, 15, 17, 18, 19, 20, 21, 22, 8]);

const R2: CoordWingEdges = CoordWingEdges(permute_arr(&R.0, &R.0));

const R_PRIME: CoordWingEdges = CoordWingEdges(arr_inv(&R.0));

 // 1 -> 6 -> 17 -> 22
#[allow(non_upper_case_globals)]
const r_SLICE: CoordWingEdges = CoordWingEdges([0, 6, 2, 3, 4, 5, 17, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 22, 18, 19, 20, 21, 1, 23]);

#[allow(non_upper_case_globals)]
const Rw: CoordWingEdges = CoordWingEdges(permute_arr(&R.0, &r_SLICE.0));

#[allow(non_upper_case_globals)]
const Rw2: CoordWingEdges = CoordWingEdges(permute_arr(&Rw.0, &Rw.0));

#[allow(non_upper_case_globals)]
const Rw_PRIME: CoordWingEdges = CoordWingEdges(arr_inv(&Rw.0));

// 1 -> 15 -> 21 -> 9
// 2 -> 8 -> 22 -> 14
const B: CoordWingEdges = CoordWingEdges([0, 15, 8, 3, 4, 5, 6, 7, 22, 1, 10, 11, 12, 13, 2, 21, 16, 17, 18, 19, 20, 9, 14, 23]);

const B2: CoordWingEdges = CoordWingEdges(permute_arr(&B.0, &B.0));

const B_PRIME: CoordWingEdges = CoordWingEdges(arr_inv(&B.0));

//  3 -> 0 -> 23 -> 20
#[allow(non_upper_case_globals)]
const b_SLICE: CoordWingEdges = CoordWingEdges([23, 1, 2, 0, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 3, 21, 22, 20]);

#[allow(non_upper_case_globals)]
const Bw: CoordWingEdges = CoordWingEdges(permute_arr(&B.0, &b_SLICE.0));

#[allow(non_upper_case_globals)]
const Bw2: CoordWingEdges = CoordWingEdges(permute_arr(&Bw.0, &Bw.0));

#[allow(non_upper_case_globals)]
const Bw_PRIME: CoordWingEdges = CoordWingEdges(arr_inv(&Bw.0));

// 3 -> 14 -> 19 -> 10
// 4 -> 9 -> 20 -> 13
const L: CoordWingEdges = CoordWingEdges([0, 1, 2, 14, 9, 5, 6, 7, 8, 20, 3, 11, 12, 4, 19, 15, 16, 17, 18, 10, 13, 21, 22, 23]);

const L2: CoordWingEdges = CoordWingEdges(permute_arr(&L.0, &L.0));

const L_PRIME: CoordWingEdges = CoordWingEdges(arr_inv(&L.0));

 // 2 -> 21 -> 18 -> 5
#[allow(non_upper_case_globals)]
const l_SLICE: CoordWingEdges = CoordWingEdges([0, 1, 21, 3, 4, 2, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 5, 19, 20, 18, 22, 23]);

#[allow(non_upper_case_globals)]
const Lw: CoordWingEdges = CoordWingEdges(permute_arr(&L.0, &l_SLICE.0));

#[allow(non_upper_case_globals)]
const Lw2: CoordWingEdges = CoordWingEdges(permute_arr(&Lw.0, &Lw.0));

#[allow(non_upper_case_globals)]
const Lw_PRIME: CoordWingEdges = CoordWingEdges(arr_inv(&Lw.0));

// 16 -> 18 -> 20 -> 22
// 17 -> 19 -> 21 -> 23
const D: CoordWingEdges = CoordWingEdges([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 18, 19, 20, 21, 22, 23, 16, 17]);

const D2: CoordWingEdges = CoordWingEdges(permute_arr(&D.0, &D.0));

const D_PRIME: CoordWingEdges = CoordWingEdges(arr_inv(&D.0));

// 12 -> 13 -> 14 -> 15
#[allow(non_upper_case_globals)]
const d_SLICE: CoordWingEdges = CoordWingEdges([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 14, 15, 12, 16, 17, 18, 19, 20, 21, 22, 23]);

#[allow(non_upper_case_globals)]
const Dw: CoordWingEdges = CoordWingEdges(permute_arr(&D.0, &d_SLICE.0));

#[allow(non_upper_case_globals)]
const Dw2: CoordWingEdges = CoordWingEdges(permute_arr(&Dw.0, &Dw.0));

#[allow(non_upper_case_globals)]
const Dw_PRIME: CoordWingEdges = CoordWingEdges(arr_inv(&Dw.0));

use super::move_sets::wide_turns::WideTurn;
impl From<WideTurn> for CoordWingEdges {
    fn from(wt: WideTurn) -> CoordWingEdges {
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

use super::move_sets::g1_wide_turns::G1WideTurn;
impl From<G1WideTurn> for CoordWingEdges {
    fn from(wt: G1WideTurn) -> CoordWingEdges {
        match wt {
            G1WideTurn::U => U,
            G1WideTurn::U2 => U2,
            G1WideTurn::UPrime => U_PRIME,
            G1WideTurn::Uw2 => Uw2,
            G1WideTurn::F2 => F2,
            G1WideTurn::Fw2 => Fw2,
            G1WideTurn::R2 => R2,
            G1WideTurn::Rw2 => Rw2,
            G1WideTurn::B2 => B2,
            G1WideTurn::Bw2 => Bw2,
            G1WideTurn::L2 => L2,
            G1WideTurn::Lw2 => Lw2,
            G1WideTurn::D => D,
            G1WideTurn::D2 => D2,
            G1WideTurn::DPrime => D_PRIME,
            G1WideTurn::Dw2 => Dw2,
        }
    }
}

// 6 -> 11 -> 7
// 5 -> 12 -> 0
// 1 -> 10 -> 16
// 2 -> 13 -> 23
// 8 -> 4 -> 17
// 15 -> 3 -> 18
// 9 -> 19 -> 22
// 14 -> 20 -> 21
const S_URF: CoordWingEdges = CoordWingEdges([5, 10, 13, 18, 17, 12, 11, 6, 4, 19, 16, 7, 0, 23, 20, 3, 1, 8, 15, 22, 21, 14, 9, 2]);

const S_F: CoordWingEdges = CoordWingEdges(permute_arr(&Fw2.0, &Bw2.0));

const S_U: CoordWingEdges =  CoordWingEdges(permute_arr(&Uw.0, &Dw_PRIME.0));

// TODO:  Notably, this does NOT render correctly if done only once, it must be
// applied as a symmetry (S * X * S^-1), because it would also need to flip all
// edges too, and we don't track that.  This shouldn't cause a practical
// problem, but we couldtrack if we should flip all edges for the purpose of
// converting into a FaceletWingEdges representation.
const S_MRL: CoordWingEdges = CoordWingEdges([3, 2, 1, 0, 7, 6, 5, 4, 9, 8, 11, 10, 13, 12, 15, 14, 19, 18, 17, 16, 23, 22, 21, 20]);

use super::move_sets::g1_symmetry_generators::G1SymmetryGenerator;
impl From<G1SymmetryGenerator> for CoordWingEdges {
    fn from(g1sg: G1SymmetryGenerator) -> CoordWingEdges {
        match g1sg {
            G1SymmetryGenerator::SF => S_F,
            G1SymmetryGenerator::SU => S_U,
            G1SymmetryGenerator::SMrl => S_MRL,
        }
    }
}

use super::move_sets::g1_symmetry_generators::G1SymGenList;
impl From<G1SymGenList> for CoordWingEdges {
    fn from(sgl: G1SymGenList) -> CoordWingEdges {
        let mut perm = CoordWingEdges::identity();
        for x in sgl.0 {
            perm = perm.permute(x.into());
        }
        perm
    }
}

use super::equivalence_class::EquivalenceClass;
impl EquivalenceClass<G1SymGenList> for CoordWingEdges {
    fn get_equivalent(self, sgl: &G1SymGenList) -> CoordWingEdges {
        let x = CoordWingEdges::from(sgl.clone());
        x.invert().permute(self).permute(x)
    }
}

use super::move_sets::symmetry_generators::SymmetryGenerator;
impl From<SymmetryGenerator> for CoordWingEdges {
    fn from(g1sg: SymmetryGenerator) -> CoordWingEdges {
        match g1sg {
            SymmetryGenerator::SUrf => S_URF,
            SymmetryGenerator::SF => S_F,
            SymmetryGenerator::SU => S_U,
            SymmetryGenerator::SMrl => S_MRL,
        }
    }
}

use super::move_sets::symmetry_generators::SymGenList;
impl From<SymGenList> for CoordWingEdges {
    fn from(sgl: SymGenList) -> CoordWingEdges {
        let mut perm = CoordWingEdges::identity();
        for x in sgl.0 {
            perm = perm.permute(x.into());
        }
        perm
    }
}

impl EquivalenceClass<SymGenList> for CoordWingEdges {
    fn get_equivalent(self, sgl: &SymGenList) -> CoordWingEdges {
        let x = CoordWingEdges::from(sgl.clone());
        x.invert().permute(self).permute(x)
    }
}

use super::facelet_wing_edges::FaceletWingEdges;
impl From<CoordWingEdges> for FaceletWingEdges {
    fn from(coord_wing_edges: CoordWingEdges) -> FaceletWingEdges {
        let mut arr = [0; 48];

        for i in 0..coord_wing_edges.0.len() {
            let indexes = to_facelets(i as u8);
            let facelets = to_facelets(coord_wing_edges.0[i]);
            arr[indexes.0 as usize] = facelets.0;
            arr[indexes.1 as usize] = facelets.1;
        }

        FaceletWingEdges(arr)
    }
}

impl fmt::Display for CoordWingEdges {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        FaceletWingEdges::from(*self).fmt(f)
    }
}

fn to_facelets(i: u8) -> (u8, u8) {
    match i {
      0 => (0, 17),
      1 => (26, 1),
      2 => (2, 25),
      3 => (34, 3),
      4 => (4, 33),
      5 => (10, 5),
      6 => (6, 9),
      7 => (18, 7),
      8 => (16, 27),
      9 => (24, 35),
      10 => (32, 11),
      11 => (8, 19),
      12 => (20, 15),
      13 => (12, 39),
      14 => (36, 31),
      15 => (28, 23),
      16 => (40, 21),
      17 => (14, 41),
      18 => (42, 13),
      19 => (38, 43),
      20 => (44, 37),
      21 => (30, 45),
      22 => (46, 29),
      23 => (22, 47),
      _ => panic!("Invalid edge input!"),
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

const fn arr_inv(a: &[u8; 24]) -> [u8; 24] {
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

const fn permute_arr(a: &[u8; 24], b: &[u8; 24]) -> [u8; 24] {
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
