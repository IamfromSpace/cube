extern crate functional;

pub mod locked_evens;

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
const U: CoordWingEdges = CoordWingEdges(arr_inv(&U_PRIME.0));

const U2: CoordWingEdges = CoordWingEdges(permute_arr(&U.0, &U.0));

const U_PRIME: CoordWingEdges = CoordWingEdges([2, 3, 4, 5, 6, 7, 0, 1, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23]);

// 8 -> 9 -> 10 -> 11
#[allow(non_upper_case_globals)]
const u_SLICE: CoordWingEdges = CoordWingEdges(arr_inv(&u_SLICE_PRIME.0));

#[allow(non_upper_case_globals)]
const u_SLICE_PRIME: CoordWingEdges = CoordWingEdges([0, 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 8, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23]);

#[allow(non_upper_case_globals)]
const Uw: CoordWingEdges = CoordWingEdges(permute_arr(&U.0, &u_SLICE.0));

#[allow(non_upper_case_globals)]
const Uw2: CoordWingEdges = CoordWingEdges(permute_arr(&Uw.0, &Uw.0));

#[allow(non_upper_case_globals)]
const Uw_PRIME: CoordWingEdges = CoordWingEdges(arr_inv(&Uw.0));

// 11 -> 5 -> 13 -> 17
// 12 -> 6 -> 10 -> 18
const F: CoordWingEdges = CoordWingEdges(arr_inv(&F_PRIME.0));

const F2: CoordWingEdges = CoordWingEdges(permute_arr(&F.0, &F.0));

const F_PRIME: CoordWingEdges = CoordWingEdges([0, 1, 2, 3, 4, 13, 10, 7, 8, 9, 18, 5, 6, 17, 14, 15, 16, 11, 12, 19, 20, 21, 22, 23]);

// 7 -> 4 -> 19 -> 16
#[allow(non_upper_case_globals)]
const f_SLICE: CoordWingEdges = CoordWingEdges(arr_inv(&f_SLICE_PRIME.0));

#[allow(non_upper_case_globals)]
const f_SLICE_PRIME: CoordWingEdges = CoordWingEdges([0, 1, 2, 3, 19, 5, 6, 4, 8, 9, 10, 11, 12, 13, 14, 15, 7, 17, 18, 16, 20, 21, 22, 23]);

#[allow(non_upper_case_globals)]
const Fw: CoordWingEdges = CoordWingEdges(permute_arr(&F.0, &f_SLICE.0));

#[allow(non_upper_case_globals)]
const Fw2: CoordWingEdges = CoordWingEdges(permute_arr(&Fw.0, &Fw.0));

#[allow(non_upper_case_globals)]
const Fw_PRIME: CoordWingEdges = CoordWingEdges(arr_inv(&Fw.0));

// 8 -> 7 -> 12 -> 23
// 15 -> 0 -> 11 -> 16
const R: CoordWingEdges = CoordWingEdges(arr_inv(&R_PRIME.0));

const R2: CoordWingEdges = CoordWingEdges(permute_arr(&R.0, &R.0));

const R_PRIME: CoordWingEdges = CoordWingEdges([11, 1, 2, 3, 4, 5, 6, 12, 7, 9, 10, 16, 23, 13, 14, 0, 15, 17, 18, 19, 20, 21, 22, 8]);

 // 1 -> 6 -> 17 -> 22
#[allow(non_upper_case_globals)]
const r_SLICE: CoordWingEdges = CoordWingEdges(arr_inv(&r_SLICE_PRIME.0));

#[allow(non_upper_case_globals)]
const r_SLICE_PRIME: CoordWingEdges = CoordWingEdges([0, 6, 2, 3, 4, 5, 17, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 22, 18, 19, 20, 21, 1, 23]);

#[allow(non_upper_case_globals)]
const Rw: CoordWingEdges = CoordWingEdges(permute_arr(&R.0, &r_SLICE.0));

#[allow(non_upper_case_globals)]
const Rw2: CoordWingEdges = CoordWingEdges(permute_arr(&Rw.0, &Rw.0));

#[allow(non_upper_case_globals)]
const Rw_PRIME: CoordWingEdges = CoordWingEdges(arr_inv(&Rw.0));

// 1 -> 15 -> 21 -> 9
// 2 -> 8 -> 22 -> 14
const B: CoordWingEdges = CoordWingEdges(arr_inv(&B_PRIME.0));

const B2: CoordWingEdges = CoordWingEdges(permute_arr(&B.0, &B.0));

const B_PRIME: CoordWingEdges = CoordWingEdges([0, 15, 8, 3, 4, 5, 6, 7, 22, 1, 10, 11, 12, 13, 2, 21, 16, 17, 18, 19, 20, 9, 14, 23]);

//  3 -> 0 -> 23 -> 20
#[allow(non_upper_case_globals)]
const b_SLICE: CoordWingEdges = CoordWingEdges(arr_inv(&b_SLICE_PRIME.0));

#[allow(non_upper_case_globals)]
const b_SLICE_PRIME: CoordWingEdges = CoordWingEdges([23, 1, 2, 0, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 3, 21, 22, 20]);

#[allow(non_upper_case_globals)]
const Bw: CoordWingEdges = CoordWingEdges(permute_arr(&B.0, &b_SLICE.0));

#[allow(non_upper_case_globals)]
const Bw2: CoordWingEdges = CoordWingEdges(permute_arr(&Bw.0, &Bw.0));

#[allow(non_upper_case_globals)]
const Bw_PRIME: CoordWingEdges = CoordWingEdges(arr_inv(&Bw.0));

// 3 -> 14 -> 19 -> 10
// 4 -> 9 -> 20 -> 13
const L: CoordWingEdges = CoordWingEdges(arr_inv(&L_PRIME.0));

const L2: CoordWingEdges = CoordWingEdges(permute_arr(&L.0, &L.0));

const L_PRIME: CoordWingEdges = CoordWingEdges([0, 1, 2, 14, 9, 5, 6, 7, 8, 20, 3, 11, 12, 4, 19, 15, 16, 17, 18, 10, 13, 21, 22, 23]);

 // 2 -> 21 -> 18 -> 5
#[allow(non_upper_case_globals)]
const l_SLICE: CoordWingEdges = CoordWingEdges(arr_inv(&l_SLICE_PRIME.0));

#[allow(non_upper_case_globals)]
const l_SLICE_PRIME: CoordWingEdges = CoordWingEdges([0, 1, 21, 3, 4, 2, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 5, 19, 20, 18, 22, 23]);

#[allow(non_upper_case_globals)]
const Lw: CoordWingEdges = CoordWingEdges(permute_arr(&L.0, &l_SLICE.0));

#[allow(non_upper_case_globals)]
const Lw2: CoordWingEdges = CoordWingEdges(permute_arr(&Lw.0, &Lw.0));

#[allow(non_upper_case_globals)]
const Lw_PRIME: CoordWingEdges = CoordWingEdges(arr_inv(&Lw.0));

// 16 -> 18 -> 20 -> 22
// 17 -> 19 -> 21 -> 23
const D: CoordWingEdges = CoordWingEdges(arr_inv(&D_PRIME.0));

const D2: CoordWingEdges = CoordWingEdges(permute_arr(&D.0, &D.0));

const D_PRIME: CoordWingEdges = CoordWingEdges([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 18, 19, 20, 21, 22, 23, 16, 17]);

// 12 -> 13 -> 14 -> 15
#[allow(non_upper_case_globals)]
const d_SLICE: CoordWingEdges = CoordWingEdges(arr_inv(&d_SLICE_PRIME.0));

#[allow(non_upper_case_globals)]
const d_SLICE_PRIME: CoordWingEdges = CoordWingEdges([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 14, 15, 12, 16, 17, 18, 19, 20, 21, 22, 23]);

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
const S_URF: CoordWingEdges = CoordWingEdges([12, 16, 23, 15, 8, 0, 7, 11, 17, 22, 1, 6, 5, 2, 21, 18, 10, 4, 3, 9, 14, 20, 19, 13]);

const S_F: CoordWingEdges = CoordWingEdges(permute_arr(&Fw.0, &Bw_PRIME.0));

const S_F2: CoordWingEdges = CoordWingEdges(permute_arr(&S_F.0, &S_F.0));

const S_U: CoordWingEdges =  CoordWingEdges(permute_arr(&Uw.0, &Dw_PRIME.0));

const S_U2: CoordWingEdges = CoordWingEdges(permute_arr(&S_U.0, &S_U.0));

const S_U2F2: CoordWingEdges = CoordWingEdges(permute_arr(&S_U2.0, &S_F2.0));

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
            G1SymmetryGenerator::SF => S_F2,
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
            SymmetryGenerator::SF => S_F2,
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


// TODO: How do we make this abstract....?
// If we only allow <U, Uw2, F, Fw2, R2, Rw2, B, Bw2, L2, Lw2, D, Dw2>
// then pieces are locked in even and odd orbits.
// This gives U2, F2, and M_RL symmetries
// 0 2 4 6 8 10 12 14 16 18 20 22
pub fn sort_odds_and_evens(x: &CoordWingEdges) -> CoordWingEdges {
    let mut odds = [0; 12];
    let mut odd_count = 0;
    let mut evens = [0; 12];
    let mut even_count = 0;
    for i in 0..24 {
        if x.0[i] % 2 == 0 {
            evens[even_count] = i;
            even_count += 1;
        } else {
            odds[odd_count] = i;
            odd_count += 1;
        }
    }
    let mut y = [0u8; 24];
    for i in 0..12 {
        y[evens[i]] = i as u8 * 2;
        y[odds[i]] = i as u8 * 2 + 1;
    }
    CoordWingEdges(y)
}

// Orientation only sort of means orientation here.  It really means in the
// correct orbit, but it is quite a lot like normal orientation, because there
// are two edges of each color pairing, and if the two are swapped, they will
// appear flipped.
pub fn edges_are_oriented(x: &CoordWingEdges) -> bool {
    // If the index is odd, the value must be, and if the index is even, the
    // value must be
    (0..24).all(|i| (i % 2 == 0) == (x.0[i] % 2 == 0))
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
        b[a[0] as usize],
        b[a[1] as usize],
        b[a[2] as usize],
        b[a[3] as usize],
        b[a[4] as usize],
        b[a[5] as usize],
        b[a[6] as usize],
        b[a[7] as usize],
        b[a[8] as usize],
        b[a[9] as usize],
        b[a[10] as usize],
        b[a[11] as usize],
        b[a[12] as usize],
        b[a[13] as usize],
        b[a[14] as usize],
        b[a[15] as usize],
        b[a[16] as usize],
        b[a[17] as usize],
        b[a[18] as usize],
        b[a[19] as usize],
        b[a[20] as usize],
        b[a[21] as usize],
        b[a[22] as usize],
        b[a[23] as usize],
    ]
}


#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck::Gen;
    use rand::Rng;

    impl quickcheck::Arbitrary for WideTurn {
        fn arbitrary<G: Gen>(g: &mut G) -> WideTurn {
            *g.choose(&[
              WideTurn::U,
              WideTurn::U2,
              WideTurn::UPrime,
              WideTurn::Uw,
              WideTurn::Uw2,
              WideTurn::UwPrime,
              WideTurn::F,
              WideTurn::F2,
              WideTurn::FPrime,
              WideTurn::Fw,
              WideTurn::Fw2,
              WideTurn::FwPrime,
              WideTurn::R,
              WideTurn::R2,
              WideTurn::RPrime,
              WideTurn::Rw,
              WideTurn::Rw2,
              WideTurn::RwPrime,
              WideTurn::B,
              WideTurn::B2,
              WideTurn::BPrime,
              WideTurn::Bw,
              WideTurn::Bw2,
              WideTurn::BwPrime,
              WideTurn::L,
              WideTurn::L2,
              WideTurn::LPrime,
              WideTurn::Lw,
              WideTurn::Lw2,
              WideTurn::LwPrime,
              WideTurn::D,
              WideTurn::D2,
              WideTurn::DPrime,
              WideTurn::Dw,
              WideTurn::Dw2,
              WideTurn::DwPrime,
            ]).unwrap()
        }
    }

    impl quickcheck::Arbitrary for SymmetryGenerator {
        fn arbitrary<G: Gen>(g: &mut G) -> SymmetryGenerator {
            *g.choose(&[
                SymmetryGenerator::SUrf,
                SymmetryGenerator::SF,
                SymmetryGenerator::SU,
                SymmetryGenerator::SMrl,
            ]).unwrap()
        }
    }

    impl quickcheck::Arbitrary for SymGenList {
        fn arbitrary<G: Gen>(g: &mut G) -> SymGenList {
            SymGenList(quickcheck::Arbitrary::arbitrary(g))
        }
    }

    use quickcheck::TestResult;
    quickcheck! {
        fn applying_turns_creates_the_equivalent_of_applying_equivalent_turns(turns: Vec<WideTurn>, sym: SymGenList) -> TestResult {
            // TODO: This test doesn't seem to work if I mess with stuff
            if turns.len() == 0 || sym.0.len() == 0 {
                TestResult::discard()
            } else {
                let mut scramble = CoordWingEdges::identity();
                for turn in &turns {
                    scramble = scramble.permute((*turn).into());
                }
                let final_state_equivalent = scramble.get_equivalent(&sym);

                let mut scramble = CoordWingEdges::identity();
                for turn in &turns {
                    scramble = scramble.permute(turn.get_equivalent(&sym).into());
                }
                let turn_equivalent = scramble;

                TestResult::from_bool(turn_equivalent == final_state_equivalent)
                // TestResult::from_bool(turns.len() == 0)
            }
        }
    }

    quickcheck! {
        fn facelet_result_with_the_same_turn_matches(turns: Vec<WideTurn>) -> TestResult {
            if turns.len() == 0 {
                TestResult::discard()
            } else {
                let mut scramble = CoordWingEdges::identity();
                for turn in &turns {
                    scramble = scramble.permute((*turn).into());
                }
                let coord = scramble;

                let mut scramble = FaceletWingEdges::identity();
                for turn in &turns {
                    scramble = scramble.permute((*turn).into());
                }
                let facelet = scramble;

                TestResult::from_bool(facelet == coord.into())
            }
        }
    }

    fn no_effect_on_orientation(turn: &WideTurn) -> bool {
        turn == &WideTurn::U ||
        turn == &WideTurn::U2 ||
        turn == &WideTurn::UPrime ||
        turn == &WideTurn::Uw2 ||
        turn == &WideTurn::F ||
        turn == &WideTurn::F2 ||
        turn == &WideTurn::FPrime ||
        turn == &WideTurn::Fw2 ||
        turn == &WideTurn::R2 ||
        turn == &WideTurn::Rw2 ||
        turn == &WideTurn::B ||
        turn == &WideTurn::B2 ||
        turn == &WideTurn::BPrime ||
        turn == &WideTurn::Bw2 ||
        turn == &WideTurn::L2 ||
        turn == &WideTurn::Lw2 ||
        turn == &WideTurn::D ||
        turn == &WideTurn::D2 ||
        turn == &WideTurn::DPrime ||
        turn == &WideTurn::Dw2
    }

    quickcheck! {
        fn the_identity_is_not_affected_by_some_turns_after_sorted_odds_and_evens(turns: Vec<WideTurn>) -> TestResult {
            // Note that this _only_ applies to the identity, this doesn't hold
            // for an arbitrary scramble.  For example, sorting [Bw] is not the
            // same as sorting [Bw, U], even though [] and [U] are.
            let mut scramble = CoordWingEdges::identity();
            for turn in &turns {
                if no_effect_on_orientation(turn) {
                    scramble = scramble.permute((*turn).into());
                }
            }
            let scramble = sort_odds_and_evens(&scramble);

            TestResult::from_bool(&scramble == &CoordWingEdges::identity() && edges_are_oriented(&scramble))
        }
    }

    quickcheck! {
        fn some_turns_alter_orientation(turn: WideTurn) -> TestResult {
            if no_effect_on_orientation(&turn) {
                TestResult::discard()
            } else {
                let scramble: CoordWingEdges = turn.into();
                TestResult::from_bool(!edges_are_oriented(&scramble))
            }

        }
    }
}
