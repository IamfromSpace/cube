extern crate functional;

use super::permutation_group::PermutationGroup;
use super::invertable::Invertable;

/* A Cube in G1 can only rotate the top and bottom side with quarter turns,
 * all others are 180 deg turns only.  This means that the edges and corners
 * are always "oriented," there is no move set that can get them back to the
 * same position "twisted."
 *
 * Corners can move to any other corner position, but the edges are caught
 * in two tracks.  The top and bottom edges are in one track, and the UD middle
 * slice are in the other.
 *
 * This is a very symmetric group; the only symmetry that doesn't apply is S_URF
 *
 * This is the non-compact version of this coordinate.  It's not as memory
 * efficient but it's drastically easier to operate on.  The other is for
 * storage, this is for permuting.
 */
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct G1CoordCube {
    pub corners: [u8; 8],
    pub top_bottom_edges: [u8; 8],
    pub middle_edges: [u8; 4],
}

impl functional::BinaryOperation<G1CoordCube> for G1CoordCube {
    fn apply(a: G1CoordCube, b: G1CoordCube) -> G1CoordCube {
        G1CoordCube {
            corners: permute_arr_8(&a.corners, &b.corners),
            top_bottom_edges: permute_arr_8(&a.top_bottom_edges, &b.top_bottom_edges),
            middle_edges: permute_arr_4(&a.middle_edges, &b.middle_edges)
        }
    }
}

impl functional::AssociativeOperation<G1CoordCube> for G1CoordCube { }

impl functional::Monoid<G1CoordCube> for G1CoordCube {
    fn one() -> G1CoordCube {
        G1CoordCube {
            corners: arr_identity_8(),
            top_bottom_edges: arr_identity_8(),
            middle_edges: arr_identity_4()
        }
    }
}

impl Invertable for G1CoordCube {
    fn invert(&self) -> G1CoordCube {
        G1CoordCube {
            corners: arr_inv_8(&self.corners),
            top_bottom_edges: arr_inv_8(&self.top_bottom_edges),
            middle_edges: arr_inv_4(&self.middle_edges)
        }
    }
}

impl PermutationGroup for G1CoordCube {}

const U: G1CoordCube = G1CoordCube {
    corners: [1, 2, 3, 0, 4, 5, 6, 7],
    top_bottom_edges: [1, 2, 3, 0, 4, 5, 6, 7],
    middle_edges: [0, 1, 2, 3],
};

const U_PRIME: G1CoordCube = G1CoordCube {
    corners: [3, 0, 1, 2, 4, 5, 6, 7],
    top_bottom_edges: [3, 0, 1, 2, 4, 5, 6, 7],
    middle_edges: [0, 1, 2, 3],
};

const F2: G1CoordCube = G1CoordCube {
    corners: [0, 1, 4, 5, 2, 3, 6, 7],
    top_bottom_edges: [0, 1, 2, 5, 4, 3, 6, 7],
    middle_edges: [0, 1, 3, 2],
};

const R2: G1CoordCube = G1CoordCube {
    corners: [4, 1, 2, 7, 0, 5, 6, 3],
    top_bottom_edges: [4, 1, 2, 3, 0, 5, 6, 7],
    middle_edges: [3, 1, 2, 0],
};

const B2: G1CoordCube = G1CoordCube {
    corners: [6, 7, 2, 3, 4, 5, 0, 1],
    top_bottom_edges: [0, 7, 2, 3, 4, 5, 6, 1],
    middle_edges: [1, 0, 2, 3],
};

const L2: G1CoordCube = G1CoordCube {
    corners: [0, 5, 6, 3, 4, 1, 2, 7],
    top_bottom_edges: [0, 1, 6, 3, 4, 5, 2, 7],
    middle_edges: [0, 2, 1, 3],
};

const D: G1CoordCube = G1CoordCube {
    corners: [0, 1, 2, 3, 5, 6, 7, 4],
    top_bottom_edges: [0, 1, 2, 3, 5, 6, 7, 4],
    middle_edges: [0, 1, 2, 3],
};

const D_PRIME: G1CoordCube = G1CoordCube {
    corners: [0, 1, 2, 3, 7, 4, 5, 6],
    top_bottom_edges: [0, 1, 2, 3, 7, 4, 5, 6],
    middle_edges: [0, 1, 2, 3],
};

use super::move_sets::g1_turns::G1Turn;
impl From<G1Turn> for G1CoordCube {
    fn from(g1t: G1Turn) -> G1CoordCube {
        match g1t {
            G1Turn::U => U,
            G1Turn::UPrime => U_PRIME,
            G1Turn::F2 => F2,
            G1Turn::R2 => R2,
            G1Turn::B2 => B2,
            G1Turn::L2 => L2,
            G1Turn::D => D,
            G1Turn::DPrime => D_PRIME,
        }
    }
}

const S_F: G1CoordCube = G1CoordCube {
    corners: [6, 7, 4, 5, 2, 3, 0, 1],
    top_bottom_edges: [6, 7, 4, 5, 2, 3, 0, 1],
    middle_edges: [1, 0, 3, 2],
};

const S_U: G1CoordCube = G1CoordCube {
    corners: [1, 2, 3, 0, 7, 4, 5, 6],
    top_bottom_edges: [1, 2, 3, 0, 7, 4, 5, 6],
    middle_edges: [1, 2, 3, 0],
};

const S_MRL: G1CoordCube = G1CoordCube {
    corners: [1, 0, 3, 2, 5, 4, 7, 6],
    top_bottom_edges: [1, 0, 3, 2, 5, 4, 7, 6],
    middle_edges: [1, 0, 3, 2],
};

use super::move_sets::g1_symmetry_generators::G1SymmetryGenerator;
impl From<G1SymmetryGenerator> for G1CoordCube {
    fn from(g1sg: G1SymmetryGenerator) -> G1CoordCube {
        match g1sg {
            G1SymmetryGenerator::SF => S_F,
            G1SymmetryGenerator::SU => S_U,
            G1SymmetryGenerator::SMrl => S_MRL,
        }
    }
}

use super::move_sets::g1_symmetry_generators::G1SymGenList;
impl From<G1SymGenList> for G1CoordCube {
    fn from(sgl: G1SymGenList) -> G1CoordCube {
        let mut perm = G1CoordCube::identity();
        for x in sgl.0 {
            perm = perm.permute(x.into());
        }
        perm
    }
}

use super::equivalence_class::EquivalenceClass;
impl EquivalenceClass<G1SymGenList> for G1CoordCube {
    fn get_equivalent(self, sgl: &G1SymGenList) -> G1CoordCube {
        let x = G1CoordCube::from(sgl.clone());
        x.invert().permute(self).permute(x)
    }
}

fn permute_arr_8(a: &[u8; 8], b: &[u8; 8]) -> [u8; 8] {
    [
        a[b[0] as usize],
        a[b[1] as usize],
        a[b[2] as usize],
        a[b[3] as usize],
        a[b[4] as usize],
        a[b[5] as usize],
        a[b[6] as usize],
        a[b[7] as usize],
    ]
}

fn permute_arr_4(a: &[u8; 4], b: &[u8; 4]) -> [u8; 4] {
    [
        a[b[0] as usize],
        a[b[1] as usize],
        a[b[2] as usize],
        a[b[3] as usize],
    ]
}

fn arr_identity_8() -> [u8; 8] {
    [0, 1, 2, 3, 4, 5, 6, 7]
}

fn arr_identity_4() -> [u8; 4] {
    [0, 1, 2, 3]
}

fn arr_inv_8(a: &[u8; 8]) -> [u8; 8] {
    let mut r = [0; 8];
    //silly looking, but twice as fast ;)
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

fn arr_inv_4(a: &[u8; 4]) -> [u8; 4] {
    let mut r = [0; 4];
    //silly looking, but twice as fast ;)
    r[a[0] as usize] = 0;
    r[a[1] as usize] = 1;
    r[a[2] as usize] = 2;
    r[a[3] as usize] = 3;
    r
}

use std::convert::TryFrom;
use super::coord_cube::CoordCube;
use super::cubie_orientations_and_ud_slice::CubieOrientationAndUDSlice;
impl TryFrom<CoordCube> for G1CoordCube {
    type Error = ();

    fn try_from(coord_cube: CoordCube) -> Result<G1CoordCube, Self::Error> {
        if CubieOrientationAndUDSlice::from(coord_cube).is_solved() {
            let corners = [
                coord_cube.corners[0].0,
                coord_cube.corners[1].0,
                coord_cube.corners[2].0,
                coord_cube.corners[3].0,
                coord_cube.corners[4].0,
                coord_cube.corners[5].0,
                coord_cube.corners[6].0,
                coord_cube.corners[7].0,
            ];

            let top_bottom_edges = [
                coord_cube.edges[0].0,
                coord_cube.edges[1].0,
                coord_cube.edges[2].0,
                coord_cube.edges[3].0,
                coord_cube.edges[8].0 - 4,
                coord_cube.edges[9].0 - 4,
                coord_cube.edges[10].0 - 4,
                coord_cube.edges[11].0 - 4,
            ];

            let middle_edges = [
                coord_cube.edges[4].0 - 4,
                coord_cube.edges[5].0 - 4,
                coord_cube.edges[6].0 - 4,
                coord_cube.edges[7].0 - 4,
            ];

            Ok(G1CoordCube {
                corners,
                top_bottom_edges,
                middle_edges,
            })
        } else {
            Err(())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;

    #[test]
    fn should_be_able_to_generate_one_turn_with_a_combination_of_the_other_five() {
        //L2 = D U' F2 R2 B2 D' U R2 F2
        let l2 = G1CoordCube::from(G1Turn::D)
            .permute(G1Turn::UPrime.into())
            .permute(G1Turn::F2.into())
            .permute(G1Turn::R2.into())
            .permute(G1Turn::B2.into())
            .permute(G1Turn::DPrime.into())
            .permute(G1Turn::U.into())
            .permute(G1Turn::R2.into())
            .permute(G1Turn::F2.into());
        assert_eq!(l2, G1Turn::L2.into());
    }

    #[test]
    fn d_and_d_prime_should_be_the_identity() {
        let d = G1CoordCube::from(G1Turn::D)
            .permute(G1Turn::DPrime.into());
        assert_eq!(d, G1CoordCube::identity());
    }

    #[test]
    fn u_and_u_prime_should_be_the_identity() {
        let u = G1CoordCube::from(G1Turn::U)
            .permute(G1Turn::UPrime.into());
        assert_eq!(u, G1CoordCube::identity());
    }

    #[test]
    fn u_is_symmetrical_to_d() {
        let d = G1CoordCube::from(G1SymmetryGenerator::SF)
            .permute(G1Turn::U.into())
            .permute(G1SymmetryGenerator::SF.into());
        assert_eq!(d, G1Turn::D.into());
    }

    #[test]
    fn f2_is_symmetrical_to_r2() {
        let r2 = G1CoordCube::from(G1SymmetryGenerator::SU)
            .permute(G1Turn::F2.into())
            .permute(G1SymmetryGenerator::SU.into())
            .permute(G1SymmetryGenerator::SU.into())
            .permute(G1SymmetryGenerator::SU.into());
        assert_eq!(r2, G1Turn::R2.into());
    }

    #[test]
    fn u_is_symmetrical_to_u_prime() {
        let u_prime = G1CoordCube::from(G1SymmetryGenerator::SMrl)
            .permute(G1Turn::U.into())
            .permute(G1SymmetryGenerator::SMrl.into());
        assert_eq!(u_prime, G1Turn::UPrime.into());
    }

    #[bench]
    fn repeat_all_turn_identity_sequence(b: &mut Bencher) {
        b.iter(|| {
            G1CoordCube::from(G1Turn::D)
                .permute(G1Turn::UPrime.into())
                .permute(G1Turn::F2.into())
                .permute(G1Turn::R2.into())
                .permute(G1Turn::B2.into())
                .permute(G1Turn::DPrime.into())
                .permute(G1Turn::U.into())
                .permute(G1Turn::R2.into())
                .permute(G1Turn::F2.into())
                .permute(G1Turn::L2.into());
        });
    }
}
