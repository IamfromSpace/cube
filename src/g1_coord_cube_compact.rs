extern crate functional;

use super::permutation_group::PermutationGroup;
use super::invertable::Invertable;

/* This is exactly like the G1CoordCube, but it is more memory efficient.
 * It compacts each piece group into a single numeric representation to
 * take up the fewest bytes.  However, it's much more expensive to alter,
 * because to do turns we must convert it to its more usable version,
 * permute and then convert it back.
 */
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct G1CoordCubeCompact {
    corners: u16,
    top_bottom_edges: u16,
    middle_edges: u8,
}

impl functional::BinaryOperation<G1CoordCubeCompact> for G1CoordCubeCompact {
    fn apply(a: G1CoordCubeCompact, b: G1CoordCubeCompact) -> G1CoordCubeCompact {
        let a_from = super::g1_coord_cube::G1CoordCube::from(a);
        let b_from = super::g1_coord_cube::G1CoordCube::from(b);
        a_from.permute(b_from).into()

    }
}

impl functional::AssociativeOperation<G1CoordCubeCompact> for G1CoordCubeCompact { }

impl functional::Monoid<G1CoordCubeCompact> for G1CoordCubeCompact {
    fn one() -> G1CoordCubeCompact {
        G1CoordCubeCompact {
            corners: 0,
            top_bottom_edges: 0,
            middle_edges: 0
        }
    }
}

impl Invertable for G1CoordCubeCompact {
    fn invert(&self) -> G1CoordCubeCompact {
        super::g1_coord_cube::G1CoordCube::from(*self).invert().into()
    }
}

impl PermutationGroup for G1CoordCubeCompact {}

const U: G1CoordCubeCompact = G1CoordCubeCompact {
    corners: 5880,
    top_bottom_edges: 5880,
    middle_edges: 0
};

const U_PRIME: G1CoordCubeCompact = G1CoordCubeCompact {
    corners: 15120,
    top_bottom_edges: 15120,
    middle_edges: 0
};

const F2: G1CoordCubeCompact = G1CoordCubeCompact {
    corners: 288,
    top_bottom_edges: 54,
    middle_edges: 1
};

const R2: G1CoordCubeCompact = G1CoordCubeCompact {
    corners: 21099,
    top_bottom_edges: 21024,
    middle_edges: 27
};

const B2: G1CoordCubeCompact = G1CoordCubeCompact {
    corners: 34864,
    top_bottom_edges: 4473,
    middle_edges: 8
};

const L2: G1CoordCubeCompact = G1CoordCubeCompact {
    corners: 3420,
    top_bottom_edges: 512,
    middle_edges: 2
};

const D: G1CoordCubeCompact = G1CoordCubeCompact {
    corners: 9,
    top_bottom_edges: 9,
    middle_edges: 0
};

const D_PRIME: G1CoordCubeCompact = G1CoordCubeCompact {
    corners: 18,
    top_bottom_edges: 18,
    middle_edges: 0
};

use super::move_sets::g1_turns::G1Turn;
impl From<G1Turn> for G1CoordCubeCompact {
    fn from(g1t: G1Turn) -> G1CoordCubeCompact {
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

const S_F: G1CoordCubeCompact = G1CoordCubeCompact {
    corners: 35152,
    top_bottom_edges: 35152,
    middle_edges: 9
};

const S_U: G1CoordCubeCompact = G1CoordCubeCompact {
    corners: 5898,
    top_bottom_edges: 5898,
    middle_edges: 11
};

const S_MRL: G1CoordCubeCompact = G1CoordCubeCompact {
    corners: 5167,
    top_bottom_edges: 5167,
    middle_edges: 9
};

use super::move_sets::g1_symmetry_generators::G1SymmetryGenerator;
impl From<G1SymmetryGenerator> for G1CoordCubeCompact {
    fn from(g1sg: G1SymmetryGenerator) -> G1CoordCubeCompact {
        match g1sg {
            G1SymmetryGenerator::SF => S_F,
            G1SymmetryGenerator::SU => S_U,
            G1SymmetryGenerator::SMrl => S_MRL,
        }
    }
}

impl From<super::g1_coord_cube::G1CoordCube> for G1CoordCubeCompact {
    fn from(g1c: super::g1_coord_cube::G1CoordCube) -> G1CoordCubeCompact {
        G1CoordCubeCompact {
            corners: arr_to_index_8(g1c.corners),
            top_bottom_edges: arr_to_index_8(g1c.top_bottom_edges),
            middle_edges: arr_to_index_4(g1c.middle_edges),
        }
    }
}

impl From<G1CoordCubeCompact> for super::g1_coord_cube::G1CoordCube {
    fn from(g1cc: G1CoordCubeCompact) -> super::g1_coord_cube::G1CoordCube {
        super::g1_coord_cube::G1CoordCube {
            corners: index_to_arr_8(g1cc.corners),
            top_bottom_edges: index_to_arr_8(g1cc.top_bottom_edges),
            middle_edges: index_to_arr_4(g1cc.middle_edges),
        }
    }
}

// TODO: This is O(n^2), but with popcnt it could be O(n) or O(nlogn)
// (depending if it's available as a single instruction)
fn index_to_arr_4(index: u8) -> [u8; 4] {
    let mut arr = [
        index / 8,
        index / 2 % 4,
        index % 2,
        0,
    ];

    for i in (0..arr.len()).rev() {
        for j in (0..i).rev() {
            if arr[j] <= arr[i] {
                arr[i] = arr[i] + 1;
            }
        }
    }

    arr
}

fn index_to_arr_8(index: u16) -> [u8; 8] {
    let mut arr = [
        (index / 5040) as u8,
        (index / 720 % 7) as u8,
        (index / 120 % 6) as u8,
        (index / 24 % 5) as u8,
        (index / 6 % 4) as u8,
        (index / 2 % 3) as u8,
        (index % 2) as u8,
        0,
    ];

    for i in (0..arr.len()).rev() {
        for j in (0..i).rev() {
            if arr[j] <= arr[i] {
                arr[i] = arr[i] + 1;
            }
        }
    }

    arr
}

fn arr_to_index_4(arr_raw: [u8; 4]) -> u8 {
    let mut arr = arr_raw.clone();
    for i in (1..arr.len() - 1).rev() {
        for j in 0..i {
            if arr[j] < arr_raw[i] {
                arr[i] = arr[i] - 1;
            }
        }
    }
    arr[0] * 8 + arr[1] * 2 + arr[2]
}

// TODO: arr_to_index_4 demonstrates using powers of two instead of a
// fully accurate mixed-radix, and it has an ever so slight speed increase.
// It should be greater here, but it costs 17 bits instead of 16.
// Luckily, middle_edges has a couple bits to spare, so the radix-2 case
// can be stored in it.
fn arr_to_index_8(arr_raw: [u8; 8]) -> u16 {
    let mut arr = arr_raw.clone();
    for i in (1..arr.len() - 1).rev() {
        for j in 0..i {
            if arr[j] < arr_raw[i] {
                arr[i] = arr[i] - 1;
            }
        }
    }
    arr[0] as u16 * 5040
        + arr[1] as u16 * 720
        + arr[2] as u16 * 120
        + arr[3] as u16 * 24
        + arr[4] as u16 * 6
        + arr[5] as u16 * 2
        + arr[6] as u16
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;

    #[test]
    fn index_to_arr_4_works_for_0_case() {
        assert_eq!(index_to_arr_4(0), [0, 1, 2, 3]);
    }

    #[test]
    fn index_to_arr_4_works_for_max_case() {
        assert_eq!(index_to_arr_4(29), [3, 2, 1, 0]);
    }

    #[test]
    fn index_to_arr_8_works_for_0_case() {
        assert_eq!(index_to_arr_8(0), [0, 1, 2, 3, 4, 5, 6, 7]);
    }

    #[test]
    fn index_to_arr_8_works_for_max_case() {
        assert_eq!(index_to_arr_8(40319), [7, 6, 5, 4, 3, 2, 1, 0]);
    }

    #[test]
    fn arr_to_index_4_works_for_0_case() {
        assert_eq!(arr_to_index_4([0, 1, 2, 3]), 0);
    }

    #[test]
    fn arr_to_index_4_works_for_max_case() {
        assert_eq!(arr_to_index_4([3, 2, 1, 0]), 29);
    }

    #[test]
    fn arr_to_index_8_works_for_0_case() {
        assert_eq!(arr_to_index_8([0, 1, 2, 3, 4, 5, 6, 7]), 0);
    }

    #[test]
    fn arr_to_index_8_works_for_max_case() {
        assert_eq!(arr_to_index_8([7, 6, 5, 4, 3, 2, 1, 0]), 40319);
    }

    #[test]
    fn indexes_round_trip_4() {
        let val = 17;
        assert_eq!(arr_to_index_4(index_to_arr_4(val)), val);

        let val = 9;
        assert_eq!(arr_to_index_4(index_to_arr_4(val)), val);
    }

    #[test]
    fn indexes_round_trip_8() {
        let val = 23132;
        assert_eq!(arr_to_index_8(index_to_arr_8(val)), val);

        let val = 19732;
        assert_eq!(arr_to_index_8(index_to_arr_8(val)), val);
    }

    #[test]
    fn u_is_not_identity() {
        assert_ne!(G1CoordCubeCompact::from(G1Turn::U), G1CoordCubeCompact::identity());
    }

    #[test]
    fn should_be_able_to_generate_one_turn_with_a_combination_of_the_other_five() {
        //L2 = D U' F2 R2 B2 D' U R2 F2
        let l2 = G1CoordCubeCompact::from(G1Turn::D)
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
        let d = G1CoordCubeCompact::from(G1Turn::D)
            .permute(G1Turn::DPrime.into());
        assert_eq!(d, G1CoordCubeCompact::identity());
    }

    #[test]
    fn u_and_u_prime_should_be_the_identity() {
        let u = G1CoordCubeCompact::from(G1Turn::U)
            .permute(G1Turn::UPrime.into());
        assert_eq!(u, G1CoordCubeCompact::identity());
    }

    #[test]
    fn u_is_symmetrical_to_d() {
        let d = G1CoordCubeCompact::from(G1SymmetryGenerator::SF)
            .permute(G1Turn::U.into())
            .permute(G1SymmetryGenerator::SF.into());
        assert_eq!(d, G1Turn::D.into());
    }

    #[test]
    fn f2_is_symmetrical_to_r2() {
        let r2 = G1CoordCubeCompact::from(G1SymmetryGenerator::SU)
            .permute(G1Turn::F2.into())
            .permute(G1SymmetryGenerator::SU.into())
            .permute(G1SymmetryGenerator::SU.into())
            .permute(G1SymmetryGenerator::SU.into());
        assert_eq!(r2, G1Turn::R2.into());
    }

    #[test]
    fn u_is_symmetrical_to_u_prime() {
        let u_prime = G1CoordCubeCompact::from(G1SymmetryGenerator::SMrl)
            .permute(G1Turn::U.into())
            .permute(G1SymmetryGenerator::SMrl.into());
        assert_eq!(u_prime, G1Turn::UPrime.into());
    }

    #[bench]
    fn repeat_all_turn_identity_sequence(b: &mut Bencher) {
        b.iter(|| {
            G1CoordCubeCompact::from(G1Turn::D)
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
