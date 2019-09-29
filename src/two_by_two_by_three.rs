/* Is a coordinate that holds a fully solved edge and the
 * adjacent edges to the solved corners, making a cube that
 * is 2x2x3 in facelets.
 *
 * Since this is symmetrical via mirroring, we store a full
 * permutation of the edge facelets.
 */
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash, Serialize, Deserialize)]
pub struct TwoByTwoByThree {
    corner_positions: u8,
    corner_orientations_and_edge_positions: u32,
    edge_orientations: u8,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
struct TwoByTwoByThreeInternal {
    corner_positions: [u8; 2], // 2x8 6 bits
    corner_orientations: [[u8; 3]; 2], // 2x(3x3) 12 bits
    edge_positions: [u8; 5], // 5x12 20 bits
    edge_orientations: [bool; 5], // 5 bits
}

impl From<TwoByTwoByThree> for TwoByTwoByThreeInternal {
    fn from(tbtbt: TwoByTwoByThree) -> TwoByTwoByThreeInternal {
        TwoByTwoByThreeInternal {
            corner_positions: [
                (tbtbt.corner_positions as u8) & 0b111,
                ((tbtbt.corner_positions >> 3) as u8) & 0b111,
            ],
            corner_orientations: [
                [
                    (tbtbt.corner_orientations_and_edge_positions as u8) & 0b11,
                    ((tbtbt.corner_orientations_and_edge_positions >> 2) as u8) & 0b11,
                    ((tbtbt.corner_orientations_and_edge_positions >> 4) as u8) & 0b11,
                ],
                [
                    ((tbtbt.corner_orientations_and_edge_positions >> 6) as u8) & 0b11,
                    ((tbtbt.corner_orientations_and_edge_positions >> 8) as u8) & 0b11,
                    ((tbtbt.corner_orientations_and_edge_positions >> 10) as u8) & 0b11,
                ],
            ],
            edge_positions: [
                ((tbtbt.corner_orientations_and_edge_positions >> 12) as u8) & 0b1111,
                ((tbtbt.corner_orientations_and_edge_positions >> 16) as u8) & 0b1111,
                ((tbtbt.corner_orientations_and_edge_positions >> 20) as u8) & 0b1111,
                ((tbtbt.corner_orientations_and_edge_positions >> 24) as u8) & 0b1111,
                ((tbtbt.corner_orientations_and_edge_positions >> 28) as u8) & 0b1111,
            ],
            edge_orientations: [
                (tbtbt.edge_orientations & 1) == 1,
                ((tbtbt.edge_orientations >> 1) & 1) == 1,
                ((tbtbt.edge_orientations >> 2) & 1) == 1,
                ((tbtbt.edge_orientations >> 3) & 1) == 1,
                ((tbtbt.edge_orientations >> 4) & 1) == 1,
            ],
        }
    }
}

impl From<TwoByTwoByThreeInternal> for TwoByTwoByThree {
    fn from(tbtbti: TwoByTwoByThreeInternal) -> TwoByTwoByThree {
        TwoByTwoByThree {
            corner_positions: tbtbti.corner_positions[0] | tbtbti.corner_positions[1] << 3,
            corner_orientations_and_edge_positions: (tbtbti.corner_orientations[0][0] as u32)
                | (tbtbti.corner_orientations[0][1] as u32) << 2
                | (tbtbti.corner_orientations[0][2] as u32) << 4
                | (tbtbti.corner_orientations[1][0] as u32) << 6
                | (tbtbti.corner_orientations[1][1] as u32) << 8
                | (tbtbti.corner_orientations[1][2] as u32) << 10
                | (tbtbti.edge_positions[0] as u32) << 12
                | (tbtbti.edge_positions[1] as u32) << 16
                | (tbtbti.edge_positions[2] as u32) << 20
                | (tbtbti.edge_positions[3] as u32) << 24
                | (tbtbti.edge_positions[4] as u32) << 28,
            edge_orientations: tbtbti.edge_orientations[0] as u8
                | (tbtbti.edge_orientations[1] as u8) << 1
                | (tbtbti.edge_orientations[2] as u8) << 2
                | (tbtbti.edge_orientations[3] as u8) << 3
                | (tbtbti.edge_orientations[4] as u8) << 4,
        }
    }
}

use super::coord_cube::CoordCube;

impl From<CoordCube> for TwoByTwoByThreeInternal {
    fn from(coord_cube: CoordCube) -> TwoByTwoByThreeInternal {
        let mut corner_positions = [0; 2];
        let mut corner_orientations = [[0; 3]; 2];
        for i in 0..coord_cube.corners.len() {
            if coord_cube.corners[i].0 == 2 {
                corner_positions[0] = i as u8;
                corner_orientations[0] = coord_cube.corners[i].1;
            }
            if coord_cube.corners[i].0 == 3 {
                corner_positions[1] = i as u8;
                corner_orientations[1] = coord_cube.corners[i].1;
            }
        }

        let mut edge_positions = [0; 5];
        let mut edge_orientations = [false; 5];
        for i in 0..coord_cube.edges.len() {
            if coord_cube.edges[i].0 == 0 {
                edge_positions[0] = i as u8;
                edge_orientations[0] = coord_cube.edges[i].1;
            }
            if coord_cube.edges[i].0 == 2 {
                edge_positions[1] = i as u8;
                edge_orientations[1] = coord_cube.edges[i].1;
            }
            if coord_cube.edges[i].0 == 3 {
                edge_positions[2] = i as u8;
                edge_orientations[2] = coord_cube.edges[i].1;
            }
            if coord_cube.edges[i].0 == 6 {
                edge_positions[3] = i as u8;
                edge_orientations[3] = coord_cube.edges[i].1;
            }
            if coord_cube.edges[i].0 == 7 {
                edge_positions[4] = i as u8;
                edge_orientations[4] = coord_cube.edges[i].1;
            }
        }

        TwoByTwoByThreeInternal {
            corner_positions,
            corner_orientations,
            edge_positions,
            edge_orientations,
        }
    }
}

/* Note that this doesn't really generate a real CoordCube because we don't
 * have complete information about the corner and edge positions.  We just assume
 * that they are "as solved as possible."
 *
 * We leave all unspecified orientations solved so they don't have any extra
 * effect on the cubies we're interested in.
 */
impl From<TwoByTwoByThreeInternal> for CoordCube {
    fn from(tbtbti: TwoByTwoByThreeInternal) -> CoordCube {
        const EMPTY: u8 = 12;

        // try to leave any edges in their solved positions if possible
        let mut edges: [(u8, bool); 12] = [
            (EMPTY, false),
            (1, false),
            (EMPTY, false),
            (EMPTY, false),
            (4, false),
            (5, false),
            (EMPTY, false),
            (EMPTY, false),
            (8, false),
            (9, false),
            (10, false),
            (11, false),
        ];

        let original_indexes = [0, 2, 3, 6, 7];

        // place the 2x2x2 edges, since they're fully known
        // and if we displace another edge, record it
        let mut displaced = Vec::new();
        for i in 0..tbtbti.edge_positions.len() {
            if !original_indexes.contains(&tbtbti.edge_positions[i]) {
                displaced.push(tbtbti.edge_positions[i]);
            }

            edges[tbtbti.edge_positions[i] as usize] =
                (original_indexes[i], tbtbti.edge_orientations[i]);
        }

        // put displaced edges in any empty spot
        let mut i = 0;
        for x in displaced {
            loop {
                if edges[i].0 == EMPTY {
                    edges[i] = (x, false);
                    break;
                }
                i = i + 1;
            }
        }

        // try to leave any corners in their solved positions if possible
        let mut corners: [(u8, [u8; 3]); 8] = [
            (0, [0, 1, 2]),
            (1, [0, 1, 2]),
            (EMPTY, [0, 1, 2]),
            (EMPTY, [0, 1, 2]),
            (4, [0, 1, 2]),
            (5, [0, 1, 2]),
            (6, [0, 1, 2]),
            (7, [0, 1, 2]),
        ];

        let original_indexes = [2, 3];

        // place the 2x2x2 corners, since they're fully known
        // and if we displace another corner, record it
        let mut displaced = Vec::new();
        for i in 0..tbtbti.corner_positions.len() {
            if !original_indexes.contains(&tbtbti.corner_positions[i]) {
                displaced.push(tbtbti.corner_positions[i]);
            }

            corners[tbtbti.corner_positions[i] as usize] =
                (original_indexes[i], tbtbti.corner_orientations[i]);
        }

        // put displaced corners in any empty spot
        let mut i = 0;
        for x in displaced {
            loop {
                if corners[i].0 == EMPTY {
                    corners[i] = (x, [0, 1, 2]);
                    break;
                }
                i = i + 1;
            }
        }
        CoordCube {
            corners,
            edges,
        }
    }
}

impl From<CoordCube> for TwoByTwoByThree {
    fn from(coord_cube: CoordCube) -> TwoByTwoByThree {
        TwoByTwoByThreeInternal::from(coord_cube).into()
    }
}

/* Note that this doesn't really generate a real CoordCube because we don't
 * have complete information about the corner and edge positions.  We just assume
 * that they are "as solved as possible."
 */
impl From<TwoByTwoByThree> for CoordCube {
    fn from(tbtbti: TwoByTwoByThree) -> CoordCube {
        TwoByTwoByThreeInternal::from(tbtbti).into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;

    #[test]
    fn internal_and_public_should_round_trip() {
        let scramble = CoordCube::identity()
            .permute(QuarterTurn::R.into())
            .permute(QuarterTurn::F.into())
            .permute(QuarterTurn::R.into())
            .permute(QuarterTurn::B.into())
            .permute(QuarterTurn::D.into());
        let internal: TwoByTwoByThreeInternal = scramble.into();
        let public: TwoByTwoByThree = internal.into();
        let round_trip: TwoByTwoByThreeInternal = public.into();
        assert_eq!(internal, round_trip);
    }

    fn index_to_arr_8_2(index: u8) -> [u8; 2] {
        let mut arr = [
            index / 7,
            index % 7,
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

    fn index_to_arr_12_5(index: u32) -> [u8; 5] {
        let mut arr = [
            (index / 7920) as u8,
            (index / 720 % 11) as u8,
            (index / 72 % 10) as u8,
            (index / 8 % 9) as u8,
            (index % 8) as u8,
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

    use super::super::quickcheck::Gen;
    use super::super::rand::Rng;
    use super::super::rand::prelude::SliceRandom;
    impl super::super::quickcheck::Arbitrary for TwoByTwoByThreeInternal {
        fn arbitrary<G: Gen>(g: &mut G) -> TwoByTwoByThreeInternal {
            let edge_orientation_choices = [
                [0, 1, 2],
                [0, 2, 1],
                [1, 0, 2],
                [1, 2, 0],
                [2, 1, 0],
                [2, 0, 1],
            ];
            TwoByTwoByThreeInternal {
                corner_positions: index_to_arr_8_2(g.gen_range(0, 56)),
                corner_orientations: [
                    *g.choose(&edge_orientation_choices).unwrap(),
                    *g.choose(&edge_orientation_choices).unwrap(),
                ],
                edge_positions: index_to_arr_12_5(g.gen_range(0, 95040)),
                edge_orientations: [
                    g.gen(),
                    g.gen(),
                    g.gen(),
                    g.gen(),
                    g.gen(),
                ],
            }
        }
    }

    quickcheck! {
        fn internal_and_public_should_round_trip_quickcheck(tbtbti: TwoByTwoByThreeInternal) -> bool {
            let public: TwoByTwoByThree = tbtbti.into();
            let round_trip: TwoByTwoByThreeInternal = public.into();
            tbtbti == round_trip
        }
    }

    #[test]
    fn should_round_trip_if_its_been_converted_once() {
        let original_r = CoordCube::identity().permute(QuarterTurn::R.into());
        let converted_once: CoordCube = TwoByTwoByThree::from(original_r).into();
        let converted_twice: CoordCube = TwoByTwoByThree::from(converted_once).into();
        assert_eq!(converted_once, converted_twice);
    }

    quickcheck! {
        fn internal_should_round_trip_quickcheck(tbtbti: TwoByTwoByThreeInternal) -> bool {
        let coord_cube: CoordCube = tbtbti.into();
        let round_trip: TwoByTwoByThreeInternal = coord_cube.into();
        tbtbti == round_trip
        }
    }

    use super::super::move_sets::quarter_turns::QuarterTurn;
    use super::super::move_sets::face_turns::FaceTurn;
    use super::super::permutation_group::PermutationGroup;
    #[test]
    fn should_be_able_to_generate_one_turn_with_a_combination_of_the_other_five() {
        let id_seq = [
            QuarterTurn::R,
            QuarterTurn::L,
            QuarterTurn::F,
            QuarterTurn::F,
            QuarterTurn::B,
            QuarterTurn::B,
            QuarterTurn::RPrime,
            QuarterTurn::LPrime,
            QuarterTurn::U,
            QuarterTurn::R,
            QuarterTurn::L,
            QuarterTurn::FPrime,
            QuarterTurn::FPrime,
            QuarterTurn::BPrime,
            QuarterTurn::BPrime,
            QuarterTurn::RPrime,
            QuarterTurn::LPrime,
            QuarterTurn::D,
        ];
        let mut coord_cube = CoordCube::identity();
        for &t in &id_seq {
            coord_cube =
                CoordCube::from(TwoByTwoByThree::from(coord_cube.permute(t.into())));
        }
        assert_eq!(coord_cube, CoordCube::identity());
    }

    #[test]
    fn special_sequences_round_scramble_front() {
        let coord_seq = [
            FaceTurn::U,
            FaceTurn::F,
            FaceTurn::BPrime,
            FaceTurn::U2,
            FaceTurn::R2,
            FaceTurn::FPrime,
            FaceTurn::BPrime,
            FaceTurn::UPrime,
            FaceTurn::B2,
            FaceTurn::DPrime,
            FaceTurn::B2,
            FaceTurn::U,
            FaceTurn::B2,
            FaceTurn::U2,
            FaceTurn::L2,
            FaceTurn::UPrime,
            FaceTurn::F2,
        ];
        let tbtbt_seq = [
            FaceTurn::U2,
            FaceTurn::R,
            FaceTurn::D2,
            FaceTurn::RPrime,
            FaceTurn::B,
            FaceTurn::DPrime,
            FaceTurn::U2,
            FaceTurn::BPrime,
            FaceTurn::D,
            FaceTurn::BPrime,
            FaceTurn::D2,
            FaceTurn::BPrime,
            FaceTurn::D2,
            FaceTurn::F2,
        ];
        let mut coord_cube = CoordCube::identity();
        for &t in &coord_seq {
            coord_cube = coord_cube.permute(t.into());
        }
        let mut tbtbt_cube = CoordCube::identity();
        for &t in &tbtbt_seq {
            tbtbt_cube =
                CoordCube::from(TwoByTwoByThree::from(tbtbt_cube.permute(t.into())));
        }
        assert_eq!(coord_cube, tbtbt_cube);
    }

    #[bench]
    fn repeat_all_turn_identity_sequence(b: &mut Bencher) {
        let id_seq = [
            QuarterTurn::R,
            QuarterTurn::L,
            QuarterTurn::F,
            QuarterTurn::F,
            QuarterTurn::B,
            QuarterTurn::B,
            QuarterTurn::RPrime,
            QuarterTurn::LPrime,
            QuarterTurn::U,
            QuarterTurn::R,
            QuarterTurn::L,
            QuarterTurn::FPrime,
            QuarterTurn::FPrime,
            QuarterTurn::BPrime,
            QuarterTurn::BPrime,
            QuarterTurn::RPrime,
            QuarterTurn::LPrime,
            QuarterTurn::D,
        ];
        b.iter(|| {
            let mut coord_cube = CoordCube::identity();
            for &t in &id_seq {
                coord_cube = CoordCube::from(TwoByTwoByThree::from(coord_cube.permute(t.into())));
            }
        });
    }
}

