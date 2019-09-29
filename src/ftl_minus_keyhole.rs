/* Is a coordinate that holds two _almost_ fully solved layers.
 * In this representation, the top two layers are solved, except
 * for the corner edge pair with the URF corner and FR edge.
 * This is a useful state to acheive because it has many solved
 * pieces but still allows three face turns (F, R, and D) to be
 * used in ways that don't disturb the solve.
 *
 * Since this is symmetrical via mirroring(ish), we store a full
 * permutation of the edge facelets.
 */
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash, Serialize, Deserialize)]
pub struct FtlMinusKeyhole {
    corners_and_top_edge_orientations: u32,
    edge_positions_and_middle_edge_orientations: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
struct FtlMinusKeyholeInternal {
    corner_positions: [u8; 3], // 3x8 9 bits
    corner_orientations: [[u8; 3]; 3], // 3x(3x3) 18 bits
    edge_positions: [u8; 7], // 7x12 28 bits
    edge_orientations: [bool; 7], // 7 bits
}

impl From<FtlMinusKeyhole> for FtlMinusKeyholeInternal {
    fn from(fmk: FtlMinusKeyhole) -> FtlMinusKeyholeInternal {
        FtlMinusKeyholeInternal {
            corner_positions: [
                (fmk.corners_and_top_edge_orientations as u8) & 0b111,
                ((fmk.corners_and_top_edge_orientations >> 3) as u8) & 0b111,
                ((fmk.corners_and_top_edge_orientations >> 6) as u8) & 0b111,
            ],
            corner_orientations: [
                [
                    ((fmk.corners_and_top_edge_orientations >> 9) as u8) & 0b11,
                    ((fmk.corners_and_top_edge_orientations >> 11) as u8) & 0b11,
                    ((fmk.corners_and_top_edge_orientations >> 13) as u8) & 0b11,
                ],
                [
                    ((fmk.corners_and_top_edge_orientations >> 15) as u8) & 0b11,
                    ((fmk.corners_and_top_edge_orientations >> 17) as u8) & 0b11,
                    ((fmk.corners_and_top_edge_orientations >> 19) as u8) & 0b11,
                ],
                [
                    ((fmk.corners_and_top_edge_orientations >> 21) as u8) & 0b11,
                    ((fmk.corners_and_top_edge_orientations >> 23) as u8) & 0b11,
                    ((fmk.corners_and_top_edge_orientations >> 25) as u8) & 0b11,
                ],
            ],
            edge_positions: [
                (fmk.edge_positions_and_middle_edge_orientations as u8) & 0b1111,
                ((fmk.edge_positions_and_middle_edge_orientations >> 4) as u8) & 0b1111,
                ((fmk.edge_positions_and_middle_edge_orientations >> 8) as u8) & 0b1111,
                ((fmk.edge_positions_and_middle_edge_orientations >> 12) as u8) & 0b1111,
                ((fmk.edge_positions_and_middle_edge_orientations >> 16) as u8) & 0b1111,
                ((fmk.edge_positions_and_middle_edge_orientations >> 20) as u8) & 0b1111,
                ((fmk.edge_positions_and_middle_edge_orientations >> 24) as u8) & 0b1111,
            ],
            edge_orientations: [
                ((fmk.corners_and_top_edge_orientations >> 27) & 1) == 1,
                ((fmk.corners_and_top_edge_orientations >> 28) & 1) == 1,
                ((fmk.corners_and_top_edge_orientations >> 29) & 1) == 1,
                ((fmk.corners_and_top_edge_orientations >> 30) & 1) == 1,
                ((fmk.edge_positions_and_middle_edge_orientations >> 28) & 1) == 1,
                ((fmk.edge_positions_and_middle_edge_orientations >> 29) & 1) == 1,
                ((fmk.edge_positions_and_middle_edge_orientations >> 30) & 1) == 1,
            ],
        }
    }
}

impl From<FtlMinusKeyholeInternal> for FtlMinusKeyhole {
    fn from(fmki: FtlMinusKeyholeInternal) -> FtlMinusKeyhole {
        FtlMinusKeyhole {
            corners_and_top_edge_orientations: fmki.corner_positions[0] as u32
                | (fmki.corner_positions[1] as u32) << 3
                | (fmki.corner_positions[2] as u32) << 6
                | (fmki.corner_orientations[0][0] as u32) << 9
                | (fmki.corner_orientations[0][1] as u32) << 11
                | (fmki.corner_orientations[0][2] as u32) << 13
                | (fmki.corner_orientations[1][0] as u32) << 15
                | (fmki.corner_orientations[1][1] as u32) << 17
                | (fmki.corner_orientations[1][2] as u32) << 19
                | (fmki.corner_orientations[2][0] as u32) << 21
                | (fmki.corner_orientations[2][1] as u32) << 23
                | (fmki.corner_orientations[2][2] as u32) << 25
                | (fmki.edge_orientations[0] as u32) << 27
                | (fmki.edge_orientations[1] as u32) << 28
                | (fmki.edge_orientations[2] as u32) << 29
                | (fmki.edge_orientations[3] as u32) << 30,
            edge_positions_and_middle_edge_orientations: fmki.edge_positions[0] as u32
                | (fmki.edge_positions[1] as u32) << 4
                | (fmki.edge_positions[2] as u32) << 8
                | (fmki.edge_positions[3] as u32) << 12
                | (fmki.edge_positions[4] as u32) << 16
                | (fmki.edge_positions[5] as u32) << 20
                | (fmki.edge_positions[6] as u32) << 24
                | (fmki.edge_orientations[4] as u32) << 28
                | (fmki.edge_orientations[5] as u32) << 29
                | (fmki.edge_orientations[6] as u32) << 30
        }
    }
}

use super::coord_cube::CoordCube;

impl From<CoordCube> for FtlMinusKeyholeInternal {
    fn from(coord_cube: CoordCube) -> FtlMinusKeyholeInternal {
        let mut corner_positions = [0; 3];
        let mut corner_orientations = [[0; 3]; 3];
        for i in 0..coord_cube.corners.len() {
            if coord_cube.corners[i].0 == 0 {
                corner_positions[0] = i as u8;
                corner_orientations[0] = coord_cube.corners[i].1;
            }
            if coord_cube.corners[i].0 == 1 {
                corner_positions[1] = i as u8;
                corner_orientations[1] = coord_cube.corners[i].1;
            }
            if coord_cube.corners[i].0 == 2 {
                corner_positions[2] = i as u8;
                corner_orientations[2] = coord_cube.corners[i].1;
            }
        }

        let mut edge_positions = [0; 7];
        let mut edge_orientations = [false; 7];
        for i in 0..coord_cube.edges.len() {
            if coord_cube.edges[i].0 == 0 {
                edge_positions[0] = i as u8;
                edge_orientations[0] = coord_cube.edges[i].1;
            }
            if coord_cube.edges[i].0 == 1 {
                edge_positions[1] = i as u8;
                edge_orientations[1] = coord_cube.edges[i].1;
            }
            if coord_cube.edges[i].0 == 2 {
                edge_positions[2] = i as u8;
                edge_orientations[2] = coord_cube.edges[i].1;
            }
            if coord_cube.edges[i].0 == 3 {
                edge_positions[3] = i as u8;
                edge_orientations[3] = coord_cube.edges[i].1;
            }
            if coord_cube.edges[i].0 == 4 {
                edge_positions[4] = i as u8;
                edge_orientations[4] = coord_cube.edges[i].1;
            }
            if coord_cube.edges[i].0 == 5 {
                edge_positions[5] = i as u8;
                edge_orientations[5] = coord_cube.edges[i].1;
            }
            if coord_cube.edges[i].0 == 6 {
                edge_positions[6] = i as u8;
                edge_orientations[6] = coord_cube.edges[i].1;
            }
        }

        FtlMinusKeyholeInternal {
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
impl From<FtlMinusKeyholeInternal> for CoordCube {
    fn from(fmki: FtlMinusKeyholeInternal) -> CoordCube {
        const EMPTY: u8 = 12;

        // try to leave any edges in their solved positions if possible
        let mut edges: [(u8, bool); 12] = [
            (EMPTY, false),
            (EMPTY, false),
            (EMPTY, false),
            (EMPTY, false),
            (EMPTY, false),
            (EMPTY, false),
            (EMPTY, false),
            (7, false),
            (8, false),
            (9, false),
            (10, false),
            (11, false),
        ];

        // place the 2x2x2 edges, since they're fully known
        // and if we displace another edge, record it
        let mut displaced = Vec::new();
        for i in 0..fmki.edge_positions.len() {
            if fmki.edge_positions[i] > 6 {
                displaced.push(fmki.edge_positions[i]);
            }

            edges[fmki.edge_positions[i] as usize] =
                (i as u8, fmki.edge_orientations[i]);
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
            (EMPTY, [0, 1, 2]),
            (EMPTY, [0, 1, 2]),
            (EMPTY, [0, 1, 2]),
            (3, [0, 1, 2]),
            (4, [0, 1, 2]),
            (5, [0, 1, 2]),
            (6, [0, 1, 2]),
            (7, [0, 1, 2]),
        ];

        // place the 2x2x2 corners, since they're fully known
        // and if we displace another corner, record it
        let mut displaced = Vec::new();
        for i in 0..fmki.corner_positions.len() {
            if fmki.corner_positions[i] > 2 {
                displaced.push(fmki.corner_positions[i]);
            }

            corners[fmki.corner_positions[i] as usize] =
                (i as u8, fmki.corner_orientations[i]);
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

impl From<CoordCube> for FtlMinusKeyhole {
    fn from(coord_cube: CoordCube) -> FtlMinusKeyhole {
        FtlMinusKeyholeInternal::from(coord_cube).into()
    }
}

/* Note that this doesn't really generate a real CoordCube because we don't
 * have complete information about the corner and edge positions.  We just assume
 * that they are "as solved as possible."
 */
impl From<FtlMinusKeyhole> for CoordCube {
    fn from(fmki: FtlMinusKeyhole) -> CoordCube {
        FtlMinusKeyholeInternal::from(fmki).into()
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
        let internal: FtlMinusKeyholeInternal = scramble.into();
        let public: FtlMinusKeyhole = internal.into();
        let round_trip: FtlMinusKeyholeInternal = public.into();
        assert_eq!(internal, round_trip);
    }

    fn index_to_arr_8_3(index: u16) -> [u8; 3] {
        let mut arr = [
            (index / 42) as u8,
            (index / 6 % 7) as u8,
            (index % 6) as u8,
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

    fn index_to_arr_12_7(index: u32) -> [u8; 7] {
        let mut arr = [
            (index / 332640) as u8,
            (index / 30240 % 11) as u8,
            (index / 3024 % 10) as u8,
            (index / 336 % 9) as u8,
            (index / 42 % 8) as u8,
            (index / 6 % 7) as u8,
            (index % 6) as u8,
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
    impl super::super::quickcheck::Arbitrary for FtlMinusKeyholeInternal {
        fn arbitrary<G: Gen>(g: &mut G) -> FtlMinusKeyholeInternal {
            let edge_orientation_choices = [
                [0, 1, 2],
                [0, 2, 1],
                [1, 0, 2],
                [1, 2, 0],
                [2, 1, 0],
                [2, 0, 1],
            ];
            FtlMinusKeyholeInternal {
                corner_positions: index_to_arr_8_3(g.gen_range(0, 336)),
                corner_orientations: [
                    *g.choose(&edge_orientation_choices).unwrap(),
                    *g.choose(&edge_orientation_choices).unwrap(),
                    *g.choose(&edge_orientation_choices).unwrap(),
                ],
                edge_positions: index_to_arr_12_7(g.gen_range(0, 3991680)),
                edge_orientations: [
                    g.gen(),
                    g.gen(),
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
        fn internal_and_public_should_round_trip_quickcheck(fmki: FtlMinusKeyholeInternal) -> bool {
            let public: FtlMinusKeyhole = fmki.into();
            let round_trip: FtlMinusKeyholeInternal = public.into();
            fmki == round_trip
        }
    }

    #[test]
    fn should_round_trip_if_its_been_converted_once() {
        let original_r = CoordCube::identity().permute(QuarterTurn::R.into());
        let converted_once: CoordCube = FtlMinusKeyhole::from(original_r).into();
        let converted_twice: CoordCube = FtlMinusKeyhole::from(converted_once).into();
        assert_eq!(converted_once, converted_twice);
    }

    quickcheck! {
        fn internal_should_round_trip_quickcheck(fmki: FtlMinusKeyholeInternal) -> bool {
        let coord_cube: CoordCube = fmki.into();
        let round_trip: FtlMinusKeyholeInternal = coord_cube.into();
        fmki == round_trip
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
                CoordCube::from(FtlMinusKeyhole::from(coord_cube.permute(t.into())));
        }
        assert_eq!(coord_cube, CoordCube::identity());
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
                coord_cube = CoordCube::from(FtlMinusKeyhole::from(coord_cube.permute(t.into())));
            }
        });
    }
}

