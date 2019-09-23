/* Is a coordinate that holds a solved corner and its
 * adjacent edges, making a cube that is 2x2x2 in facelets.
 *
 * Since this is not symmetrical via mirroring, we store
 * a basic orientation representation.
 */
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash, Serialize, Deserialize)]
pub struct TwoByTwoByTwo {
    corner: u8,
    edges: u16,
}

impl TwoByTwoByTwo {
    pub fn is_solved(&self) -> bool {
        // TODO: Double check this
        self.corner == 3 && self.edges == 1840
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
struct TwoByTwoByTwoInternal {
    corner_position: u8,
    corner_orientation: u8,
    edge_positions: [u8; 3],
    edge_orientations: [bool; 3],
}

impl From<TwoByTwoByTwo> for TwoByTwoByTwoInternal {
    fn from(tbtbt: TwoByTwoByTwo) -> TwoByTwoByTwoInternal {
        TwoByTwoByTwoInternal {
            corner_position: tbtbt.corner & 0b111,
            corner_orientation: tbtbt.corner >> 3,
            edge_positions: [
                (tbtbt.edges as u8) & 0b1111,
                ((tbtbt.edges >> 4) as u8) & 0b1111,
                ((tbtbt.edges >> 8) as u8) & 0b1111,
            ],
            edge_orientations: [
                ((tbtbt.edges >> 12) & 1) == 1,
                ((tbtbt.edges >> 13) & 1) == 1,
                ((tbtbt.edges >> 14) & 1) == 1,
            ],
        }
    }
}

impl From<TwoByTwoByTwoInternal> for TwoByTwoByTwo {
    fn from(tbtbti: TwoByTwoByTwoInternal) -> TwoByTwoByTwo {
        TwoByTwoByTwo {
            corner: tbtbti.corner_position | (tbtbti.corner_orientation << 3),
            edges: (tbtbti.edge_positions[0] as u16) |
                ((tbtbti.edge_positions[1] as u16) << 4) |
                ((tbtbti.edge_positions[2] as u16) << 8) |
                ((tbtbti.edge_orientations[0] as u16) << 12) |
                ((tbtbti.edge_orientations[1] as u16) << 13) |
                ((tbtbti.edge_orientations[2] as u16) << 14),
        }
    }
}

fn complex_to_simple_corner_orientation(x: [u8; 3]) -> u8 {
    match x {
        [0, 1, 2] => 0,
        [2, 0, 1] => 1,
        [1, 2, 0] => 2,
        _ => panic!("TODO: This should be a try_into and it failed!"),
    }
}

use super::coord_cube::CoordCube;

impl From<CoordCube> for TwoByTwoByTwoInternal {
    fn from(coord_cube: CoordCube) -> TwoByTwoByTwoInternal {
        let mut edge_positions = [0; 3];
        let mut edge_orientations = [false; 3];
        for i in 0..coord_cube.edges.len() {
            if coord_cube.edges[i].0 == 0 {
                edge_positions[0] = i as u8;
                edge_orientations[0] = coord_cube.edges[i].1;
            }
            if coord_cube.edges[i].0 == 3 {
                edge_positions[1] = i as u8;
                edge_orientations[1] = coord_cube.edges[i].1;
            }
            if coord_cube.edges[i].0 == 7 {
                edge_positions[2] = i as u8;
                edge_orientations[2] = coord_cube.edges[i].1;
            }
        }

        let mut corner_position = 0;
        let mut corner_orientation = 0;
        for i in 0..coord_cube.corners.len() {
            if coord_cube.corners[i].0 == 3 {
                corner_position = i as u8;
                corner_orientation =
                    complex_to_simple_corner_orientation(coord_cube.corners[i].1);
            }
        }

        TwoByTwoByTwoInternal {
            corner_position,
            corner_orientation,
            edge_positions,
            edge_orientations,
        }
    }
}

// TODO: Enum?
fn simple_to_complex_corner_orientation(x: u8) -> [u8; 3] {
    match x {
        0 => [0, 1, 2],
        1 => [2, 0, 1],
        2 => [1, 2, 0],
        _ => panic!("Invalid corner orientation!"),
    }
}

/* Note that this doesn't really generate a real CoordCube because we don't
 * have complete information about the corner and edge positions.  We just assume
 * that they are "as solved as possible."
 *
 * We leave all unspecified orientations solved so they don't have any extra
 * effect on the cubies we're interested in.
 */
impl From<TwoByTwoByTwoInternal> for CoordCube {
    fn from(tbtbti: TwoByTwoByTwoInternal) -> CoordCube {
        const EMPTY: u8 = 12;

        // try to leave any edges in their solved positions if possible
        let mut edges: [(u8, bool); 12] = [
            (EMPTY, false),
            (1, false),
            (2, false),
            (EMPTY, false),
            (4, false),
            (5, false),
            (6, false),
            (EMPTY, false),
            (8, false),
            (9, false),
            (10, false),
            (11, false),
        ];

        // place the 2x2x2 edges, since they're fully known
        // and if we displace another edge, record it
        let mut displaced = Vec::new();
        for i in 0..tbtbti.edge_positions.len() {
            if tbtbti.edge_positions[i] != 0 && tbtbti.edge_positions[i] != 3 && tbtbti.edge_positions[i] != 7 {
                displaced.push(tbtbti.edge_positions[i]);
            }

            let original_index = if i == 0 {
                0
            } else if i == 1 {
                3
            } else if i == 2 {
                7
            } else {
                panic!("Went outside of the possible 2x2x2 edge indexes!")
            };
            edges[tbtbti.edge_positions[i] as usize] =
                (original_index, tbtbti.edge_orientations[i]);
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

        // Since we only have one corner, we can either insert its
        // orientation if the position is solved, or we can swap it
        // with the other corner.
        let mut corners: [(u8, [u8; 3]); 8] = [
            (0, [0, 1, 2]),
            (1, [0, 1, 2]),
            (2, [0, 1, 2]),
            (3, [0, 1, 2]),
            (4, [0, 1, 2]),
            (5, [0, 1, 2]),
            (6, [0, 1, 2]),
            (7, [0, 1, 2]),
        ];
        if tbtbti.corner_position == 3 {
            corners[3] = (
                3,
                simple_to_complex_corner_orientation(tbtbti.corner_orientation)
            );
        } else {
            corners[tbtbti.corner_position as usize] = (
                3,
                simple_to_complex_corner_orientation(tbtbti.corner_orientation)
            );
            corners[3] = (tbtbti.corner_position, [0, 1, 2]);
        }

        CoordCube {
            corners,
            edges,
        }
    }
}

impl From<CoordCube> for TwoByTwoByTwo {
    fn from(coord_cube: CoordCube) -> TwoByTwoByTwo {
        TwoByTwoByTwoInternal::from(coord_cube).into()
    }
}

/* Note that this doesn't really generate a real CoordCube because we don't
 * have complete information about the corner and edge positions.  We just assume
 * that they are "as solved as possible."
 */
impl From<TwoByTwoByTwo> for CoordCube {
    fn from(tbtbti: TwoByTwoByTwo) -> CoordCube {
        TwoByTwoByTwoInternal::from(tbtbti).into()
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
        let internal: TwoByTwoByTwoInternal = scramble.into();
        let public: TwoByTwoByTwo = internal.into();
        let round_trip: TwoByTwoByTwoInternal = public.into();
        assert_eq!(internal, round_trip);
    }

    #[test]
    fn should_round_trip_if_its_been_converted_once() {
        let original_r = CoordCube::identity().permute(QuarterTurn::R.into());
        let converted_once: CoordCube = TwoByTwoByTwo::from(original_r).into();
        let converted_twice: CoordCube = TwoByTwoByTwo::from(converted_once).into();
        assert_eq!(converted_once, converted_twice);
    }

    use super::super::move_sets::quarter_turns::QuarterTurn;
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
                CoordCube::from(TwoByTwoByTwo::from(coord_cube.permute(t.into())));
        }
        assert_eq!(coord_cube, CoordCube::identity());
    }

    #[test]
    fn special_sequences_round_trip_rotating_three_edges() {
        let coord_seq = [
            QuarterTurn::R,
            QuarterTurn::R,
            QuarterTurn::D,
            QuarterTurn::B,
            QuarterTurn::B,
            QuarterTurn::L,
            QuarterTurn::L,
            QuarterTurn::D,
            QuarterTurn::D,
            QuarterTurn::R,
            QuarterTurn::R,
            QuarterTurn::F,
            QuarterTurn::F,
            QuarterTurn::U,
            QuarterTurn::U,
            QuarterTurn::R,
            QuarterTurn::R,
            QuarterTurn::DPrime,
            QuarterTurn::B,
            QuarterTurn::L,
            QuarterTurn::L,
            QuarterTurn::D,
            QuarterTurn::D,
            QuarterTurn::R,
            QuarterTurn::R,
            QuarterTurn::F,
            QuarterTurn::U,
            QuarterTurn::U,
        ];
        let tbtbt_seq = [
            QuarterTurn::FPrime,
            QuarterTurn::U,
            QuarterTurn::F,
            QuarterTurn::UPrime,
            QuarterTurn::LPrime,
            QuarterTurn::BPrime,
            QuarterTurn::BPrime,
            QuarterTurn::D,
            QuarterTurn::B,
            QuarterTurn::UPrime,
            QuarterTurn::BPrime,
            QuarterTurn::DPrime,
            QuarterTurn::B,
            QuarterTurn::U,
        ];
        let mut coord_cube = CoordCube::identity();
        for &t in &coord_seq {
            coord_cube = coord_cube.permute(t.into());
        }
        let mut tbtbt_cube = CoordCube::identity();
        for &t in &tbtbt_seq {
            tbtbt_cube =
                CoordCube::from(TwoByTwoByTwo::from(tbtbt_cube.permute(t.into())));
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
                coord_cube = CoordCube::from(TwoByTwoByTwo::from(coord_cube.permute(t.into())));
            }
        });
    }
}

