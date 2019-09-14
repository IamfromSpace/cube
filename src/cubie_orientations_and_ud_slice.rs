/* Is a coordinate that maps just the corner and edge orientations
 * and the position of the UD slice (RF, BR, BL, and LF edges).
 * This is notably NOT a permutation and NOT invertable because
 * it doesn't hold enough information about the other piece
 * positions.
 *
 * Corner orientation support mirroring, so they are a full
 * permutation of the three facelets (stickers).
 */
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct CubieOrientationAndUDSlice {
    corner_orientations: [u8; 3],
    edge_orientations: u16,
    ud_slice: u16,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
struct CubieOrientationAndUDSliceInternal {
    corner_orientations: [[u8; 3]; 8],
    edge_orientations: [bool; 12],
    ud_slice: [u8; 4],
}

fn index_four_edges(index: u16) -> [u8; 4] {
    [
        ((index & 0b1111)) as u8,
        ((index >> 4) & 0b1111) as u8,
        ((index >> 8) & 0b1111) as u8,
        (index >> 12) as u8,
    ]
}

// TODO: This is already defined in CoordCube
fn index_to_arr_3(index: u8) -> [u8; 3] {
    let mut arr = [
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

fn index_to_edge_orientations(index: u16) -> [bool; 12] {
    // This looks dumb, but is quite a bit faster
    [
        0b1 & index == 0b1,
        0b10 & index == 0b10,
        0b100 & index == 0b100,
        0b1000 & index == 0b1000,
        0b10000 & index == 0b10000,
        0b100000 & index == 0b100000,
        0b1000000 & index == 0b1000000,
        0b10000000 & index == 0b10000000,
        0b100000000 & index == 0b100000000,
        0b1000000000 & index == 0b1000000000,
        0b10000000000 & index == 0b10000000000,
        0b100000000000 & index == 0b100000000000,
    ]
}

impl From<CubieOrientationAndUDSlice> for CubieOrientationAndUDSliceInternal {
    fn from(coauds: CubieOrientationAndUDSlice) -> CubieOrientationAndUDSliceInternal {
        let x = coauds.corner_orientations;
        let x = x[0] as u32 | ((x[1] as u32) << 8) | ((x[2] as u32) << 16);
        CubieOrientationAndUDSliceInternal {
            corner_orientations: [
                index_to_arr_3(0b111 & x as u8),
                index_to_arr_3(0b111 & (x >> 3) as u8),
                index_to_arr_3(0b111 & (x >> 6) as u8),
                index_to_arr_3(0b111 & (x >> 9) as u8),
                index_to_arr_3(0b111 & (x >> 12) as u8),
                index_to_arr_3(0b111 & (x >> 15) as u8),
                index_to_arr_3(0b111 & (x >> 18) as u8),
                index_to_arr_3(0b111 & (x >> 21) as u8),
            ],
            edge_orientations: index_to_edge_orientations(coauds.edge_orientations),
            ud_slice: index_four_edges(coauds.ud_slice),
        }
    }
}

fn four_edges_from_index(arr: [u8; 4]) -> u16 {
    (arr[3] as u16) * 4096 + (arr[2] as u16) * 256 + (arr[1] as u16) * 16 + (arr[0] as u16)
}

fn arr_to_index_3(arr_raw: [u8; 3]) -> u8 {
    let mut arr = arr_raw.clone();
    for i in (1..arr.len() - 1).rev() {
        for j in 0..i {
            if arr[j] < arr_raw[i] {
                arr[i] = arr[i] - 1;
            }
        }
    }
    arr[0] * 2 + arr[1]
}

fn edge_orientations_to_index(arr: [bool; 12]) -> u16 {
    let mut x = 0;
    for i in 0..arr.len() {
        x = x | (if arr[i] { 1 << i } else { 0 })
    }
    x
}

impl From<CubieOrientationAndUDSliceInternal> for CubieOrientationAndUDSlice {
    fn from(coaudsd: CubieOrientationAndUDSliceInternal) -> CubieOrientationAndUDSlice {
        let mut x: u32 = 0;

        for i in 0..coaudsd.corner_orientations.len() {
            x = x | ((arr_to_index_3(coaudsd.corner_orientations[i]) as u32) << (i * 3));
        }

        CubieOrientationAndUDSlice {
            corner_orientations: [
                (x & 0b11111111) as u8,
                ((x >> 8) & 0b11111111) as u8,
                ((x >> 16) & 0b11111111) as u8,
            ],
            edge_orientations: edge_orientations_to_index(coaudsd.edge_orientations),
            ud_slice: four_edges_from_index(coaudsd.ud_slice),
        }
    }
}

use super::coord_cube::CoordCube;

impl From<CoordCube> for CubieOrientationAndUDSliceInternal {
    fn from(coord_cube: CoordCube) -> CubieOrientationAndUDSliceInternal {
        let mut corner_orientations = [[0; 3]; 8];
        for i in 0..corner_orientations.len() {
            corner_orientations[i] = coord_cube.corners[i].1;
        }

        // Our array of edge perms starts at 0 here, but the middle slice
        // starts at 4 in the CoordCube
        let mut ud_slice = [0; 4];
        for i in 0..coord_cube.edges.len() {
            if coord_cube.edges[i].0 == 4 {
                ud_slice[0] = i as u8;
            }
            if coord_cube.edges[i].0 == 5 {
                ud_slice[1] = i as u8;
            }
            if coord_cube.edges[i].0 == 6 {
                ud_slice[2] = i as u8;
            }
            if coord_cube.edges[i].0 == 7 {
                ud_slice[3] = i as u8;
            }
        }

        let mut edge_orientations = [false; 12];
        for i in 0..edge_orientations.len() {
            edge_orientations[i] = coord_cube.edges[i].1;
        }

        CubieOrientationAndUDSliceInternal {
            corner_orientations,
            edge_orientations,
            ud_slice,
        }
    }
}

/* Note that this doesn't really generate a real CoordCube because we don't
 * have complete information about the corner and edge positions.  We just assume
 * that they are "as solved as possible."
 */
impl From<CubieOrientationAndUDSliceInternal> for CoordCube {
    fn from(coaudsd: CubieOrientationAndUDSliceInternal) -> CoordCube {
        let corners = [
            (0, coaudsd.corner_orientations[0]),
            (1, coaudsd.corner_orientations[1]),
            (2, coaudsd.corner_orientations[2]),
            (3, coaudsd.corner_orientations[3]),
            (4, coaudsd.corner_orientations[4]),
            (5, coaudsd.corner_orientations[5]),
            (6, coaudsd.corner_orientations[6]),
            (7, coaudsd.corner_orientations[7]),
        ];
        let mut edges_p: [u8; 12] = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11];
        for i in 0..coaudsd.ud_slice.len() {
            let tmp = edges_p[coaudsd.ud_slice[i] as usize];
            // Our array of edge perms starts at 0 here, but the middle slice
            // starts at 4 in the CoordCube
            edges_p[coaudsd.ud_slice[i] as usize] = i as u8 + 4;
            edges_p[i as usize + 4] = tmp;
        }

        let mut edges = [(0,false); 12];
        for i in 0..edges.len() {
            edges[i] = (edges_p[i], coaudsd.edge_orientations[i]);
        }

        CoordCube {
            corners,
            edges,
        }
    }
}

impl From<CoordCube> for CubieOrientationAndUDSlice {
    fn from(coord_cube: CoordCube) -> CubieOrientationAndUDSlice {
        CubieOrientationAndUDSliceInternal::from(coord_cube).into()
    }
}

/* Note that this doesn't really generate a real CoordCube because we don't
 * have complete information about the corner and edge positions.  We just assume
 * that they are "as solved as possible."
 */
impl From<CubieOrientationAndUDSlice> for CoordCube {
    fn from(coaudsd: CubieOrientationAndUDSlice) -> CoordCube {
        CubieOrientationAndUDSliceInternal::from(coaudsd).into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;

    #[test]
    // TODO: This may be elsewhere
    fn arr_to_index_3_should_round_trip() {
        let cases = [
            [0, 1, 2],
            [0, 2, 1],
            [1, 0, 2],
            [1, 2, 0],
            [2, 1, 0],
            [2, 0, 1],
        ];
        for &case in &cases {
            assert_eq!(case, index_to_arr_3(arr_to_index_3(case)));
        }
    }

    #[test]
    fn internal_and_public_should_round_trip() {
        let scramble = CoordCube::identity()
            .permute(QuarterTurn::R.into())
            .permute(QuarterTurn::F.into())
            .permute(QuarterTurn::R.into())
            .permute(QuarterTurn::B.into())
            .permute(QuarterTurn::D.into());
        let internal: CubieOrientationAndUDSliceInternal = scramble.into();
        let public: CubieOrientationAndUDSlice = internal.into();
        println!("{:?}", public);
        let round_trip: CubieOrientationAndUDSliceInternal = public.into();
        assert_eq!(internal, round_trip);
    }

    #[test]
    fn should_round_trip_if_its_been_converted_once() {
        let original_r = CoordCube::identity().permute(QuarterTurn::R.into());
        let converted_once: CoordCube = CubieOrientationAndUDSlice::from(original_r).into();
        let converted_twice: CoordCube = CubieOrientationAndUDSlice::from(converted_once).into();
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
                CoordCube::from(CubieOrientationAndUDSlice::from(coord_cube.permute(t.into())));
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
                coord_cube = CoordCube::from(CubieOrientationAndUDSlice::from(coord_cube.permute(t.into())));
            }
        });
    }

    #[bench]
    fn edge_orientation_packing_x20(b: &mut Bencher) {
        let cases: Vec<u16> = vec!(
            3263,
            477,
            2089,
            1057,
            341,
            3979,
            3053,
            3709,
            2680,
            2789,
            1629,
            1810,
            2530,
            1131,
            3429,
            1689,
            37,
            3301,
            2315,
            972,
        );

        b.iter(|| {
            let mut eos = Vec::new();
            for &case in &cases {
                eos.push(index_to_edge_orientations(case));
            }
            for &eo in &eos {
                edge_orientations_to_index(eo);
            }
        });
    }
}

