extern crate functional;

use std::fmt;
use super::permutation_group::PermutationGroup;
use super::invertable::Invertable;

/* This representation of the cube talks in "cubies" and their orientations
 * and can represent all reachable states.
 *
 * Orientation is a bit tricky to define, but ultimately it asks, is this
 * cubie in a G1 position?  If not, what would it take to get there.
 *
 * Corners have three rotations, so the number of clockwise rotations
 * is stored (0, 1, or 2).
 *
 * To find the rotation of a corner, move it to the URF position using only
 * moves from G1 (U, F2, R2, B2, L2, D).  Count how many times the U or D
 * color has rotate clockwise from the U face.
 *
 * Edges can only be flipped so they are represented by a bool.
 *
 * Edges are flipped if either:
 *   The edge has a U/D color and it is on the U, D, F, or B face.
 *   The edge does not have a U/D color and the F/B color is showing on the U, D, F, or B face.
 *
 * Essentially we ask, if the cubie were moved to its correct position
 * via G2 (U, F2, R, B2, L, D), is flipped in place (vs solved)?
 *
 * For indexing, a unit circle is used as a guide (0, pi/2, pi, ...) and we start
 * at the top face.  If we overlay a unit circle on the top face, we see that
 * the UR edge falls on the zero value, so it gets the first index.
 * Then UB and so on.
 *
 * We can do the same for the corners, even though they don't land as cleanly.
 * The URB corner falls on the lowest value of pi/4, so it gets the first index,
 * then UBL and so on.
 *
 * For edges, we then consider the middle layer, but continue to "look" from
 * U to D.  In this case the RB edge is next, so it gets index 4.
 *
 * Finally, to handle our D face, we rotate the cube 180 degrees through the L-R axis
 * (the axis that would point to 0 on the unit circle) and then repeat the same process.
 * So DB gets index 8 and DF gets 9.  DFR gets 8 and so on.
 */
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct CoordCube {
    // TODO: These lists should probably be separate, because certain coordinates
    // completely ignore half pieces of data, and being able to simply copy one
    // piece would be better than having to map over them.
    pub corners: [(u8, [u8; 3]); 8],
    pub edges: [(u8, bool); 12],
}

impl functional::BinaryOperation<CoordCube> for CoordCube {
    fn apply(a: CoordCube, b: CoordCube) -> CoordCube {
        CoordCube {
            corners: permute_corners(&a.corners, &b.corners),
            edges: permute_edges(&a.edges, &b.edges),
        }
    }
}

impl functional::AssociativeOperation<CoordCube> for CoordCube { }

impl functional::Monoid<CoordCube> for CoordCube {
    fn one() -> CoordCube {
        CoordCube {
            corners: corner_identity(),
            edges: edge_identity(),
        }
    }
}

impl Invertable for CoordCube {
    fn invert(&self) -> CoordCube {
        CoordCube {
            corners: corner_inv(&self.corners),
            edges: edge_inv(&self.edges),
        }
    }
}

impl PermutationGroup for CoordCube {}

const CORNER_IDENTITY: [(u8,[u8; 3]); 8] =
    [(0, [0, 1, 2]), (1, [0, 1, 2]), (2, [0, 1, 2]), (3, [0, 1, 2]), (4, [0, 1, 2]), (5, [0, 1, 2]), (6, [0, 1, 2]), (7, [0, 1, 2])];

const EDGE_IDENTITY: [(u8,bool); 12] =
    [
        (0, false),
        (1, false),
        (2, false),
        (3, false),
        (4, false),
        (5, false),
        (6, false),
        (7, false),
        (8, false),
        (9, false),
        (10, false),
        (11, false),
    ];

const U: CoordCube = CoordCube {
    corners: [(1, [0, 1, 2]), (2, [0, 1, 2]), (3, [0, 1, 2]), (0, [0, 1, 2]), (4, [0, 1, 2]), (5, [0, 1, 2]), (6, [0, 1, 2]), (7, [0, 1, 2])],
    edges: [
        (1, false),
        (2, false),
        (3, false),
        (0, false),
        (4, false),
        (5, false),
        (6, false),
        (7, false),
        (8, false),
        (9, false),
        (10, false),
        (11, false),
    ],
};

const U_PRIME: CoordCube = CoordCube {
    corners: [(3, [0, 1, 2]), (0, [0, 1, 2]), (1, [0, 1, 2]), (2, [0, 1, 2]), (4, [0, 1, 2]), (5, [0, 1, 2]), (6, [0, 1, 2]), (7, [0, 1, 2])],
    edges: [
        (3, false),
        (0, false),
        (1, false),
        (2, false),
        (4, false),
        (5, false),
        (6, false),
        (7, false),
        (8, false),
        (9, false),
        (10, false),
        (11, false),
    ],
};

const F: CoordCube = CoordCube {
    corners: [(0, [0, 1, 2]), (1, [0, 1, 2]), (5, [1, 2, 0]), (2, [2, 0, 1]), (3, [1, 2, 0]), (4, [2, 0, 1]), (6, [0, 1, 2]), (7, [0, 1, 2])],
    edges: [
        (0, false),
        (1, false),
        (2, false),
        (6, true),
        (4, false),
        (5, false),
        (9, true),
        (3, true),
        (8, false),
        (7, true),
        (10, false),
        (11, false),
    ],
};

const F_PRIME: CoordCube = CoordCube {
    corners: [(0, [0, 1, 2]), (1, [0, 1, 2]), (3, [1, 2, 0]), (4, [2, 0, 1]), (5, [1, 2, 0]), (2, [2, 0, 1]), (6, [0, 1, 2]), (7, [0, 1, 2])],
    edges: [
        (0, false),
        (1, false),
        (2, false),
        (7, true),
        (4, false),
        (5, false),
        (3, true),
        (9, true),
        (8, false),
        (6, true),
        (10, false),
        (11, false),
    ],
};

const R: CoordCube = CoordCube {
    corners: [(3, [2, 0, 1]), (1, [0, 1, 2]), (2, [0, 1, 2]), (4, [1, 2, 0]), (7, [2, 0, 1]), (5, [0, 1, 2]), (6, [0, 1, 2]), (0, [1, 2, 0])],
    edges: [
        (7, false),
        (1, false),
        (2, false),
        (3, false),
        (0, false),
        (5, false),
        (6, false),
        (8, false),
        (4, false),
        (9, false),
        (10, false),
        (11, false),
    ],
};

const R_PRIME: CoordCube = CoordCube {
    corners: [(7, [2, 0, 1]), (1, [0, 1, 2]), (2, [0, 1, 2]), (0, [1, 2, 0]), (3, [2, 0, 1]), (5, [0, 1, 2]), (6, [0, 1, 2]), (4, [1, 2, 0])],
    edges: [
        (4, false),
        (1, false),
        (2, false),
        (3, false),
        (8, false),
        (5, false),
        (6, false),
        (0, false),
        (7, false),
        (9, false),
        (10, false),
        (11, false),
    ],
};

const B: CoordCube = CoordCube {
    corners: [(7, [1, 2, 0]), (0, [2, 0, 1]), (2, [0, 1, 2]), (3, [0, 1, 2]), (4, [0, 1, 2]), (5, [0, 1, 2]), (1, [1, 2, 0]), (6, [2, 0, 1])],
    edges: [
        (0, false),
        (4, true),
        (2, false),
        (3, false),
        (11, true),
        (1, true),
        (6, false),
        (7, false),
        (8, false),
        (9, false),
        (10, false),
        (5, true),
    ],
};

const B_PRIME: CoordCube = CoordCube {
    corners: [(1, [1, 2, 0]), (6, [2, 0, 1]), (2, [0, 1, 2]), (3, [0, 1, 2]), (4, [0, 1, 2]), (5, [0, 1, 2]), (7, [1, 2, 0]), (0, [2, 0, 1])],
    edges: [
        (0, false),
        (5, true),
        (2, false),
        (3, false),
        (1, true),
        (11, true),
        (6, false),
        (7, false),
        (8, false),
        (9, false),
        (10, false),
        (4, true)
    ],
};

const L: CoordCube = CoordCube {
    corners: [(0, [0, 1, 2]), (6, [1, 2, 0]), (1, [2, 0, 1]), (3, [0, 1, 2]), (4, [0, 1, 2]), (2, [1, 2, 0]), (5, [2, 0, 1]), (7, [0, 1, 2])],
    edges: [
        (0, false),
        (1, false),
        (5, false),
        (3, false),
        (4, false),
        (10, false),
        (2, false),
        (7, false),
        (8, false),
        (9, false),
        (6, false),
        (11, false),
    ],
};

const L_PRIME: CoordCube = CoordCube {
    corners: [(0, [0, 1, 2]), (2, [1, 2, 0]), (5, [2, 0, 1]), (3, [0, 1, 2]), (4, [0, 1, 2]), (6, [1, 2, 0]), (1, [2, 0, 1]), (7, [0, 1, 2])],
    edges: [
        (0, false),
        (1, false),
        (6, false),
        (3, false),
        (4, false),
        (2, false),
        (10, false),
        (7, false),
        (8, false),
        (9, false),
        (5, false),
        (11, false),
    ],
};

const D: CoordCube = CoordCube {
    corners: [(0, [0, 1, 2]), (1, [0, 1, 2]), (2, [0, 1, 2]), (3, [0, 1, 2]), (5, [0, 1, 2]), (6, [0, 1, 2]), (7, [0, 1, 2]), (4, [0, 1, 2])],
    edges: [
        (0, false),
        (1, false),
        (2, false),
        (3, false),
        (4, false),
        (5, false),
        (6, false),
        (7, false),
        (9, false),
        (10, false),
        (11, false),
        (8, false),
    ],
};

const D_PRIME: CoordCube = CoordCube {
    corners: [(0, [0, 1, 2]), (1, [0, 1, 2]), (2, [0, 1, 2]), (3, [0, 1, 2]), (7, [0, 1, 2]), (4, [0, 1, 2]), (5, [0, 1, 2]), (6, [0, 1, 2])],
    edges: [
        (0, false),
        (1, false),
        (2, false),
        (3, false),
        (4, false),
        (5, false),
        (6, false),
        (7, false),
        (11, false),
        (8, false),
        (9, false),
        (10, false),
    ],
};

use super::move_sets::quarter_turns::QuarterTurn;
impl From<QuarterTurn> for CoordCube {
    fn from(qt: QuarterTurn) -> CoordCube {
        match qt {
            QuarterTurn::U => U,
            QuarterTurn::UPrime => U_PRIME,
            QuarterTurn::F => F,
            QuarterTurn::FPrime => F_PRIME,
            QuarterTurn::R => R,
            QuarterTurn::RPrime => R_PRIME,
            QuarterTurn::B => B,
            QuarterTurn::BPrime => B_PRIME,
            QuarterTurn::L => L,
            QuarterTurn::LPrime => L_PRIME,
            QuarterTurn::D => D,
            QuarterTurn::DPrime => D_PRIME,
        }
    }
}

const S_URF: CoordCube = CoordCube {
    corners: [(2, [1, 2, 0]), (5, [2, 0, 1]), (4, [1, 2, 0]), (3, [2, 0, 1]), (0, [1, 2, 0]), (7, [2, 0, 1]), (6, [1, 2, 0]), (1, [2, 0, 1])],
    edges: [
        (3, true),
        (6, false),
        (9, true),
        (7, false),
        (2, true),
        (10, true),
        (8, true),
        (0, true),
        (1, true),
        (4, false),
        (11, true),
        (5, false),
    ],
};

const S_F: CoordCube = CoordCube {
    corners: [(6, [0, 1, 2]), (7, [0, 1, 2]), (4, [0, 1, 2]), (5, [0, 1, 2]), (2, [0, 1, 2]), (3, [0, 1, 2]), (0, [0, 1, 2]), (1, [0, 1, 2])],
    edges: [
        (10, false),
        (11, false),
        (8, false),
        (9, false),
        (5, false),
        (4, false),
        (7, false),
        (6, false),
        (2, false),
        (3, false),
        (0, false),
        (1, false),
    ]
};

const S_U: CoordCube = CoordCube {
    corners: [(1, [0, 1, 2]), (2, [0, 1, 2]), (3, [0, 1, 2]), (0, [0, 1, 2]), (7, [0, 1, 2]), (4, [0, 1, 2]), (5, [0, 1, 2]), (6, [0, 1, 2])],
    edges: [
        (1, false),
        (2, false),
        (3, false),
        (0, false),
        (5, true),
        (6, true),
        (7, true),
        (4, true),
        (11, false),
        (8, false),
        (9, false),
        (10, false),
    ],
};

const S_MRL: CoordCube = CoordCube {
    corners: [(1, [0, 2, 1]), (0, [0, 2, 1]), (3, [0, 2, 1]), (2, [0, 2, 1]), (5, [0, 2, 1]), (4, [0, 2, 1]), (7, [0, 2, 1]), (6, [0, 2, 1])],
    edges: [
        (2, false),
        (1, false),
        (0, false),
        (3, false),
        (5, false),
        (4, false),
        (7, false),
        (6, false),
        (10, false),
        (9, false),
        (8, false),
        (11, false),
    ],
};

use super::move_sets::symmetry_generators::SymmetryGenerator;
impl From<SymmetryGenerator> for CoordCube {
    fn from(g1sg: SymmetryGenerator) -> CoordCube {
        match g1sg {
            SymmetryGenerator::SUrf => S_URF,
            SymmetryGenerator::SF => S_F,
            SymmetryGenerator::SU => S_U,
            SymmetryGenerator::SMrl => S_MRL,
        }
    }
}

fn corner_to_facelets(corner: u8) -> [u8; 3] {
    match corner {
      0 => [0, 13, 8],
      1 => [1, 17, 12],
      2 => [2, 5, 16],
      3 => [3, 9, 4],
      4 => [20, 7, 10],
      5 => [21, 19, 6],
      6 => [22, 15, 18],
      7 => [23, 11, 14],
      _ => panic!("Invalid corner input!"),
    }
}

fn edge_to_facelets(corner: u8) -> [u8; 2] {
    match corner {
      0 => [0, 9],
      1 => [1, 13],
      2 => [2, 17],
      3 => [3, 5],
      4 => [14, 8],
      5 => [12, 18],
      6 => [6, 16],
      7 => [4, 10],
      8 => [20, 11],
      9 => [21, 7],
      10 => [22, 19],
      11 => [23, 15],
      _ => panic!("Invalid edge input!"),
    }
}

use super::facelet_cube::FaceletCube;
impl From<CoordCube> for FaceletCube {
    fn from(coord_cube: CoordCube) -> FaceletCube {
        let mut fc = FaceletCube {
            corners: [0; 24],
            edges: [0; 24],
        };

        for i in 0..coord_cube.corners.len() {
            let indexes = corner_to_facelets(i as u8);
            let p = coord_cube.corners[i];
            let corner = corner_to_facelets(p.0);
            fc.corners[indexes[0] as usize] = corner[p.1[0] as usize];
            fc.corners[indexes[1] as usize] = corner[p.1[1] as usize];
            fc.corners[indexes[2] as usize] = corner[p.1[2] as usize];
        }

        for i in 0..coord_cube.edges.len() {
            let indexes = edge_to_facelets(i as u8);
            let edge = edge_to_facelets(coord_cube.edges[i].0);
            fc.edges[indexes[0] as usize] = edge[coord_cube.edges[i].1 as usize];
            fc.edges[indexes[1] as usize] = edge[(coord_cube.edges[i].1 as usize + 1) % 2];
        }

        fc
    }
}

impl fmt::Display for CoordCube {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        FaceletCube::from(*self).fmt(f)
    }
}

fn permute_corner_orientation(a: [u8; 3], b: [u8; 3]) -> [u8; 3] {
    [
        a[b[0] as usize],
        a[b[1] as usize],
        a[b[2] as usize],
    ]
}

// TODO: All functions like these should really just be implemented as
// PermutationGroups themselves, then the "permute" for the top level
// just uses the permute functions for each field.
fn permute_corners(a: &[(u8, [u8; 3]); 8], b: &[(u8, [u8; 3]); 8]) -> [(u8, [u8; 3]); 8] {
    [
        (a[b[0].0 as usize].0, permute_corner_orientation(a[b[0].0 as usize].1, b[0].1)),
        (a[b[1].0 as usize].0, permute_corner_orientation(a[b[1].0 as usize].1, b[1].1)),
        (a[b[2].0 as usize].0, permute_corner_orientation(a[b[2].0 as usize].1, b[2].1)),
        (a[b[3].0 as usize].0, permute_corner_orientation(a[b[3].0 as usize].1, b[3].1)),
        (a[b[4].0 as usize].0, permute_corner_orientation(a[b[4].0 as usize].1, b[4].1)),
        (a[b[5].0 as usize].0, permute_corner_orientation(a[b[5].0 as usize].1, b[5].1)),
        (a[b[6].0 as usize].0, permute_corner_orientation(a[b[6].0 as usize].1, b[6].1)),
        (a[b[7].0 as usize].0, permute_corner_orientation(a[b[7].0 as usize].1, b[7].1)),
    ]
}

fn permute_edges(a: &[(u8, bool); 12], b: &[(u8, bool); 12]) -> [(u8, bool); 12] {
    [
        (a[b[0].0 as usize].0, a[b[0].0 as usize].1 != b[0].1),
        (a[b[1].0 as usize].0, a[b[1].0 as usize].1 != b[1].1),
        (a[b[2].0 as usize].0, a[b[2].0 as usize].1 != b[2].1),
        (a[b[3].0 as usize].0, a[b[3].0 as usize].1 != b[3].1),
        (a[b[4].0 as usize].0, a[b[4].0 as usize].1 != b[4].1),
        (a[b[5].0 as usize].0, a[b[5].0 as usize].1 != b[5].1),
        (a[b[6].0 as usize].0, a[b[6].0 as usize].1 != b[6].1),
        (a[b[7].0 as usize].0, a[b[7].0 as usize].1 != b[7].1),
        (a[b[8].0 as usize].0, a[b[8].0 as usize].1 != b[8].1),
        (a[b[9].0 as usize].0, a[b[9].0 as usize].1 != b[9].1),
        (a[b[10].0 as usize].0, a[b[10].0 as usize].1 != b[10].1),
        (a[b[11].0 as usize].0, a[b[11].0 as usize].1 != b[11].1),
    ]
}

fn corner_identity() -> [(u8, [u8; 3]); 8] {
    CORNER_IDENTITY
}

fn edge_identity() -> [(u8, bool); 12] {
    EDGE_IDENTITY
}

fn corner_orientation_inv(a: &[u8; 3]) -> [u8; 3] {
    let mut r = [0; 3];
    r[a[0] as usize] = 0;
    r[a[1] as usize] = 1;
    r[a[2] as usize] = 2;
    r
}

fn corner_inv(a: &[(u8, [u8; 3]); 8]) -> [(u8, [u8; 3]); 8] {
    let mut r = [(0, [0; 3]); 8];
    r[a[0].0 as usize] = (0, corner_orientation_inv(&a[0].1));
    r[a[1].0 as usize] = (1, corner_orientation_inv(&a[1].1));
    r[a[2].0 as usize] = (2, corner_orientation_inv(&a[2].1));
    r[a[3].0 as usize] = (3, corner_orientation_inv(&a[3].1));
    r[a[4].0 as usize] = (4, corner_orientation_inv(&a[4].1));
    r[a[5].0 as usize] = (5, corner_orientation_inv(&a[5].1));
    r[a[6].0 as usize] = (6, corner_orientation_inv(&a[6].1));
    r[a[7].0 as usize] = (7, corner_orientation_inv(&a[7].1));
    r
}

fn edge_inv(a: &[(u8, bool); 12]) -> [(u8, bool); 12] {
    let mut r = [(0, false); 12];
    r[a[0].0 as usize] = (0, a[0].1);
    r[a[1].0 as usize] = (1, a[1].1);
    r[a[2].0 as usize] = (2, a[2].1);
    r[a[3].0 as usize] = (3, a[3].1);
    r[a[4].0 as usize] = (4, a[4].1);
    r[a[5].0 as usize] = (5, a[5].1);
    r[a[6].0 as usize] = (6, a[6].1);
    r[a[7].0 as usize] = (7, a[7].1);
    r[a[8].0 as usize] = (8, a[8].1);
    r[a[9].0 as usize] = (9, a[9].1);
    r[a[10].0 as usize] = (10, a[10].1);
    r[a[11].0 as usize] = (11, a[11].1);
    r
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;

    #[test]
    fn permute_corners_works_for_a_simple_swap() {
        let simple_swap = [(1, [2, 0, 1]), (0, [1, 2, 0]), (2, [0, 1, 2]), (3, [0, 1, 2]), (4, [0, 1, 2]), (5, [0, 1, 2]), (6, [0, 1, 2]), (7, [0, 1, 2])];
        assert_eq!(CORNER_IDENTITY, permute_corners(&simple_swap, &simple_swap));
    }

    #[test]
    fn permute_corners_works_for_a_three_cycle() {
        let three_cycle = [(1, [2, 0, 1]), (2, [2, 0, 1]), (0, [2, 0, 1]), (3, [0, 1, 2]), (4, [0, 1, 2]), (5, [0, 1, 2]), (6, [0, 1, 2]), (7, [0, 1, 2])];
        assert_eq!(CORNER_IDENTITY, permute_corners(&permute_corners(&three_cycle, &three_cycle), &three_cycle));
    }

    #[test]
    fn permute_edges_works_for_a_simple_swap() {
        let simple_swap = [
            (1, true),
            (0, true),
            (2, false),
            (3, false),
            (4, false),
            (5, false),
            (6, false),
            (7, false),
            (8, false),
            (9, false),
            (10, false),
            (11, false),
        ];
        assert_eq!(EDGE_IDENTITY, permute_edges(&simple_swap, &simple_swap));
    }

    #[test]
    fn permute_edges_works_for_a_four_cycle() {
        let four_cycle = [
            (1, true),
            (2, false),
            (3, true),
            (0, false),
            (4, false),
            (5, false),
            (6, false),
            (7, false),
            (8, false),
            (9, false),
            (10, false),
            (11, false),
        ];
        assert_eq!(EDGE_IDENTITY, permute_edges(&permute_edges(&permute_edges(&four_cycle, &four_cycle), &four_cycle), &four_cycle));
    }

    #[test]
    fn u_is_not_identity() {
        assert_ne!(CoordCube::from(QuarterTurn::U), CoordCube::identity());
    }

    #[test]
    fn u_invert_is_its_prime() {
        assert_eq!(CoordCube::from(QuarterTurn::U).invert(), CoordCube::from(QuarterTurn::UPrime));
    }

    #[test]
    fn f_invert_is_its_prime() {
        assert_eq!(CoordCube::from(QuarterTurn::F).invert(), CoordCube::from(QuarterTurn::FPrime));
    }

    #[test]
    fn r_invert_is_its_prime() {
        assert_eq!(CoordCube::from(QuarterTurn::R).invert(), CoordCube::from(QuarterTurn::RPrime));
    }

    #[test]
    fn b_invert_is_its_prime() {
        assert_eq!(CoordCube::from(QuarterTurn::B).invert(), CoordCube::from(QuarterTurn::BPrime));
    }

    #[test]
    fn l_invert_is_its_prime() {
        assert_eq!(CoordCube::from(QuarterTurn::L).invert(), CoordCube::from(QuarterTurn::LPrime));
    }

    #[test]
    fn d_invert_is_its_prime() {
        assert_eq!(CoordCube::from(QuarterTurn::D).invert(), CoordCube::from(QuarterTurn::DPrime));
    }

    #[test]
    fn u_3_times_is_its_prime() {
        let u_prime = CoordCube::from(QuarterTurn::U)
            .permute(QuarterTurn::U.into())
            .permute(QuarterTurn::U.into());
        assert_eq!(u_prime, CoordCube::from(QuarterTurn::UPrime));
    }

    #[test]
    fn f_3_times_is_its_prime() {
        let f_prime = CoordCube::from(QuarterTurn::F)
            .permute(QuarterTurn::F.into())
            .permute(QuarterTurn::F.into());
        assert_eq!(f_prime, CoordCube::from(QuarterTurn::FPrime));
    }

    #[test]
    fn r_3_times_is_its_prime() {
        let r_prime = CoordCube::from(QuarterTurn::R)
            .permute(QuarterTurn::R.into())
            .permute(QuarterTurn::R.into());
        assert_eq!(r_prime, CoordCube::from(QuarterTurn::RPrime));
    }

    #[test]
    fn b_3_times_is_its_prime() {
        let b_prime = CoordCube::from(QuarterTurn::B)
            .permute(QuarterTurn::B.into())
            .permute(QuarterTurn::B.into());
        assert_eq!(b_prime, CoordCube::from(QuarterTurn::BPrime));
    }

    #[test]
    fn l_3_times_is_its_prime() {
        let l_prime = CoordCube::from(QuarterTurn::L)
            .permute(QuarterTurn::L.into())
            .permute(QuarterTurn::L.into());
        assert_eq!(l_prime, CoordCube::from(QuarterTurn::LPrime));
    }

    #[test]
    fn d_3_times_is_its_prime() {
        let d_prime = CoordCube::from(QuarterTurn::D)
            .permute(QuarterTurn::D.into())
            .permute(QuarterTurn::D.into());
        assert_eq!(d_prime, CoordCube::from(QuarterTurn::DPrime));
    }

    #[test]
    fn should_be_able_to_generate_one_turn_with_a_combination_of_the_other_five() {
        let d = CoordCube::from(QuarterTurn::R)
            .permute(QuarterTurn::L.into())
            .permute(QuarterTurn::F.into())
            .permute(QuarterTurn::F.into())
            .permute(QuarterTurn::B.into())
            .permute(QuarterTurn::B.into())
            .permute(QuarterTurn::RPrime.into())
            .permute(QuarterTurn::LPrime.into())
            .permute(QuarterTurn::U.into())
            .permute(QuarterTurn::R.into())
            .permute(QuarterTurn::L.into())
            .permute(QuarterTurn::FPrime.into())
            .permute(QuarterTurn::FPrime.into())
            .permute(QuarterTurn::BPrime.into())
            .permute(QuarterTurn::BPrime.into())
            .permute(QuarterTurn::RPrime.into())
            .permute(QuarterTurn::LPrime.into());
        assert_eq!(d, QuarterTurn::D.into());
    }

    #[test]
    fn d_and_d_prime_should_be_the_identity() {
        let d = CoordCube::from(QuarterTurn::D)
            .permute(QuarterTurn::DPrime.into());
        assert_eq!(d, CoordCube::identity());
    }

    #[test]
    fn u_and_u_prime_should_be_the_identity() {
        let u = CoordCube::from(QuarterTurn::U)
            .permute(QuarterTurn::UPrime.into());
        assert_eq!(u, CoordCube::identity());
    }

    #[test]
    fn f_and_f_prime_should_be_the_identity() {
        let f = CoordCube::from(QuarterTurn::F)
            .permute(QuarterTurn::FPrime.into());
        assert_eq!(f, CoordCube::identity());
    }

    #[test]
    fn r_and_r_prime_should_be_the_identity() {
        let r = CoordCube::from(QuarterTurn::R)
            .permute(QuarterTurn::RPrime.into());
        assert_eq!(r, CoordCube::identity());
    }

    #[test]
    fn b_and_b_prime_should_be_the_identity() {
        let b = CoordCube::from(QuarterTurn::B)
            .permute(QuarterTurn::BPrime.into());
        assert_eq!(b, CoordCube::identity());
    }

    #[test]
    fn l_and_l_prime_should_be_the_identity() {
        let l = CoordCube::from(QuarterTurn::L)
            .permute(QuarterTurn::LPrime.into());
        assert_eq!(l, CoordCube::identity());
    }

    #[test]
    fn s_urf_3_times_is_the_identity() {
        let id = CoordCube::from(SymmetryGenerator::SUrf)
            .permute(SymmetryGenerator::SUrf.into())
            .permute(SymmetryGenerator::SUrf.into());
        assert_eq!(id, CoordCube::identity());
    }

    #[test]
    fn s_f_2_times_is_the_identity() {
        let id = CoordCube::from(SymmetryGenerator::SF)
            .permute(SymmetryGenerator::SF.into());
        assert_eq!(id, CoordCube::identity());
    }

    #[test]
    fn s_u_4_times_is_the_identity() {
        let id = CoordCube::from(SymmetryGenerator::SU)
            .permute(SymmetryGenerator::SU.into())
            .permute(SymmetryGenerator::SU.into())
            .permute(SymmetryGenerator::SU.into());
        assert_eq!(id, CoordCube::identity());
    }

    #[test]
    fn s_mrl_2_times_is_the_identity() {
        let id = CoordCube::from(SymmetryGenerator::SMrl)
            .permute(SymmetryGenerator::SMrl.into());
        assert_eq!(id, CoordCube::identity());
    }

    #[test]
    fn u_is_symmetrical_to_f() {
        let f = CoordCube::from(SymmetryGenerator::SUrf)
            .permute(QuarterTurn::U.into())
            .permute(SymmetryGenerator::SUrf.into())
            .permute(SymmetryGenerator::SUrf.into());
        assert_eq!(f, QuarterTurn::F.into());
    }

    #[test]
    fn u_is_symmetrical_to_d() {
        let d = CoordCube::from(SymmetryGenerator::SF)
            .permute(QuarterTurn::U.into())
            .permute(SymmetryGenerator::SF.into());
        assert_eq!(d, QuarterTurn::D.into());
    }

    #[test]
    fn f_is_symmetrical_to_r() {
        let r = CoordCube::from(SymmetryGenerator::SU)
            .permute(QuarterTurn::F.into())
            .permute(SymmetryGenerator::SU.into())
            .permute(SymmetryGenerator::SU.into())
            .permute(SymmetryGenerator::SU.into());
        assert_eq!(r, QuarterTurn::R.into());
    }

    #[test]
    fn r_is_symmetrical_to_b() {
        let r = CoordCube::from(SymmetryGenerator::SU)
            .permute(SymmetryGenerator::SU.into())
            .permute(SymmetryGenerator::SU.into())
            .permute(QuarterTurn::B.into())
            .permute(SymmetryGenerator::SU.into());
        assert_eq!(r, QuarterTurn::R.into());
    }

    #[test]
    fn b_is_symmetrical_to_l() {
        let r = CoordCube::from(SymmetryGenerator::SU)
            .permute(SymmetryGenerator::SU.into())
            .permute(SymmetryGenerator::SU.into())
            .permute(QuarterTurn::L.into())
            .permute(SymmetryGenerator::SU.into());
        assert_eq!(r, QuarterTurn::B.into());
    }

    #[test]
    fn u_is_symmetrical_to_u_prime() {
        let u_prime = CoordCube::from(SymmetryGenerator::SMrl)
            .permute(QuarterTurn::U.into())
            .permute(SymmetryGenerator::SMrl.into());
        assert_eq!(u_prime, QuarterTurn::UPrime.into());
    }

    #[test]
    fn r_is_symmetrical_to_l_prime() {
        let l_prime = CoordCube::from(SymmetryGenerator::SMrl)
            .permute(QuarterTurn::R.into())
            .permute(SymmetryGenerator::SMrl.into());
        assert_eq!(l_prime, QuarterTurn::LPrime.into());
    }

    #[test]
    fn r_f_is_symmetrical_to_l_prime_f_prime() {
        let actual = CoordCube::from(SymmetryGenerator::SMrl)
            .permute(QuarterTurn::R.into())
            .permute(QuarterTurn::F.into())
            .permute(SymmetryGenerator::SMrl.into());
        let expected = CoordCube::from(QuarterTurn::LPrime)
            .permute(QuarterTurn::FPrime.into());
        println!("{}", actual);
        println!("{}", expected);
        assert_eq!(actual, expected);
    }

    #[bench]
    fn repeat_all_turn_identity_sequence(b: &mut Bencher) {
        b.iter(|| {
            CoordCube::from(QuarterTurn::R)
                .permute(QuarterTurn::L.into())
                .permute(QuarterTurn::F.into())
                .permute(QuarterTurn::F.into())
                .permute(QuarterTurn::B.into())
                .permute(QuarterTurn::B.into())
                .permute(QuarterTurn::RPrime.into())
                .permute(QuarterTurn::LPrime.into())
                .permute(QuarterTurn::U.into())
                .permute(QuarterTurn::R.into())
                .permute(QuarterTurn::L.into())
                .permute(QuarterTurn::FPrime.into())
                .permute(QuarterTurn::FPrime.into())
                .permute(QuarterTurn::BPrime.into())
                .permute(QuarterTurn::BPrime.into())
                .permute(QuarterTurn::RPrime.into())
                .permute(QuarterTurn::LPrime.into())
                .permute(QuarterTurn::D.into());
        });
    }
}
