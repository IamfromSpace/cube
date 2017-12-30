use std::convert::From;
use std::u32;

#[derive(Debug)]
struct CoordCube {
    orientations: u32,
    edges: u32,
    corners: u32,
}

#[derive(Debug)]
struct CubiesCube {
    c_p: [Corner; 8],
    c_o: [CornerOr; 8],
    e_p: [Edge; 12],
    e_o: [EdgeOr; 12],
}

#[derive(Debug, Copy, Clone, PartialOrd, PartialEq)]
enum Corner {
    URF,
    UFL,
    ULB,
    UBR,
    DFR,
    DLF,
    DBL,
    DRB,
}

#[derive(Debug, Copy, Clone)]
enum CornerOr {
    Correct,
    Clock,
    CounterClock,
}

#[derive(Debug, Copy, Clone)]
enum EdgeOr {
    Correct,
    Flipped,
}

#[derive(Debug, Copy, Clone, PartialOrd, PartialEq)]
enum Edge {
    UF,
    UL,
    UB,
    UR,
    FR,
    FL,
    BL,
    BR,
    DF,
    DL,
    DB,
    DR,
}

impl CubiesCube {
    fn turn_mut(&mut self, turn: HalfTurn) -> &CubiesCube {
        match turn {
            // Big ol' TODO
            HalfTurn::F => {
                let temp = self.c_p[Corner::URF as usize];
                self.c_p[Corner::URF as usize] = self.c_p[Corner::UFL as usize];
                self.c_p[Corner::UFL as usize] = self.c_p[Corner::DLF as usize];
                self.c_p[Corner::DLF as usize] = self.c_p[Corner::DFR as usize];
                self.c_p[Corner::DFR as usize] = temp;
                self
            }
            _ => self,
        }
    }
}

fn perm_to_index<T: std::cmp::PartialOrd>(pm: &[T]) -> u32 {
    let n = pm.len();
    let mut t = 0;
    for i in (1..n).rev() {
        let mut s = 0;
        for j in (0..i).rev() {
            if pm[j] > pm[i] {
                s += 1;
            }
        }
        t = (t + s) * i as u32
    }
    t
}

// Need to supply a mutable array of all 0s, which will be filled in
fn index_to_perm(mut t: u32, pm: &mut [u32]) -> &mut [u32] {
    let n = pm.len();
    pm[n - 1] = 1;
    for i in (0..n).rev() {
        pm[i] = t % (n as u32 - i as u32);
        t = t.wrapping_div(n as u32 - i as u32);
        for j in (i + 1)..n {
            if pm[j] >= pm[i] {
                pm[j] += 1;
            }
        }
    }
    pm
}

impl From<CubiesCube> for CoordCube {
    fn from(cubies: CubiesCube) -> Self {
        let mut orientations = 0;
        for i in 0..12 {
            orientations |= (cubies.e_o[i] as u32) << (i);
        }
        for i in 0..8 {
            orientations |= (cubies.c_o[i] as u32) << (i * 2 + 12);
        }
        let corners = perm_to_index(&cubies.c_p);
        let edges = perm_to_index(&cubies.e_p);
        CoordCube {
            orientations,
            corners,
            edges,
        }
    }
}

impl From<CoordCube> for CubiesCube {
    fn from(coord: CoordCube) -> Self {
        CubiesCube {
            c_p: [
                Corner::DFR,
                Corner::UFL,
                Corner::ULB,
                Corner::URF,
                Corner::DRB,
                Corner::DLF,
                Corner::DBL,
                Corner::UBR,
            ],
            c_o: [
                CornerOr::Correct,
                CornerOr::Correct,
                CornerOr::Correct,
                CornerOr::Correct,
                CornerOr::Correct,
                CornerOr::Correct,
                CornerOr::Correct,
                CornerOr::Correct,
            ],
            e_p: [
                Edge::FR,
                Edge::UL,
                Edge::UB,
                Edge::UF,
                Edge::BR,
                Edge::FL,
                Edge::BL,
                Edge::UR,
                Edge::DF,
                Edge::DL,
                Edge::DB,
                Edge::DR,
            ],
            e_o: [
                EdgeOr::Flipped,
                EdgeOr::Flipped,
                EdgeOr::Flipped,
                EdgeOr::Flipped,
                EdgeOr::Flipped,
                EdgeOr::Flipped,
                EdgeOr::Flipped,
                EdgeOr::Flipped,
                EdgeOr::Flipped,
                EdgeOr::Flipped,
                EdgeOr::Flipped,
                EdgeOr::Flipped,
            ],
        }
    }
}

enum HalfTurn {
    U,
    UC,
    D,
    DC,
    F,
    FC,
    B,
    BC,
    L,
    LC,
    R,
    RC,
}

fn main() {
    let cubies = CubiesCube {
        c_p: [
            Corner::DFR,
            Corner::UFL,
            Corner::ULB,
            Corner::URF,
            Corner::DRB,
            Corner::DLF,
            Corner::DBL,
            Corner::UBR,
        ],
        c_o: [
            CornerOr::Clock,
            CornerOr::Clock,
            CornerOr::Clock,
            CornerOr::Clock,
            CornerOr::Clock,
            CornerOr::Clock,
            CornerOr::Clock,
            CornerOr::Clock,
        ],
        e_p: [
            Edge::FR,
            Edge::UL,
            Edge::UB,
            Edge::UF,
            Edge::BR,
            Edge::FL,
            Edge::BL,
            Edge::UR,
            Edge::DF,
            Edge::DL,
            Edge::DB,
            Edge::DR,
        ],
        e_o: [
            EdgeOr::Flipped,
            EdgeOr::Flipped,
            EdgeOr::Flipped,
            EdgeOr::Flipped,
            EdgeOr::Flipped,
            EdgeOr::Flipped,
            EdgeOr::Flipped,
            EdgeOr::Flipped,
            EdgeOr::Flipped,
            EdgeOr::Flipped,
            EdgeOr::Flipped,
            EdgeOr::Flipped,
        ],
    };
    for i in 0..8 {
        println!("{}", cubies.c_p[i] as u32);
    }
    let coord = CoordCube::from(cubies);
    println!("coord: {:?}", coord);
    let mut repermed: [u32; 8] = [0, 0, 0, 0, 0, 0, 0, 0];
    index_to_perm(coord.corners, &mut repermed);
    println!("repermed: {:?}", repermed);
    println!("repermed as index: {:?}", perm_to_index(&repermed));
}
