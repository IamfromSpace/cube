use std::convert::From;

#[derive(Debug)]
struct CoordCube {
    orientations_and_corners: u64,
    edges: u64,
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

impl CoordCube {
    fn get_corner_p(&self, i: usize) -> u64 {
        self.orientations_and_corners >> (i * 3 + 28) & 7
    }
    fn get_edge_p(&self, i: usize) -> u64 {
        self.edges >> (i * 4) & 15
    }
    fn get_edge_o(&self, i: usize) -> u64 {
        self.orientations_and_corners >> i & 1
    }
    fn get_corner_o(&self, i: usize) -> u64 {
        self.orientations_and_corners >> (i * 2 + 12) & 3
    }
    fn set_corner_p_mut(&mut self, i: usize, to: u64) -> () {
        let offset = i * 3 + 28;
        let new = to << offset;
        let mask = !(7 << offset);
        self.orientations_and_corners = self.orientations_and_corners & mask | new;
    }
    fn set_edge_p_mut(&mut self, i: usize, to: u64) -> () {
        let offset = i * 4;
        let new = to << offset;
        let mask = !(15 << offset);
        self.edges = self.edges & mask | new;
    }
    fn set_edge_o_mut(&mut self, i: usize, to: u64) -> () {
        let new = to << i;
        let mask = !(1 << i);
        self.orientations_and_corners = self.orientations_and_corners & mask | new;
    }
    fn set_corner_o_mut(&mut self, i: usize, to: u64) -> () {
        let offset = i * 2 + 12;
        let new = to << offset;
        let mask = !(3 << offset);
        self.orientations_and_corners = self.orientations_and_corners & mask | new;
    }
}

impl From<CubiesCube> for CoordCube {
    fn from(cubies: CubiesCube) -> Self {
        let mut orientations_and_corners = 0;
        let mut edges = 0;
        for i in 0..12 {
            orientations_and_corners |= (cubies.e_o[i] as u64) << i;
            edges |= (cubies.e_p[i] as u64) << (i * 4);
        }
        for i in 0..8 {
            orientations_and_corners |= (cubies.c_o[i] as u64) << (i * 2 + 12);
            orientations_and_corners |= (cubies.c_p[i] as u64) << (i * 3 + 28);
        }
        CoordCube {
            orientations_and_corners,
            edges,
        }
    }
}

impl From<CoordCube> for CubiesCube {
    fn from(coord: CoordCube) -> Self {
        let mut c_p = [Corner::URF; 8];
        let mut c_o = [CornerOr::Correct; 8];
        let mut e_p = [Edge::UF; 12];
        let mut e_o = [EdgeOr::Correct; 12];
        for i in 0..12 {
            e_p[i] = match coord.get_edge_p(i) {
                0 => Edge::UF,
                1 => Edge::UL,
                2 => Edge::UB,
                3 => Edge::UR,
                4 => Edge::FR,
                5 => Edge::FL,
                6 => Edge::BL,
                7 => Edge::BR,
                8 => Edge::DF,
                9 => Edge::DL,
                10 => Edge::DB,
                11 => Edge::DR,
                _ => panic!("coord edge position out of bounds!"),
            };

            e_o[i] = match coord.get_edge_o(i) {
                0 => EdgeOr::Correct,
                1 => EdgeOr::Flipped,
                _ => panic!("coord edge orientation out of bounds!"),
            };
        }
        for i in 0..8 {
            c_p[i] = match coord.get_corner_p(i) {
                0 => Corner::URF,
                1 => Corner::UFL,
                2 => Corner::ULB,
                3 => Corner::UBR,
                4 => Corner::DFR,
                5 => Corner::DLF,
                6 => Corner::DBL,
                7 => Corner::DRB,
                _ => panic!("coord corner position out of bounds!"),
            };
            c_o[i] = match coord.get_corner_o(i) {
                0 => CornerOr::Correct,
                1 => CornerOr::Clock,
                2 => CornerOr::CounterClock,
                _ => panic!("coord corner orientation out of bounds!"),
            };
        }
        CubiesCube { c_p, c_o, e_p, e_o }
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
    println!("cubies: {:?}", cubies);
    let coord = CoordCube::from(cubies);
    println!("coord: {:?}", coord);
    let cubies2 = CubiesCube::from(coord);
    println!("cubies: {:?}", cubies2);
    let mut coord2 = CoordCube::from(cubies2);
    println!("coord: {:?}", coord2);
    let corner0 = coord2.get_corner_p(0);
    let corner1 = coord2.get_corner_p(1);
    let edge0 = coord2.get_edge_p(0);
    let edge1 = coord2.get_edge_p(1);
    coord2.set_corner_p_mut(0, corner1);
    coord2.set_corner_p_mut(1, corner0);
    coord2.set_edge_p_mut(0, edge1);
    coord2.set_edge_p_mut(1, edge0);
    coord2.set_edge_o_mut(0, 0);
    coord2.set_corner_o_mut(0, 0);
    println!("coord: {:?}", coord2);
    let cubies3 = CubiesCube::from(coord2);
    println!("cubies: {:?}", cubies3);
}
