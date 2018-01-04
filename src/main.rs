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
    fn cycle_corners_mut(&mut self, cycle: &[(Corner, u64)]) -> () {
        match cycle.get(0) {
            Some(&(pi0, oi0)) => {
                let tmp_p = self.get_corner_p(pi0 as usize);
                let tmp_o = (self.get_corner_o(pi0 as usize) + oi0) % 3;
                let mut p_last = pi0;
                for i in 1..cycle.len() {
                    let (pi, oi) = cycle[i];
                    let next_p = self.get_corner_p(pi as usize);
                    let next_o = (self.get_corner_o(pi as usize) + oi) % 3;
                    self.set_corner_p_mut(p_last as usize, next_p);
                    self.set_corner_o_mut(p_last as usize, next_o);
                    p_last = pi;
                }
                self.set_corner_p_mut(p_last as usize, tmp_p);
                self.set_corner_o_mut(p_last as usize, tmp_o);
            }
            None => (),
        }
    }
    fn cycle_edges_mut(&mut self, cycle: &[(Edge, u64)]) -> () {
        match cycle.get(0) {
            Some(&(pi0, oi0)) => {
                let tmp_p = self.get_edge_p(pi0 as usize);
                let tmp_o = (self.get_edge_o(pi0 as usize) + oi0) % 2;
                let mut p_last = pi0;
                for i in 1..cycle.len() {
                    let (pi, oi) = cycle[i];
                    let next_p = self.get_edge_p(pi as usize);
                    let next_o = (self.get_edge_o(pi as usize) + oi) % 2;
                    self.set_edge_p_mut(p_last as usize, next_p);
                    self.set_edge_o_mut(p_last as usize, next_o);
                    p_last = pi;
                }
                self.set_edge_p_mut(p_last as usize, tmp_p);
                self.set_edge_o_mut(p_last as usize, tmp_o);
            }
            None => (),
        }
    }
    fn turn_mut(&mut self, turn: HalfTurn) -> () {
        self.cycle_corners_mut(&half_turn_to_corner_cycle(turn));
        self.cycle_edges_mut(&half_turn_to_edge_cycle(turn));
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

#[derive(Debug, Copy, Clone)]
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

fn half_turn_to_corner_cycle(turn: HalfTurn) -> [(Corner, u64); 4] {
    use HalfTurn::*;
    use Corner::*;
    match turn {
        U => [(URF, 0), (UBR, 0), (ULB, 0), (UFL, 0)],
        UC => [(UFL, 0), (ULB, 0), (UBR, 0), (URF, 0)],
        D => [(DFR, 0), (DLF, 0), (DBL, 0), (DBL, 0)],
        DC => [(DBL, 0), (DBL, 0), (DLF, 0), (DFR, 0)],
        F => [(URF, 1), (UFL, 2), (DLF, 1), (DFR, 2)],
        FC => [(DFR, 2), (DLF, 1), (UFL, 2), (URF, 1)],
        //TODO:
        B => [(URF, 0), (UBR, 0), (ULB, 0), (UFL, 0)],
        BC => [(URF, 0), (UBR, 0), (ULB, 0), (UFL, 0)],
        L => [(URF, 0), (UBR, 0), (ULB, 0), (UFL, 0)],
        LC => [(URF, 0), (UBR, 0), (ULB, 0), (UFL, 0)],
        R => [(URF, 0), (UBR, 0), (ULB, 0), (UFL, 0)],
        RC => [(URF, 0), (UBR, 0), (ULB, 0), (UFL, 0)],
    }
}

fn half_turn_to_edge_cycle(turn: HalfTurn) -> [(Edge, u64); 4] {
    use HalfTurn::*;
    use Edge::*;
    match turn {
        U => [(UF, 0), (UR, 0), (UB, 0), (UL, 0)],
        UC => [(UL, 0), (UB, 0), (UR, 0), (UF, 0)],
        D => [(DF, 0), (DL, 0), (DB, 0), (DR, 0)],
        DC => [(DR, 0), (DB, 0), (DL, 0), (DF, 0)],
        //TODO:
        F => [(UF, 0), (UR, 0), (UB, 0), (UL, 0)],
        FC => [(UF, 0), (UR, 0), (UB, 0), (UL, 0)],
        B => [(UF, 0), (UR, 0), (UB, 0), (UL, 0)],
        BC => [(UF, 0), (UR, 0), (UB, 0), (UL, 0)],
        L => [(UF, 0), (UR, 0), (UB, 0), (UL, 0)],
        LC => [(UF, 0), (UR, 0), (UB, 0), (UL, 0)],
        R => [(UF, 0), (UR, 0), (UB, 0), (UL, 0)],
        RC => [(UF, 0), (UR, 0), (UB, 0), (UL, 0)],
    }
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
