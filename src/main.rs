#![feature(test)]
extern crate test;

use std::hash::Hash;

mod permutation_group;
mod facelet_cube;
mod coord_cube;
mod g1_coord_cube;
mod g1_coord_cube_compact;
mod move_sets;
mod move_table;
mod util;

use permutation_group::PermutationGroup as PG;
use facelet_cube::FaceletCube;
use g1_coord_cube::G1CoordCube;
use g1_coord_cube_compact::G1CoordCubeCompact;
use move_sets::quarter_turns::QuarterTurn;
use move_sets::g1_turns::G1Turn;
use move_sets::symmetry_generators::SymmetryGenerator;
use move_sets::g1_symmetry_generators::G1SymmetryGenerator;

fn g1_move_table<Stored: Hash + Eq + Send + Sync + Copy + From<Used>, Used: PG + Ord + Sync + Copy + From<Stored> + From<G1SymmetryGenerator> + From<G1Turn>>(n: usize) -> move_table::MoveTable<Stored, Used> {
    let mut syms = Vec::with_capacity(16);
    for i in 0..16 {
        let fs = i % 2;
        let us = i / 2 % 4;
        let ms = i / 8;

        let mut c: Used = PG::identity();
        for _ in 0..fs {
            c = c.permute(G1SymmetryGenerator::SF.into());
        }
        for _ in 0..us {
            c = c.permute(G1SymmetryGenerator::SU.into());
        }
        for _ in 0..ms {
            c = c.permute(G1SymmetryGenerator::SMrl.into());
        }
        syms.push(c);
    }

    let turns: Vec<Used> = vec![
        G1Turn::U.into(),
        G1Turn::UPrime.into(),
        G1Turn::F2.into(),
        G1Turn::R2.into(),
        G1Turn::B2.into(),
        G1Turn::L2.into(),
        G1Turn::D.into(),
        G1Turn::DPrime.into(),
    ];

    move_table::new(&turns, syms, n)
}

fn quarter_turn_move_table<Stored: Hash + Eq + Send + Sync + Copy + From<Used>, Used: PG + Ord + Sync + Copy + From<Stored> + From<SymmetryGenerator> + From<QuarterTurn>>(n: usize) -> move_table::MoveTable<Stored, Used> {
    let mut syms = Vec::with_capacity(48);
    for i in 0..48 {
        let urfs = i % 3;
        let fs = i / 3 % 2;
        let us = i / 6 % 4;
        let ms = i / 24;

        let mut c: Used = PG::identity();
        for _ in 0..urfs {
            c = c.permute(SymmetryGenerator::SUrf.into());
        }
        for _ in 0..fs {
            c = c.permute(SymmetryGenerator::SF.into());
        }
        for _ in 0..us {
            c = c.permute(SymmetryGenerator::SU.into());
        }
        for _ in 0..ms {
            c = c.permute(SymmetryGenerator::SMrl.into());
        }
        syms.push(c);
    }

    let turns: Vec<Used> = vec![
        QuarterTurn::U.into(),
        QuarterTurn::UPrime.into(),
        QuarterTurn::F.into(),
        QuarterTurn::FPrime.into(),
        QuarterTurn::R.into(),
        QuarterTurn::RPrime.into(),
        QuarterTurn::B.into(),
        QuarterTurn::BPrime.into(),
        QuarterTurn::L.into(),
        QuarterTurn::LPrime.into(),
        QuarterTurn::D.into(),
        QuarterTurn::DPrime.into(),
    ];

    move_table::new(&turns, syms, n)
}

fn main() {
    let qt_mt: move_table::MoveTable<FaceletCube, FaceletCube> = quarter_turn_move_table(4);
    move_table::solve(&qt_mt, &QuarterTurn::U.into());

    let g1c_mt: move_table::MoveTable<G1CoordCubeCompact, G1CoordCube> = g1_move_table(4);
    move_table::solve(&g1c_mt, &G1Turn::U.into());
}
