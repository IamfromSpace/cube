#![feature(test)]
extern crate test;

use std::hash::Hash;

mod invertable;
mod permutation_group;
mod equivalence_class;
mod facelet_cube;
mod coord_cube;
mod g1_coord_cube;
mod g1_coord_cube_compact;
mod cubie_orientations_and_ud_slice;
mod move_sets;
mod move_table;
mod util;

use permutation_group::PermutationGroup as PG;
use equivalence_class::EquivalenceClass;
use facelet_cube::FaceletCube;
use g1_coord_cube::G1CoordCube;
use g1_coord_cube_compact::G1CoordCubeCompact;
use move_sets::quarter_turns::QuarterTurn;
use move_sets::g1_turns::G1Turn;
use move_sets::symmetry_generators::{SymmetryGenerator, SymGenList};
use move_sets::g1_symmetry_generators::{G1SymmetryGenerator, G1SymGenList};

fn g1_move_table<Stored: Hash + Eq + Send + Sync + Copy + From<Used>, Used: PG + Ord + Send + Sync + Copy + EquivalenceClass<G1SymGenList> + From<Stored> + From<G1Turn>>(n: usize) -> move_table::MoveTable<Stored, Used, G1SymGenList, G1Turn> {
    let mut syms = Vec::with_capacity(16);
    for i in 0..16 {
        let fs = i % 2;
        let us = i / 2 % 4;
        let ms = i / 8;

        let mut c: G1SymGenList = PG::identity();
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

    let turns: Vec<G1Turn> = vec![
        G1Turn::U,
        G1Turn::UPrime,
        G1Turn::F2,
        G1Turn::R2,
        G1Turn::B2,
        G1Turn::L2,
        G1Turn::D,
        G1Turn::DPrime,
    ];

    move_table::new(turns, syms, n)
}

fn quarter_turn_move_table<Stored: Hash + Eq + Send + Sync + Copy + From<Used>, Used: PG + Ord + Send + Sync + Copy + EquivalenceClass<SymGenList> + From<Stored> + From<QuarterTurn>>(n: usize) -> move_table::MoveTable<Stored, Used, SymGenList, QuarterTurn> {
    let mut syms = Vec::with_capacity(48);
    for i in 0..48 {
        let urfs = i % 3;
        let fs = i / 3 % 2;
        let us = i / 6 % 4;
        let ms = i / 24;

        let mut c: SymGenList = PG::identity();
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

    let turns: Vec<QuarterTurn> = vec![
        QuarterTurn::U,
        QuarterTurn::UPrime,
        QuarterTurn::F,
        QuarterTurn::FPrime,
        QuarterTurn::R,
        QuarterTurn::RPrime,
        QuarterTurn::B,
        QuarterTurn::BPrime,
        QuarterTurn::L,
        QuarterTurn::LPrime,
        QuarterTurn::D,
        QuarterTurn::DPrime,
    ];

    move_table::new(turns, syms, n)
}

fn main() {
    let qt_mt: move_table::MoveTable<FaceletCube, FaceletCube, SymGenList, QuarterTurn> = quarter_turn_move_table(4);
    move_table::solve(&qt_mt, &QuarterTurn::U.into());

    let g1c_mt: move_table::MoveTable<G1CoordCubeCompact, G1CoordCube, G1SymGenList, G1Turn> = g1_move_table(4);
    move_table::solve(&g1c_mt, &G1Turn::U.into());
}
