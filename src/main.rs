#![feature(test)]
extern crate test;

mod permutation_group;
mod facelet_cube;
mod move_sets;
mod move_table;
mod util;

use permutation_group::PermutationGroup as PG;
use facelet_cube::FaceletCube;
use move_sets::quarter_turns::QuarterTurn;
use move_sets::symmetry_generators::SymmetryGenerator;

fn main() {
    let mut syms = Vec::with_capacity(48);
    for i in 0..48 {
        let urfs = i % 3;
        let fs = i / 3 % 2;
        let us = i / 6 % 4;
        let ms = i / 24;

        let mut c: FaceletCube = PG::identity();
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

    let turns: Vec<FaceletCube> = vec![
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

    let mt: move_table::MoveTable<FaceletCube> = move_table::new(&turns, syms, 9);
    move_table::solve(&mt, &f);
}
