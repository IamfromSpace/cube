#![feature(test)]
extern crate test;
extern crate bincode;
#[macro_use]
extern crate serde;

use std::hash::Hash;

mod invertable;
mod permutation_group;
mod equivalence_class;
mod facelet_cube;
mod coord_cube;
mod g1_coord_cube;
mod g1_coord_cube_compact;
mod cubie_orientations_and_ud_slice;
mod two_by_two_by_two;
mod move_sets;
mod move_table;
mod coord_move_table;
mod pruning_table;
mod util;

use std::fs::File;
use std::io::Write;
use invertable::Invertable;
use permutation_group::PermutationGroup as PG;
use equivalence_class::EquivalenceClass;
use facelet_cube::FaceletCube;
use cubie_orientations_and_ud_slice::CubieOrientationAndUDSlice;
use coord_cube::CoordCube;
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

fn group_h_pruning_table<Stored: Hash + Eq + Ord + Send + Sync + Copy + From<Used>, Used: PG + Send + Sync + Copy + EquivalenceClass<G1SymGenList> + From<Stored> + From<QuarterTurn>>(n: usize) -> pruning_table::PruningTable<Stored, Used, G1SymGenList, QuarterTurn> {
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

    pruning_table::new(turns, syms, n)
}

fn two_by_two_by_two_pruning_table<Stored: Hash + Eq + Ord + Send + Sync + Copy + From<Used>, Used: PG + Send + Sync + Copy + EquivalenceClass<SymGenList> + From<Stored> + From<QuarterTurn>>(n: usize) -> pruning_table::PruningTable<Stored, Used, SymGenList, QuarterTurn> {
    // TODO: SymGenList has more syms than we'd like the type system to enforce
    let mut syms = Vec::with_capacity(3);
    for i in 0..3 {
        let mut c: SymGenList = PG::identity();
        for _ in 0..i {
            c = c.permute(SymmetryGenerator::SUrf.into());
        }
        syms.push(c);
    }

    // TODO: Definitely prefer half turn here
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

    pruning_table::new(turns, syms, n)
}

fn solve_two_by_two_by_two(
    pt: &pruning_table::PruningTable<two_by_two_by_two::TwoByTwoByTwo, CoordCube, SymGenList, QuarterTurn>,
    scramble: &CoordCube,
) -> Option<Vec<QuarterTurn>> {
    let mut best = None;
    for i in 0..8 {
        let fs = i % 2;
        let us = i / 2 % 4;

        let mut c: SymGenList = PG::identity();
        for _ in 0..fs {
            c = c.permute(SymmetryGenerator::SF.into());
        }
        for _ in 0..us {
            c = c.permute(SymmetryGenerator::SU.into());
        }

        let uncorrected_solution: Option<Vec<QuarterTurn>> = pt.solve(&scramble.get_equivalent(&c));
        let solution = match uncorrected_solution {
            None => None,
            Some(moves) => {
                let mut s = Vec::with_capacity(moves.len());
                for m in moves {
                    s.push(m.get_equivalent(&c.invert()));
                }
                Some(s)
            },
        };

        match best {
            None => best = solution,
            Some(b) => {
                match solution {
                    None => best = Some(b),
                    Some(s) => {
                        if s.len() < b.len() {
                            best = Some(s);
                        } else {
                            best = Some(b);
                        }
                    }
                }
            }
        }
    }
    best
}

use std::convert::TryInto;

// TODO: The Turn type should be parameterized
// it needs something like the following, but this didn't satisfy From<Turn> for CoordCube
// Turn: Copy + invertable::Invertable + Into<CoordCube> + EquivalenceClass<G1SymGenList>
fn two_phase(
    gh_mt: &pruning_table::PruningTable<CubieOrientationAndUDSlice, CoordCube, G1SymGenList, QuarterTurn>,
    g1_mt: &move_table::MoveTable<G1CoordCubeCompact, G1CoordCube, G1SymGenList, G1Turn>,
    scramble: &CoordCube,
) -> Option<Vec<QuarterTurn>> {
    let mut ts = gh_mt.solve(&scramble)?;

    let mut half_solved = scramble.clone();
    for &t in &ts {
        half_solved = half_solved.permute(t.into());
    }
    let hs_g1 = half_solved
        .try_into()
        .expect("Solution to move scramble into G1 didn't result to G1!");

    let t2s = g1_mt.solve(&hs_g1)?;
    for t in t2s {
        // TODO: this should be more generic and use a From somewhere
        match t {
            G1Turn::U => ts.push(QuarterTurn::U),
            G1Turn::UPrime => ts.push(QuarterTurn::UPrime),
            G1Turn::F2 => {
                ts.push(QuarterTurn::F);
                ts.push(QuarterTurn::F);
            },
            G1Turn::R2 => {
                ts.push(QuarterTurn::R);
                ts.push(QuarterTurn::R);
            },
            G1Turn::B2 => {
                ts.push(QuarterTurn::B);
                ts.push(QuarterTurn::B);
            },
            G1Turn::L2 => {
                ts.push(QuarterTurn::L);
                ts.push(QuarterTurn::L);
            },
            G1Turn::D => ts.push(QuarterTurn::D),
            G1Turn::DPrime => ts.push(QuarterTurn::DPrime),
        }
    }
    Some(ts)
}

fn main() {
    let qt_mt: move_table::MoveTable<FaceletCube, FaceletCube, SymGenList, QuarterTurn> = quarter_turn_move_table(4);
    qt_mt.solve(&QuarterTurn::U.into());

    let g1c_mt: move_table::MoveTable<G1CoordCubeCompact, G1CoordCube, G1SymGenList, G1Turn> = g1_move_table(4);
    g1c_mt.solve(&G1Turn::U.into());

    let file_name = "group_h_pruning_table";
    let create_and_write = false;
    // TODO: These should be buffered to be efficient
    let gh_pt: pruning_table::PruningTable<CubieOrientationAndUDSlice, CoordCube, G1SymGenList, QuarterTurn> = if create_and_write {
        let x = group_h_pruning_table(4);
        let serialized = bincode::serialize(&x).unwrap();

        let mut file = File::create(file_name).expect("Could not create file!");
        file.write_all(&serialized).expect("Could not write file!");
        x
    } else {
        let contents = std::fs::read(file_name).expect("Could not read file");
        bincode::deserialize(&contents).expect("Failed to deserialize!")
    };
    gh_pt.solve(&QuarterTurn::U.into());

    two_phase(&gh_pt, &g1c_mt, &QuarterTurn::U.into());
}

#[cfg(test)]
#[macro_use]
extern crate quickcheck;
extern crate rand;

#[cfg(test)]
mod tests {
    use super::*;
    use super::quickcheck::Gen;
    use super::rand::Rng;

    impl super::quickcheck::Arbitrary for QuarterTurn {
        fn arbitrary<G: Gen>(g: &mut G) -> QuarterTurn {
            *g.choose(&[
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
            ]).unwrap()
        }
    }

    impl super::quickcheck::Arbitrary for G1Turn {
        fn arbitrary<G: Gen>(g: &mut G) -> G1Turn {
            *g.choose(&[
              G1Turn::U,
              G1Turn::UPrime,
              G1Turn::F2,
              G1Turn::R2,
              G1Turn::B2,
              G1Turn::L2,
              G1Turn::D,
              G1Turn::DPrime,
            ]).unwrap()
        }
    }

    use quickcheck::TestResult;
    quickcheck! {
        fn gh_pt_works(turns: Vec<QuarterTurn>) -> TestResult {
            let max_turns = 3;
            if turns.len() > max_turns {
                TestResult::discard()
            } else {
                let gh_pt: pruning_table::PruningTable<CubieOrientationAndUDSlice, CoordCube, G1SymGenList, QuarterTurn> = group_h_pruning_table(max_turns + 1);

                let mut scramble = CoordCube::identity();
                for turn in turns {
                    scramble = scramble.permute(turn.into());
                }
                let scramble = scramble;
                match gh_pt.solve(&scramble) {
                    Some(solution) => {
                        let mut solved = scramble;
                        for turn in solution {
                            solved = solved.permute(turn.into());
                        }
                        TestResult::from_bool(CubieOrientationAndUDSlice::from(solved).is_solved())
                    },
                    None => TestResult::from_bool(false),
                }
            }
        }
    }

    quickcheck! {
        fn g1c_mt_works(turns: Vec<G1Turn>) -> TestResult {
            let max_turns = 4;
            if turns.len() > max_turns {
                TestResult::discard()
            } else {
                let g1c_mt: move_table::MoveTable<G1CoordCube, G1CoordCube, G1SymGenList, G1Turn> = g1_move_table(max_turns + 1);

                let mut scramble = G1CoordCube::identity();
                for turn in turns {
                    scramble = scramble.permute(turn.into());
                }
                let scramble = scramble;
                match g1c_mt.solve(&scramble) {
                    Some(solution) => {
                        let mut solved = scramble;
                        for turn in solution {
                            solved = solved.permute(turn.into());
                        }
                        TestResult::from_bool(solved == G1CoordCube::identity())
                    },
                    None => TestResult::from_bool(false),
                }
            }
        }
    }
}
