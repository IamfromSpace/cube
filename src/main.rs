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
mod facelet_wing_edges;
mod coord_cube;
mod coord_wing_edges;
mod g1_coord_cube;
mod g1_coord_cube_compact;
mod cubie_orientations_and_ud_slice;
mod two_by_two_by_two;
mod two_by_two_by_three;
mod ftl_minus_keyhole;
mod move_sets;
mod representative_table;
mod move_table;
mod flat_move_table;
mod coord_move_table;
mod pruning_table;
mod util;

#[cfg(test)]
mod two_triangles;

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
use move_sets::face_turns::FaceTurn;
use move_sets::quarter_turns::QuarterTurn;
use move_sets::g1_turns::G1Turn;
use move_sets::symmetry_generators::{SymmetryGenerator, SymGenList};
use move_sets::g1_symmetry_generators::{G1SymmetryGenerator, G1SymGenList};

/* This function tries solving a cube via a pruning table in numerous ways
 * where the cube is rotated differently in each approach.  This is useful for
 * coordinates (such as a 2x2x2) where there are multiple ways to accomplish
 * a solve (there are eight different 2x2x2 blocks we could construct).
 */
// TODO: This can be broken down further into  find-all/select-best
// TODO: Further parameterize types
fn multi_approach_solve<Stored: Hash + Eq + Ord + From<CoordCube>>(
    pt: &pruning_table::PruningTable<Stored, CoordCube, SymGenList, FaceTurn>,
    syms: &Vec<SymGenList>,
    scramble: &CoordCube,
) -> Option<(Vec<FaceTurn>, SymGenList)> {
    let mut best = None;
    for sym in syms {
        let sym = sym.clone();
        let uncorrected_solution: Option<Vec<FaceTurn>> = pt.solve(&scramble.get_equivalent(&sym));
        let solution = match uncorrected_solution {
            None => None,
            Some(moves) => {
                let mut s = Vec::with_capacity(moves.len());
                for m in moves {
                    s.push(m.get_equivalent(&sym.invert()));
                }
                Some((s, sym))
            },
        };

        match best {
            None => best = solution,
            Some(b) => {
                match solution {
                    None => best = Some(b),
                    Some(s) => {
                        if s.0.len() < b.0.len() {
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
        G1Turn::U2,
        G1Turn::UPrime,
        G1Turn::F2,
        G1Turn::R2,
        G1Turn::B2,
        G1Turn::L2,
        G1Turn::D,
        G1Turn::D2,
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

fn group_h_pruning_table<Stored: Hash + Eq + Ord + Send + Sync + Copy + From<Used>, Used: PG + Send + Sync + Copy + EquivalenceClass<G1SymGenList> + From<Stored> + From<FaceTurn>>(n: usize) -> pruning_table::PruningTable<Stored, Used, G1SymGenList, FaceTurn> {
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

    let turns: Vec<FaceTurn> = vec![
        FaceTurn::U,
        FaceTurn::U2,
        FaceTurn::UPrime,
        FaceTurn::F,
        FaceTurn::F2,
        FaceTurn::FPrime,
        FaceTurn::R,
        FaceTurn::R2,
        FaceTurn::RPrime,
        FaceTurn::B,
        FaceTurn::B2,
        FaceTurn::BPrime,
        FaceTurn::L,
        FaceTurn::L2,
        FaceTurn::LPrime,
        FaceTurn::D,
        FaceTurn::D2,
        FaceTurn::DPrime,
    ];

    pruning_table::new(turns, syms, n)
}

fn two_by_two_by_two_pruning_table<Stored: Hash + Eq + Ord + Send + Sync + Copy + From<Used>, Used: PG + Send + Sync + Copy + EquivalenceClass<SymGenList> + From<Stored> + From<FaceTurn>>(n: usize) -> pruning_table::PruningTable<Stored, Used, SymGenList, FaceTurn> {
    // TODO: SymGenList has more syms than we'd like the type system to enforce
    let mut syms = Vec::with_capacity(3);
    for i in 0..3 {
        let mut c: SymGenList = PG::identity();
        for _ in 0..i {
            c = c.permute(SymmetryGenerator::SUrf.into());
        }
        syms.push(c);
    }

    let turns: Vec<FaceTurn> = vec![
        FaceTurn::U,
        FaceTurn::U2,
        FaceTurn::UPrime,
        FaceTurn::F,
        FaceTurn::F2,
        FaceTurn::FPrime,
        FaceTurn::R,
        FaceTurn::R2,
        FaceTurn::RPrime,
        FaceTurn::B,
        FaceTurn::B2,
        FaceTurn::BPrime,
        FaceTurn::L,
        FaceTurn::L2,
        FaceTurn::LPrime,
        FaceTurn::D,
        FaceTurn::D2,
        FaceTurn::DPrime,
    ];

    pruning_table::new(turns, syms, n)
}

fn solve_two_by_two_by_two(
    pt: &pruning_table::PruningTable<two_by_two_by_two::TwoByTwoByTwo, CoordCube, SymGenList, FaceTurn>,
    scramble: &CoordCube,
) -> Option<(Vec<FaceTurn>, SymGenList)> {
    let mut syms = Vec::with_capacity(8);
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
        syms.push(c);
    }

    multi_approach_solve(&pt, &syms, &scramble)
}

fn two_by_two_by_three_pruning_table<Stored: Hash + Eq + Ord + Send + Sync + Copy + From<Used>, Used: PG + Send + Sync + Copy + EquivalenceClass<SymGenList> + From<Stored> + From<FaceTurn>>(n: usize) -> pruning_table::PruningTable<Stored, Used, SymGenList, FaceTurn> {
    // TODO: SymGenList has more syms than we'd like the type system to enforce
    let mut syms = Vec::with_capacity(2);
    for i in 0..2 {
        let mut c: SymGenList = PG::identity();
        for _ in 0..i {
            c = c.permute(SymmetryGenerator::SMrl.into());
        }
        syms.push(c);
    }

    let turns: Vec<FaceTurn> = vec![
        FaceTurn::U,
        FaceTurn::U2,
        FaceTurn::UPrime,
        FaceTurn::F,
        FaceTurn::F2,
        FaceTurn::FPrime,
        FaceTurn::R,
        FaceTurn::R2,
        FaceTurn::RPrime,
        FaceTurn::B,
        FaceTurn::B2,
        FaceTurn::BPrime,
        FaceTurn::L,
        FaceTurn::L2,
        FaceTurn::LPrime,
        FaceTurn::D,
        FaceTurn::D2,
        FaceTurn::DPrime,
    ];

    pruning_table::new(turns, syms, n)
}

fn solve_two_by_two_by_three(
    pt: &pruning_table::PruningTable<two_by_two_by_three::TwoByTwoByThree, CoordCube, SymGenList, FaceTurn>,
    scramble: &CoordCube,
) -> Option<(Vec<FaceTurn>, SymGenList)> {
    let mut syms = Vec::with_capacity(8);
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
        syms.push(c);
    }

    multi_approach_solve(&pt, &syms, &scramble)
}

fn ftl_minus_keyhole_pruning_table<Stored: Hash + Eq + Ord + Send + Sync + Copy + From<Used>, Used: PG + Send + Sync + Copy + EquivalenceClass<SymGenList> + From<Stored> + From<FaceTurn>>(n: usize) -> pruning_table::PruningTable<Stored, Used, SymGenList, FaceTurn> {
    // TODO: SymGenList has more syms than we'd like the type system to enforce
    let mut syms = Vec::with_capacity(2);
    for i in 0..2 {
        let mut c: SymGenList = PG::identity();
        for _ in 0..i {
            // The FTL-Keyhole has exactly one symmetry where the cube
            // is rotated and then reflected to move the keyhold back to
            // its original position, which is reflection along the diagonal
            // from the URF corner to the UBL corner.
            c = c.permute(SymmetryGenerator::SU.into());
            c = c.permute(SymmetryGenerator::SMrl.into());
        }
        syms.push(c);
    }

    let turns: Vec<FaceTurn> = vec![
        FaceTurn::U,
        FaceTurn::U2,
        FaceTurn::UPrime,
        FaceTurn::F,
        FaceTurn::F2,
        FaceTurn::FPrime,
        FaceTurn::R,
        FaceTurn::R2,
        FaceTurn::RPrime,
        FaceTurn::B,
        FaceTurn::B2,
        FaceTurn::BPrime,
        FaceTurn::L,
        FaceTurn::L2,
        FaceTurn::LPrime,
        FaceTurn::D,
        FaceTurn::D2,
        FaceTurn::DPrime,
    ];

    pruning_table::new(turns, syms, n)
}

fn two_phase_two_by_two_by_three(
    two_pt: &pruning_table::PruningTable<two_by_two_by_two::TwoByTwoByTwo, CoordCube, SymGenList, FaceTurn>,
    three_pt: &pruning_table::PruningTable<two_by_two_by_three::TwoByTwoByThree, CoordCube, SymGenList, FaceTurn>,
    scramble: &CoordCube,
) -> Option<(Vec<FaceTurn>, SymGenList)> {
    let (two, move_corner_to_urf) = solve_two_by_two_by_two(&two_pt, &scramble)?;
    let mut remaining = *scramble;
    for &t in &two {
        remaining = remaining.permute(t.into());
    }
    remaining = remaining.get_equivalent(&move_corner_to_urf);

    let mut syms = Vec::with_capacity(3);
    for i in 0..3 {
        let mut c: SymGenList = PG::identity();
        for _ in 0..i {
            c = c.permute(SymmetryGenerator::SUrf.into());
        }
        syms.push(c);
    }

    let (three, sym) = multi_approach_solve(&three_pt, &syms, &remaining)?;
    let mut full_solution = two;
    for t in three {
        full_solution.push(t.get_equivalent(&move_corner_to_urf.invert()));
    }
    Some((full_solution, sym))
}

fn three_phase_ftl_minus_keyhole(
    two_pt: &pruning_table::PruningTable<two_by_two_by_two::TwoByTwoByTwo, CoordCube, SymGenList, FaceTurn>,
    three_pt: &pruning_table::PruningTable<two_by_two_by_three::TwoByTwoByThree, CoordCube, SymGenList, FaceTurn>,
    key_pt: &pruning_table::PruningTable<ftl_minus_keyhole::FtlMinusKeyhole, CoordCube, SymGenList, FaceTurn>,
    scramble: &CoordCube,
) -> Option<(Vec<FaceTurn>, SymGenList)> {
    let (three, move_to_uf) = two_phase_two_by_two_by_three(&two_pt, &three_pt, &scramble)?;
    let mut remaining = *scramble;
    for &t in &three {
        remaining = remaining.permute(t.into());
    }
    remaining = remaining.get_equivalent(&move_to_uf);

    // There are four ways to create the keyhole while utilizing
    // and existing 2x2x3 along the UF, and none are immediately obvious
    let syms = vec!(
        SymGenList::identity()
            .permute(SymmetryGenerator::SU.into()),
        SymGenList::identity()
            .permute(SymmetryGenerator::SU.into())
            .permute(SymmetryGenerator::SU.into()),
        SymGenList::identity()
            .permute(SymmetryGenerator::SUrf.into())
            .permute(SymmetryGenerator::SU.into())
            .permute(SymmetryGenerator::SU.into()),
        SymGenList::identity()
            .permute(SymmetryGenerator::SUrf.into())
            .permute(SymmetryGenerator::SU.into())
            .permute(SymmetryGenerator::SU.into())
            .permute(SymmetryGenerator::SU.into()),
    );

    let (key, sym) = multi_approach_solve(&key_pt, &syms, &remaining)?;
    let mut full_solution = three;
    for t in key {
        full_solution.push(t.get_equivalent(&move_to_uf.invert()));
    }
    Some((full_solution, sym))
}

use std::convert::TryInto;

// TODO: The Turn type should be parameterized
// it needs something like the following, but this didn't satisfy From<Turn> for CoordCube
// Turn: Copy + invertable::Invertable + Into<CoordCube> + EquivalenceClass<G1SymGenList>
fn two_phase(
    gh_mt: &pruning_table::PruningTable<CubieOrientationAndUDSlice, CoordCube, G1SymGenList, FaceTurn>,
    g1_mt: &move_table::MoveTable<G1CoordCubeCompact, G1CoordCube, G1SymGenList, G1Turn>,
    scramble: &CoordCube,
) -> Option<Vec<FaceTurn>> {
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
        ts.push(t.into());
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
    let gh_pt: pruning_table::PruningTable<CubieOrientationAndUDSlice, CoordCube, G1SymGenList, FaceTurn> = if create_and_write {
        let x = group_h_pruning_table(14);
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

    impl super::quickcheck::Arbitrary for FaceTurn {
        fn arbitrary<G: Gen>(g: &mut G) -> FaceTurn {
            *g.choose(&[
              FaceTurn::U,
              FaceTurn::U2,
              FaceTurn::UPrime,
              FaceTurn::F,
              FaceTurn::F2,
              FaceTurn::FPrime,
              FaceTurn::R,
              FaceTurn::R2,
              FaceTurn::RPrime,
              FaceTurn::B,
              FaceTurn::B2,
              FaceTurn::BPrime,
              FaceTurn::L,
              FaceTurn::L2,
              FaceTurn::LPrime,
              FaceTurn::D,
              FaceTurn::D2,
              FaceTurn::DPrime,
            ]).unwrap()
        }
    }

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
              G1Turn::U2,
              G1Turn::UPrime,
              G1Turn::F2,
              G1Turn::R2,
              G1Turn::B2,
              G1Turn::L2,
              G1Turn::D,
              G1Turn::D2,
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
                let gh_pt: pruning_table::PruningTable<CubieOrientationAndUDSlice, CoordCube, G1SymGenList, FaceTurn> = group_h_pruning_table(max_turns + 1);

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
