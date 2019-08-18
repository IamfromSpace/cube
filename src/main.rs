#![feature(test)]
extern crate test;

mod permutation_group;
mod facelet_cube;
mod move_table;
mod util;

use permutation_group::PermutationGroup as PG;
// TODO: do not use arr_identity directly
use facelet_cube::{FaceletCube, arr_identity};

enum Facelet {
    U0,
    U1,
    U2,
    U3,
    F0,
    F1,
    F2,
    F3,
    R0,
    R1,
    R2,
    R3,
    B0,
    B1,
    B2,
    B3,
    L0,
    L1,
    L2,
    L3,
    D0,
    D1,
    D2,
    D3,
}

fn main() {
    use Facelet::*;

    // Create a Clockwise turn of the U face
    let mut e = arr_identity();
    e[U0 as usize] = U1 as u8;
    e[U1 as usize] = U2 as u8;
    e[U2 as usize] = U3 as u8;
    e[U3 as usize] = U0 as u8;
    e[F1 as usize] = R1 as u8;
    e[R1 as usize] = B1 as u8;
    e[B1 as usize] = L1 as u8;
    e[L1 as usize] = F1 as u8;

    let mut c = arr_identity();
    c[U0 as usize] = U1 as u8;
    c[U1 as usize] = U2 as u8;
    c[U2 as usize] = U3 as u8;
    c[U3 as usize] = U0 as u8;
    c[F0 as usize] = R0 as u8;
    c[R0 as usize] = B0 as u8;
    c[B0 as usize] = L0 as u8;
    c[L0 as usize] = F0 as u8;
    c[F1 as usize] = R1 as u8;
    c[R1 as usize] = B1 as u8;
    c[B1 as usize] = L1 as u8;
    c[L1 as usize] = F1 as u8;

    let u = FaceletCube {
        edges: e,
        corners: c,
    };

    // Create a Clockwise turn of the whole cube on the axis from URF to DBL
    let mut e = arr_identity();
    e[U0 as usize] = F1 as u8;
    e[U1 as usize] = F2 as u8;
    e[U2 as usize] = F3 as u8;
    e[U3 as usize] = F0 as u8;
    e[F0 as usize] = R1 as u8;
    e[F1 as usize] = R2 as u8;
    e[F2 as usize] = R3 as u8;
    e[F3 as usize] = R0 as u8;
    e[R0 as usize] = U2 as u8;
    e[R1 as usize] = U3 as u8;
    e[R2 as usize] = U0 as u8;
    e[R3 as usize] = U1 as u8;
    e[B0 as usize] = L3 as u8;
    e[B1 as usize] = L0 as u8;
    e[B2 as usize] = L1 as u8;
    e[B3 as usize] = L2 as u8;
    e[L0 as usize] = D0 as u8;
    e[L1 as usize] = D1 as u8;
    e[L2 as usize] = D2 as u8;
    e[L3 as usize] = D3 as u8;
    e[D0 as usize] = B1 as u8;
    e[D1 as usize] = B2 as u8;
    e[D2 as usize] = B3 as u8;
    e[D3 as usize] = B0 as u8;

    let mut c = arr_identity();
    c[U0 as usize] = F1 as u8;
    c[U1 as usize] = F2 as u8;
    c[U2 as usize] = F3 as u8;
    c[U3 as usize] = F0 as u8;
    c[F0 as usize] = R1 as u8;
    c[F1 as usize] = R2 as u8;
    c[F2 as usize] = R3 as u8;
    c[F3 as usize] = R0 as u8;
    c[R0 as usize] = U2 as u8;
    c[R1 as usize] = U3 as u8;
    c[R2 as usize] = U0 as u8;
    c[R3 as usize] = U1 as u8;
    c[B0 as usize] = L3 as u8;
    c[B1 as usize] = L0 as u8;
    c[B2 as usize] = L1 as u8;
    c[B3 as usize] = L2 as u8;
    c[L0 as usize] = D0 as u8;
    c[L1 as usize] = D1 as u8;
    c[L2 as usize] = D2 as u8;
    c[L3 as usize] = D3 as u8;
    c[D0 as usize] = B1 as u8;
    c[D1 as usize] = B2 as u8;
    c[D2 as usize] = B3 as u8;
    c[D3 as usize] = B0 as u8;

    let s_urf = FaceletCube {
        edges: e,
        corners: c,
    };

    // Create a 180deg turn of the whole cube on the F face
    let mut e = arr_identity();
    e[U0 as usize] = D2 as u8;
    e[U1 as usize] = D3 as u8;
    e[U2 as usize] = D0 as u8;
    e[U3 as usize] = D1 as u8;
    e[F0 as usize] = F2 as u8;
    e[F1 as usize] = F3 as u8;
    e[F2 as usize] = F0 as u8;
    e[F3 as usize] = F1 as u8;
    e[R0 as usize] = L2 as u8;
    e[R1 as usize] = L3 as u8;
    e[R2 as usize] = L0 as u8;
    e[R3 as usize] = L1 as u8;
    e[B0 as usize] = B2 as u8;
    e[B1 as usize] = B3 as u8;
    e[B2 as usize] = B0 as u8;
    e[B3 as usize] = B1 as u8;
    e[L0 as usize] = R2 as u8;
    e[L1 as usize] = R3 as u8;
    e[L2 as usize] = R0 as u8;
    e[L3 as usize] = R1 as u8;
    e[D0 as usize] = U2 as u8;
    e[D1 as usize] = U3 as u8;
    e[D2 as usize] = U0 as u8;
    e[D3 as usize] = U1 as u8;

    let mut c = arr_identity();
    c[U0 as usize] = D2 as u8;
    c[U1 as usize] = D3 as u8;
    c[U2 as usize] = D0 as u8;
    c[U3 as usize] = D1 as u8;
    c[F0 as usize] = F2 as u8;
    c[F1 as usize] = F3 as u8;
    c[F2 as usize] = F0 as u8;
    c[F3 as usize] = F1 as u8;
    c[R0 as usize] = L2 as u8;
    c[R1 as usize] = L3 as u8;
    c[R2 as usize] = L0 as u8;
    c[R3 as usize] = L1 as u8;
    c[B0 as usize] = B2 as u8;
    c[B1 as usize] = B3 as u8;
    c[B2 as usize] = B0 as u8;
    c[B3 as usize] = B1 as u8;
    c[L0 as usize] = R2 as u8;
    c[L1 as usize] = R3 as u8;
    c[L2 as usize] = R0 as u8;
    c[L3 as usize] = R1 as u8;
    c[D0 as usize] = U2 as u8;
    c[D1 as usize] = U3 as u8;
    c[D2 as usize] = U0 as u8;
    c[D3 as usize] = U1 as u8;

    let s_f = FaceletCube {
        edges: e,
        corners: c,
    };

    // Create a Clockwise turn of the whole cube on the U face
    let mut e = arr_identity();
    e[U0 as usize] = U1 as u8;
    e[U1 as usize] = U2 as u8;
    e[U2 as usize] = U3 as u8;
    e[U3 as usize] = U0 as u8;
    e[F0 as usize] = R0 as u8;
    e[F1 as usize] = R1 as u8;
    e[F2 as usize] = R2 as u8;
    e[F3 as usize] = R3 as u8;
    e[R0 as usize] = B0 as u8;
    e[R1 as usize] = B1 as u8;
    e[R2 as usize] = B2 as u8;
    e[R3 as usize] = B3 as u8;
    e[B0 as usize] = L0 as u8;
    e[B1 as usize] = L1 as u8;
    e[B2 as usize] = L2 as u8;
    e[B3 as usize] = L3 as u8;
    e[L0 as usize] = F0 as u8;
    e[L1 as usize] = F1 as u8;
    e[L2 as usize] = F2 as u8;
    e[L3 as usize] = F3 as u8;
    e[D0 as usize] = D3 as u8;
    e[D1 as usize] = D0 as u8;
    e[D2 as usize] = D1 as u8;
    e[D3 as usize] = D2 as u8;

    let mut c = arr_identity();
    c[U0 as usize] = U1 as u8;
    c[U1 as usize] = U2 as u8;
    c[U2 as usize] = U3 as u8;
    c[U3 as usize] = U0 as u8;
    c[F0 as usize] = R0 as u8;
    c[F1 as usize] = R1 as u8;
    c[F2 as usize] = R2 as u8;
    c[F3 as usize] = R3 as u8;
    c[R0 as usize] = B0 as u8;
    c[R1 as usize] = B1 as u8;
    c[R2 as usize] = B2 as u8;
    c[R3 as usize] = B3 as u8;
    c[B0 as usize] = L0 as u8;
    c[B1 as usize] = L1 as u8;
    c[B2 as usize] = L2 as u8;
    c[B3 as usize] = L3 as u8;
    c[L0 as usize] = F0 as u8;
    c[L1 as usize] = F1 as u8;
    c[L2 as usize] = F2 as u8;
    c[L3 as usize] = F3 as u8;
    c[D0 as usize] = D3 as u8;
    c[D1 as usize] = D0 as u8;
    c[D2 as usize] = D1 as u8;
    c[D3 as usize] = D2 as u8;

    let s_u = FaceletCube {
        edges: e,
        corners: c,
    };

    // Create a mirror of the whole cube from the left to right side
    let mut e = arr_identity();
    e[U0 as usize] = U2 as u8;
    e[U2 as usize] = U0 as u8;
    e[F0 as usize] = F2 as u8;
    e[F2 as usize] = F0 as u8;
    e[R0 as usize] = L2 as u8;
    e[R1 as usize] = L1 as u8;
    e[R2 as usize] = L0 as u8;
    e[R3 as usize] = L3 as u8;
    e[B0 as usize] = B2 as u8;
    e[B2 as usize] = B0 as u8;
    e[L0 as usize] = R2 as u8;
    e[L1 as usize] = R1 as u8;
    e[L2 as usize] = R0 as u8;
    e[L3 as usize] = R3 as u8;
    e[D0 as usize] = D2 as u8;
    e[D2 as usize] = D0 as u8;

    let mut c = arr_identity();
    c[U0 as usize] = U1 as u8;
    c[U1 as usize] = U0 as u8;
    c[U2 as usize] = U3 as u8;
    c[U3 as usize] = U2 as u8;
    c[F0 as usize] = F1 as u8;
    c[F1 as usize] = F0 as u8;
    c[F2 as usize] = F3 as u8;
    c[F3 as usize] = F2 as u8;
    c[R0 as usize] = L1 as u8;
    c[R1 as usize] = L0 as u8;
    c[R2 as usize] = L3 as u8;
    c[R3 as usize] = L2 as u8;
    c[B0 as usize] = B1 as u8;
    c[B1 as usize] = B0 as u8;
    c[B2 as usize] = B3 as u8;
    c[B3 as usize] = B2 as u8;
    c[L0 as usize] = R1 as u8;
    c[L1 as usize] = R0 as u8;
    c[L2 as usize] = R3 as u8;
    c[L3 as usize] = R2 as u8;
    c[D0 as usize] = D1 as u8;
    c[D1 as usize] = D0 as u8;
    c[D2 as usize] = D3 as u8;
    c[D3 as usize] = D2 as u8;

    let s_mrl = FaceletCube {
        edges: e,
        corners: c,
    };

    let mut syms = Vec::with_capacity(48);
    for i in 0..48 {
        let urfs = i % 3;
        let fs = i / 3 % 2;
        let us = i / 6 % 4;
        let ms = i / 24;

        let mut c: FaceletCube = PG::identity();
        for _ in 0..urfs {
            c = c.permute(s_urf);
        }
        for _ in 0..fs {
            c = c.permute(s_f);
        }
        for _ in 0..us {
            c = c.permute(s_u);
        }
        for _ in 0..ms {
            c = c.permute(s_mrl);
        }
        syms.push(c);
    }

    let f = u.apply_symmetry(syms[2]);
    let r = u.apply_symmetry(syms[1]);
    let b = u.apply_symmetry(syms[19]);
    let l = u.apply_symmetry(syms[4]);
    let d = u.apply_symmetry(syms[3]);

    let turns = vec![
        u,
        u.invert(),
        f,
        f.invert(),
        r,
        r.invert(),
        b,
        b.invert(),
        l,
        l.invert(),
        d,
        d.invert(),
    ];

    let mt: move_table::MoveTable<FaceletCube> = move_table::new(&turns, syms, 9);
    move_table::solve(&mt, &f);
}
