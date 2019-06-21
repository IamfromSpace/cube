#![feature(i128_type, test)]
use std::fmt;
use std::collections::HashSet;
use std::hash::Hash;
use std::sync::Mutex;

extern crate ansi_term;
extern crate test;
extern crate crossbeam;

use crossbeam::thread;

struct CoordCube {
    corners: i128,
    edges: i128,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
struct FaceletCube {
    // u8 benchmarked as fastest for permuting
    corners: [u8; 24],
    edges: [u8; 24],
}

impl fmt::Display for FaceletCube {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ansi_term::Colour::*;
        let w = White.paint("██");
        let g = Green.paint("██");
        let r = Red.paint("██");
        let b = Blue.paint("██");
        let o = RGB(250, 48, 11).paint("██");
        let y = Yellow.paint("██");
        let c = |id: usize| match self.corners[id] >> 2 {
            0 => &w,
            1 => &g,
            2 => &r,
            3 => &b,
            4 => &o,
            5 => &y,
            _ => panic!("could not match color"),
        };
        let e = |id: usize| match self.edges[id] >> 2 {
            0 => &w,
            1 => &g,
            2 => &r,
            3 => &b,
            4 => &o,
            5 => &y,
            _ => panic!("could not match color"),
        };
        write!(
            f,
            "\n         ┌──┬──┬──┐\
             \n         │{}│{}│{}│\
             \n         ├──┼──┼──┤\
             \n         │{}│{}│{}│\
             \n         ├──┼──┼──┤\
             \n         │{}│{}│{}│\
             \n┌──┬──┬──┼──┼──┼──┼──┬──┬──┬──┬──┬──┐\
             \n│{}│{}│{}│{}│{}│{}│{}│{}│{}│{}│{}│{}│\
             \n├──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┤\
             \n│{}│{}│{}│{}│{}│{}│{}│{}│{}│{}│{}│{}│\
             \n├──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┤\
             \n│{}│{}│{}│{}│{}│{}│{}│{}│{}│{}│{}│{}│\
             \n└──┴──┴──┼──┼──┼──┼──┴──┴──┴──┴──┴──┘\
             \n         │{}│{}│{}│\
             \n         ├──┼──┼──┤\
             \n         │{}│{}│{}│\
             \n         ├──┼──┼──┤\
             \n         │{}│{}│{}│\
             \n         └──┴──┴──┘",
            c(1), e(1), c(0),
            e(2), w, e(0),
            c(2), e(3), c(3),

            c(17), e(17), c(16),
            c(5), e(5), c(4),
            c(9), e(9), c(8),
            c(13), e(13), c(12),

            e(16), o, e(16),
            e(6), g, e(4),
            e(10), r, e(8),
            e(14), b, e(12),

            c(18), e(19), c(19),
            c(6), e(7), c(7),
            c(10), e(11), c(11),
            c(14), e(15), c(15),

            c(21), e(21), c(20),
            e(22), y, e(20),
            c(22), e(23), c(23),
        )
    }
}

fn permute_arr_inv(a: [u8; 24], b: [u8; 24]) -> [u8; 24] {
    let mut r = [0; 24];
    //silly looking, but twice as fast ;)
    r[b[0] as usize] = a[0];
    r[b[1] as usize] = a[1];
    r[b[2] as usize] = a[2];
    r[b[3] as usize] = a[3];
    r[b[4] as usize] = a[4];
    r[b[5] as usize] = a[5];
    r[b[6] as usize] = a[6];
    r[b[7] as usize] = a[7];
    r[b[8] as usize] = a[8];
    r[b[9] as usize] = a[9];
    r[b[10] as usize] = a[10];
    r[b[11] as usize] = a[11];
    r[b[12] as usize] = a[12];
    r[b[13] as usize] = a[13];
    r[b[14] as usize] = a[14];
    r[b[15] as usize] = a[15];
    r[b[16] as usize] = a[16];
    r[b[17] as usize] = a[17];
    r[b[18] as usize] = a[18];
    r[b[19] as usize] = a[19];
    r[b[20] as usize] = a[20];
    r[b[21] as usize] = a[21];
    r[b[22] as usize] = a[22];
    r[b[23] as usize] = a[23];
    r
}

fn permute_arr(a: [u8; 24], b: [u8; 24]) -> [u8; 24] {
    [
        a[b[0] as usize],
        a[b[1] as usize],
        a[b[2] as usize],
        a[b[3] as usize],
        a[b[4] as usize],
        a[b[5] as usize],
        a[b[6] as usize],
        a[b[7] as usize],
        a[b[8] as usize],
        a[b[9] as usize],
        a[b[10] as usize],
        a[b[11] as usize],
        a[b[12] as usize],
        a[b[13] as usize],
        a[b[14] as usize],
        a[b[15] as usize],
        a[b[16] as usize],
        a[b[17] as usize],
        a[b[18] as usize],
        a[b[19] as usize],
        a[b[20] as usize],
        a[b[21] as usize],
        a[b[22] as usize],
        a[b[23] as usize],
    ]
}

fn permute_cube(a: FaceletCube, b: FaceletCube) -> FaceletCube {
    FaceletCube {
        edges: permute_arr(a.edges, b.edges),
        corners: permute_arr(a.corners, b.corners),
    }
}

fn permute_cube_inv(a: FaceletCube, b: FaceletCube) -> FaceletCube {
    FaceletCube {
        edges: permute_arr_inv(a.edges, b.edges),
        corners: permute_arr_inv(a.corners, b.corners),
    }
}

fn greatest_equivalence(
    syms_inv: &[FaceletCube; 48],
    syms: &[FaceletCube; 48],
    perm: FaceletCube,
) -> FaceletCube {
    let mut greatest = perm;
    for i in 1..48 {
        let e = permute_cube(permute_cube(syms_inv[i], perm), syms[i]);
        if e > greatest {
            greatest = e;
        }
        let e_inv = permute_cube(permute_cube_inv(syms_inv[i], perm), syms[i]);
        if e_inv > greatest {
            greatest = e_inv;
        }
    }
    greatest
}

fn permute(a: i128, b: i128) -> i128 {
    let mut r = 0;
    let mut mask = 31;
    for i in 0..24 {
        let offset = i * 5;
        let to = ((a & mask) >> offset) * 5;
        r = r | (((b & mask) >> offset) << to);
        mask = mask << 5;
    }
    r
}

fn cycle(a: i128, c: &Vec<u8>) -> i128 {
    let mut r = 0;
    let mut cycle_mask = 0;
    let mask = 31;
    let mut offset = c[c.len() - 1] * 5;
    let mut v = (a & (mask << offset)) >> offset;
    for i in c {
        offset = i * 5;
        let m = mask << offset;
        r = r | (v << offset);
        cycle_mask = cycle_mask | m;
        v = (a & m) >> offset;
    }
    r | (!cycle_mask & a)
}

fn cycle_cycles(a: i128, cs: &Vec<Vec<u8>>) -> i128 {
    let mut r = 0;
    let mut cycle_mask = 0;
    let mask = 31;
    for c in cs {
        let mut offset = c[c.len() - 1] * 5;
        let mut v = (a & (mask << offset)) >> offset;
        for i in c {
            offset = i * 5;
            let m = mask << offset;
            r = r | (v << offset);
            cycle_mask = cycle_mask | m;
            v = (a & m) >> offset;
        }
    }
    r | (!cycle_mask & a)
}

fn permute_inv(a: i128, b: i128) -> i128 {
    let base_mask = 31;
    let mut mask_1 = base_mask;
    let mut r = 0;
    for i in 0..24 {
        let offset = i * 5;
        let from = ((b & mask_1) >> offset) * 5;
        let mask_2 = base_mask << from;
        r = r | (((a & mask_2) >> from) << offset);
        mask_1 = mask_1 << 5;
    }
    r
}

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

fn gen_next_moves<F: Sync + Fn(&FaceletCube) -> FaceletCube>(
    reduce_symmetry: F,
    turns: &[FaceletCube; 12],
    parent: &HashSet<FaceletCube>,
    grandparent: &HashSet<FaceletCube>,
) -> HashSet<FaceletCube> {
    let hsm: Mutex<HashSet<FaceletCube>> =
        Mutex::new(HashSet::with_capacity(parent.len() * 12));
    let iter_m = Mutex::new(parent.iter());

    thread::scope(|s| {
        for _ in 0..8 {
            s.spawn(|_| {
                loop {
                    let mut guard = iter_m.lock().unwrap();
                    let perm_o = (*guard).next();
                    drop(guard);
                    match perm_o {
                        Some(perm) => {
                            turns.iter().for_each(|turn| {
                                let ge = reduce_symmetry(&permute_cube(*perm, *turn));
                                if !grandparent.contains(&ge) && !parent.contains(&ge) {
                                    let mut guard = hsm.lock().unwrap();
                                    (*guard).insert(ge);
                                }
                            });
                        },
                        None => break,
                    };
                }
            });
        }
    }).unwrap();
    hsm.into_inner().unwrap()
}

fn main() {
    const CLEAN_ARR: [u8; 24] = [
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23
    ];
    const CLEAN_CUBE: FaceletCube = FaceletCube {
        edges: CLEAN_ARR,
        corners: CLEAN_ARR,
    };
    use Facelet::*;

    // Create a Clockwise turn of the U face
    let mut e = CLEAN_ARR.clone();
    e[U0 as usize] = U1 as u8;
    e[U1 as usize] = U2 as u8;
    e[U2 as usize] = U3 as u8;
    e[U3 as usize] = U0 as u8;
    e[F1 as usize] = R1 as u8;
    e[R1 as usize] = B1 as u8;
    e[B1 as usize] = L1 as u8;
    e[L1 as usize] = F1 as u8;

    let mut c = CLEAN_ARR.clone();
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
    let mut e = CLEAN_ARR.clone();
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

    let mut c = CLEAN_ARR.clone();
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
    let mut e = CLEAN_ARR.clone();
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

    let mut c = CLEAN_ARR.clone();
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
    let mut e = CLEAN_ARR.clone();
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

    let mut c = CLEAN_ARR.clone();
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
    let mut e = CLEAN_ARR.clone();
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

    let mut c = CLEAN_ARR.clone();
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

    let mut syms = [CLEAN_CUBE; 48];
    for i in 0..48 {
        let mut x = i;
        let urfs = x % 3;
        let fs = i / 3 % 2;
        let us = i / 6 % 4;
        let ms = i / 24;

        let mut c = syms[i];
        for _ in 0..urfs {
            c = permute_cube(c, s_urf);
        }
        for _ in 0..fs {
            c = permute_cube(c, s_f);
        }
        for _ in 0..us {
            c = permute_cube(c, s_u);
        }
        for _ in 0..ms {
            c = permute_cube(c, s_mrl);
        }
        syms[i] = c;
    }

    let mut syms_inv = [CLEAN_CUBE; 48];
    for i in 0..48 {
        syms_inv[i] = permute_cube_inv(CLEAN_CUBE, syms[i]);
    }

    let f = permute_cube(permute_cube(syms_inv[2], u), syms[2]);
    let r = permute_cube(permute_cube(syms_inv[1], u), syms[1]);
    let b = permute_cube(permute_cube(syms_inv[19], u), syms[19]);
    let l = permute_cube(permute_cube(syms_inv[4], u), syms[4]);
    let d = permute_cube(permute_cube(syms_inv[3], u), syms[3]);

    let turns = [
        u,
        permute_cube_inv(CLEAN_CUBE, u),
        f,
        permute_cube_inv(CLEAN_CUBE, f),
        r,
        permute_cube_inv(CLEAN_CUBE, r),
        b,
        permute_cube_inv(CLEAN_CUBE, b),
        l,
        permute_cube_inv(CLEAN_CUBE, l),
        d,
        permute_cube_inv(CLEAN_CUBE, d),
    ];
    /*
    println!("turns:");
    for turn in &turns {
        println!("{}", turn);
    }
    */

    let neg_one: HashSet<FaceletCube> = HashSet::new();
    let mut zero: HashSet<FaceletCube> = HashSet::new();
    zero.insert(CLEAN_CUBE);
    let zero = zero;

    let reduce_syms = |perm: &FaceletCube| {
        greatest_equivalence(&syms, &syms_inv, *perm)
    };

    let one = gen_next_moves(&reduce_syms, &turns, &zero, &neg_one);
    let two = gen_next_moves(&reduce_syms, &turns, &one, &zero);
    let three = gen_next_moves(&reduce_syms, &turns, &two, &one);
    let four = gen_next_moves(&reduce_syms, &turns, &three, &two);
    let five = gen_next_moves(&reduce_syms, &turns, &four, &three);
    let six = gen_next_moves(&reduce_syms, &turns, &five, &four);
    let seven = gen_next_moves(&reduce_syms, &turns, &six, &five);
    println!("unique 7: {}", seven.len());
    let eight = gen_next_moves(&reduce_syms, &turns, &seven, &six);
    println!("unique 8: {}", eight.len());
    let nine = gen_next_moves(&reduce_syms, &turns, &eight, &six);
    println!("unique 9: {}", nine.len());
    /*
    let ten = gen_next_moves(&reduce_syms, &turns, &nine, &eight);
    println!("unique 10: {}", ten.len());
    */
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;
    const CLEAN: i128 =    0b00000000_10111_10110_10101_10100_10011_10010_10001_10000_01111_01110_01101_01100_01011_01010_01001_01000_00111_00110_00101_00100_00011_00010_00001_00000;
    const ONE_ZERO: i128 = 0b00000000_10111_10110_10101_10100_10011_10010_10001_10000_01111_01110_01101_01100_01011_01010_01001_01000_00111_00110_00101_00100_00011_00010_00000_00001;
    const LOTS: i128 =     0b00000000_10100_10101_10111_10000_01100_10001_10011_00110_00101_01111_01101_01000_01011_01110_00000_01001_00001_00010_01010_10010_00011_00111_00100_10110;

    const CLEAN_ARR: [u8; 24] = [
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23
    ];
    const ONE_ZERO_ARR: [u8; 24] = [
        1, 0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23
    ];
    const LOTS_ARR: [u8; 24] = [
        22, 4, 7, 3, 18, 10, 2, 1, 9, 0, 14, 11, 8, 13, 15, 5, 6, 19, 17, 12, 16, 23, 21, 20
    ];
    const LOTS_ARR_INV: [u8; 24] = [
        9, 7, 6, 3, 1, 15, 16, 2, 12, 8, 5, 11, 19, 13, 10, 14, 20, 18, 4, 17, 23, 22, 0, 21
    ];

    #[test]
    fn permuting_identity_with_identity_is_identity() {
        assert_eq!(CLEAN, permute(CLEAN, CLEAN));
        assert_eq!(CLEAN_ARR, permute_arr(CLEAN_ARR, CLEAN_ARR));
    }

    #[test]
    fn permuting_identity_with_a_perm_is_that_same_perm() {
        assert_eq!(LOTS, permute(CLEAN, LOTS));
        assert_eq!(LOTS_ARR, permute_arr(CLEAN_ARR, LOTS_ARR));
    }

    #[test]
    fn permuting_identity_with_an_inv_perm_is_that_inverted_perm() {
        // TODO, this as written shouldn't pass but does:
        // assert_eq!(LOTS, permute_inv(CLEAN, LOTS));
        assert_eq!(LOTS_ARR_INV, permute_arr_inv(CLEAN_ARR, LOTS_ARR));
    }

    #[test]
    fn applying_a_perm_and_then_its_inverse_or_vice_versa_is_the_original_perm() {
        /*
         * TODO:
        assert_eq!(LOTS, permute_inv(permute(LOTS, ONE_ZERO), ONE_ZERO));
        assert_eq!(LOTS, permute(permute_inv(LOTS, ONE_ZERO), ONE_ZERO));
        assert_eq!(CLEAN, permute_inv(permute(CLEAN, LOTS), LOTS));
        assert_eq!(CLEAN, permute(permute_inv(CLEAN, LOTS), LOTS));
        */
        assert_eq!(
            LOTS_ARR,
            permute_arr_inv(permute_arr(LOTS_ARR, ONE_ZERO_ARR), ONE_ZERO_ARR)
        );
        assert_eq!(
            LOTS_ARR,
            permute_arr(permute_arr_inv(LOTS_ARR, ONE_ZERO_ARR), ONE_ZERO_ARR)
        );
        assert_eq!(
            CLEAN_ARR,
            permute_arr_inv(permute_arr(CLEAN_ARR, LOTS_ARR), LOTS_ARR)
        );
        assert_eq!(
            CLEAN_ARR,
            permute_arr(permute_arr_inv(CLEAN_ARR, LOTS_ARR), LOTS_ARR)
        );
    }

    #[test]
    fn applying_a_cycle_and_then_its_inverse_is_the_original_permutation() {
        assert_eq!(
            CLEAN,
            cycle(cycle(CLEAN, &vec![0, 1, 5, 17, 4]), &vec![4, 17, 5, 1, 0])
        );
    }

    #[test]
    fn applying_a_cycle_group_and_then_its_inverse_is_the_original_permutation() {
        assert_eq!(
            CLEAN,
            cycle_cycles(
                cycle_cycles(CLEAN, &vec![vec![0, 1, 5, 17, 4], vec![16, 2, 19, 23, 6]]),
                &vec![vec![4, 17, 5, 1, 0], vec![6, 23, 19, 2, 16]]
            )
        );
    }

    #[bench]
    fn repeatedly_perform_1000_turns_via_complex_permutation(b: &mut Bencher) {
        let mut cleanc = CLEAN;
        b.iter(|| {
            for _ in 0..2_000 {
                cleanc = permute(cleanc, LOTS)
            }
            cleanc
        });
    }

    #[bench]
    fn repeatedly_perform_1000_turns_via_complex_arr_permutation(b: &mut Bencher) {
        let mut cleanc = CLEAN_ARR;
        b.iter(|| {
            for _ in 0..2_000 {
                cleanc = permute_arr(cleanc, LOTS_ARR)
            }
            cleanc
        });
    }

    #[bench]
    fn repeatedly_perform_1000_inv_turns_via_complex_arr_permutation(b: &mut Bencher) {
        let mut cleanc = CLEAN_ARR;
        b.iter(|| {
            for _ in 0..2_000 {
                cleanc = permute_arr_inv(cleanc, LOTS_ARR)
            }
            cleanc
        });
    }

    #[bench]
    fn repeatedly_perform_1000_turns_via_individualy_specified_cycles(b: &mut Bencher) {
        let mut cleanc = CLEAN;
        let mut cleane = CLEAN;
        b.iter(|| {
            for _ in 0..500 {
                //U
                cleanc = cycle(cleanc, &vec![0, 1, 2, 3]);
                cleanc = cycle(cleanc, &vec![4, 6, 8, 10]);
                cleanc = cycle(cleanc, &vec![5, 7, 9, 11]);
                cleane = cycle(cleane, &vec![0, 1, 2, 3]);
                cleane = cycle(cleane, &vec![4, 5, 6, 7]);

                //Mirror L to R
                cleanc = cycle(cleanc, &vec![0, 1]);
                cleanc = cycle(cleanc, &vec![2, 3]);
                cleanc = cycle(cleanc, &vec![4, 5]);
                cleanc = cycle(cleanc, &vec![6, 11]);
                cleanc = cycle(cleanc, &vec![7, 10]);
                cleanc = cycle(cleanc, &vec![8, 9]);
                cleanc = cycle(cleanc, &vec![12, 13]);
                cleanc = cycle(cleanc, &vec![14, 19]);
                cleanc = cycle(cleanc, &vec![15, 18]);
                cleanc = cycle(cleanc, &vec![16, 17]);
                cleanc = cycle(cleanc, &vec![20, 21]);
                cleanc = cycle(cleanc, &vec![22, 23]);
                cleane = cycle(cleane, &vec![1, 3]);
                cleane = cycle(cleane, &vec![5, 7]);
                cleane = cycle(cleane, &vec![8, 9]);
                cleane = cycle(cleane, &vec![10, 15]);
                cleane = cycle(cleane, &vec![11, 14]);
                cleane = cycle(cleane, &vec![12, 13]);
                cleane = cycle(cleane, &vec![17, 19]);
                cleane = cycle(cleane, &vec![21, 23]);
            }
            (cleanc, cleane)
        });
    }

    #[bench]
    fn repeatedly_perform_1000_turns_via_cycle_cycles(b: &mut Bencher) {
        let v1 = vec![
            vec![0, 1],
            vec![2, 3],
            vec![4, 5],
            vec![6, 11],
            vec![7, 10],
            vec![8, 9],
            vec![12, 13],
            vec![14, 19],
            vec![15, 18],
            vec![16, 17],
            vec![20, 21],
            vec![22, 23],
        ];
        let v2 = vec![
            vec![1, 3],
            vec![5, 7],
            vec![8, 9],
            vec![10, 15],
            vec![11, 14],
            vec![12, 13],
            vec![17, 19],
            vec![21, 23],
        ];

        let mut cleanc = CLEAN;
        let mut cleane = CLEAN;
        b.iter(|| {
            for _ in 0..500 {
                //U
                cleanc = cycle_cycles(
                    cleanc,
                    &vec![vec![0, 1, 2, 3], vec![4, 6, 8, 10], vec![5, 7, 9, 11]],
                );
                cleane = cycle_cycles(cleane, &vec![vec![0, 1, 2, 3], vec![4, 5, 6, 7]]);

                //Mirror L to R
                cleanc = cycle_cycles(cleanc, &v1);
                cleane = cycle_cycles(cleane, &v2);
            }
            (cleanc, cleane)
        });
    }
}
