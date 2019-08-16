#![feature(test)]
use std::collections::HashMap;
use std::hash::Hash;
use std::sync::Mutex;

extern crate ansi_term;
extern crate test;
extern crate crossbeam;

use crossbeam::thread;

mod permutation_group;
mod facelet_cube;

use permutation_group::PermutationGroup as PG;
// TODO: do not use arr_identity directly
use facelet_cube::{FaceletCube, arr_identity};

fn greatest_equivalence<T: Ord + PG + Copy>(syms: &[T; 48], perm: T) -> (T, T, bool) {
    let mut greatest = perm;
    let mut sym: T = PG::identity();
    let mut inverted = false;
    for i in 0..48 {
        let e = perm.apply_symmetry(syms[i]);
        if e > greatest {
            greatest = e;
            sym = syms[i];
            inverted = false;
        }
        let e_inv = perm.invert().apply_symmetry(syms[i]);
        if e_inv > greatest {
            greatest = e_inv;
            sym = syms[i];
            inverted = true;
        }
    }
    // TODO: It seems like these would make more sense to be borrow?
    // However, it gets pretty weird because they have to last as long as
    // syms, syms_inv.  However, the user can always copy.
    ( greatest, sym.clone(), inverted )
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

fn n_scoped_workers<F: Sync + Fn() -> ()>(n: usize, f: F) -> () {
    thread::scope(|s| {
        for _ in 0..n {
            s.spawn(|_| f());
        }
    //TODO: Return the result, rather than unwrapped
    }).unwrap()
}

fn while_iter_in_mutex_has_next<I: Iterator, F: Sync + Fn(I::Item) -> ()>(m: &Mutex<I>, f: F) -> () {
    loop {
        // TODO: Propogate the Poisoning
        let mut guard = m.lock().unwrap();
        let perm_o = (*guard).next();
        drop(guard);
        match perm_o {
            // TODO: Allow a return value/result to be collected
            Some(perm) => f(perm),
            None => break,
        };
    }
}

/* Given our previous table entries, we now generate the next.
 * From any position that requires an optimal number of n turns to solve,
 * if we apply another turn, there are only thre possibilities:
 *   a) This solves the cube, and it now takes n - 1 turns to solve
 *   b) This scrambles the cube more, and it now takes n + 1
 *   c) This steps towards a new but _equally_ scramble position, and it still takes n turns
 *
 * As such, we take all of our parent's positions, apply each possible turn,
 * reduce to the cannonical symmetry case, and then check to see if it's in
 * the parent or grandparent.
 *
 * If present in either, it's case a or c, and we can ignore this perm + turn.
 * If it's not, it is a newly discovered position of n+1, and we insert it into
 * the new hashmap.
 *
 * There's quite a bit of work to do here, so we do this with multiple workers,
 * and the HashMap is in a Mutex to stay safe.
 *
 * Lastly, when we insert into the Mutex, the key is the position, and the value
 * is the 'undo' turn.  This allows us to 'backtrack', so we can discover the turns
 * one at a time by stepping through each table entry.
 *
 * The inverse turn that found the position is not necessarily the turn that will
 * 'undo' the newly found position.  This is because the position that we store
 * is the canonical symmetry case--not the one the turn generated to find it.
 *
 * Let's look at some permutation algebra:
 * X is the original permutation
 * T is the turn that generates a new case
 * Y is the new position
 * Yr is the symmetry reduced case
 * so:
 * Y = X * T
 * Yr = Sr' * Y * Sr'
 * Yr = Sr' * X * T * Sr'
 *
 * Now we can find the turn that returns us to X
 * Yr * Sr' = Sr' * X * T * Sr' * Sr
 * Yr * Sr' = Sr' * X * T
 * Yr * Sr' * T' = Sr' * X
 * Yr * Sr' * T' * Sr = Sr' * X * Sr
 *
 * Notice that we don't get something quite so satisfying as:
 * Yr * T' = X
 *
 * This is acceptable for two reasons:
 * 1) Sr' * X * Sr is (by definition) symmetrical to X.
 * Our eventual goal is to find Xr, and any position symmetrical to
 * X (or Xr) can help us do this.
 *
 * 2) Any permutation that is symmetrical to a turn, is also a turn:
 *   \A t \in Turn :
 *     \E s \in Symmetry : s * t * s' \in Turn
 *
 * So now we can find the turn that "undoes" Yr (Tu):
 * Tu = Sr' * T' * Sr
 *
 * And so we write (Yr, Tu) into the HashMap
 *
 *
 * This has one more wrinkle, which is that inversion is a form of symmetry.
 * In this case:
 * Xr * T = Y
 * Sr' Y' Sr = Yr
 * Y' = Sr * Yr * Sr'
 * Y = (Sr * Yr * Sr')'
 * Y = Sr * Yr' * Sr'
 * Xr * T = Sr * Yr' * Sr'
 * Xr = Sr * Yr' * Sr' * T'
 * Xr' = (Sr * Yr' * Sr' * T')'
 * Xr' = T * Sr * Yr * Sr'
 * T = Sr * Tu * Sr'
 * Xr' = Sr * Tu * Sr' * Sr * Yr * Sr'
 * Xr' = Sr * Tu * Yr * Sr'
 * Sr' * Xr' * Sr = Tu * Yr
 * Tu = Sr' * T * Sr
 *
 * So there are two critical differences, the undo move works as a "pre" move.
 * Which means it undoes the permutation by being applied _before_ rather than
 * after.  And the stored move is not inverted before the symmetry is applied.
 */
fn gen_next_moves<T: PG + Hash + Eq + Copy + Send + Sync, F: Sync + Fn(&T) -> (T, T, bool)>(
    reduce_symmetry: F,
    turns: &[T; 12],
    parent: &HashMap<T, (T, bool)>,
    grandparent: &HashMap<T, (T, bool)>,
) -> HashMap<T, (T, bool)> {
    let hsm: Mutex<HashMap<T, (T, bool)>> =
        Mutex::new(HashMap::with_capacity(parent.len() * 12));
    let iter_m = Mutex::new(parent.iter());

    n_scoped_workers(8, || {
        while_iter_in_mutex_has_next(&iter_m, |(perm, _): (&T, &(T, bool))| {
            turns.iter().for_each(|turn| {
                let (ge, sym, was_inverted) = reduce_symmetry(&perm.permute(*turn));
                if grandparent.get(&ge) == None && parent.get(&ge) == None {
                    let undo;
                    if was_inverted {
                        undo = turn.apply_symmetry(sym);
                    } else {
                        undo = turn.invert().apply_symmetry(sym);
                    }
                    let mut guard = hsm.lock().unwrap();
                    (*guard).insert(ge, (undo, was_inverted));
                }
            });
        });
    });
    hsm.into_inner().unwrap()
}

/* Our move table is designed to be memory efficient, and this makes
 * our computation of a 'solve' more complex.
 * It's fairly easy to lookup any given position to see if it's solvable in n turns,
 * but to get those turns takes some work, due to our symmetry reductions.
 *
 * Some cube algebra to help us:
 * X is the permutation we are trying to solve
 * Xr "smallest" permutation that is symmetrical to X
 * so:
 * Xr = Srx' * X * Srx
 *
 * If we find Xr in the HashMap at position n, we know X is solvable in n moves.
 * Tx is the value at HashMap[Xr].  It is the next turn in the solve (for Xr, not X).
 * We call the resulting permuation Y:
 * Y = Xr * Tx
 *
 * However, Y will (likely) not be present in the n-1 HashMap.
 * We must also reduce it first, so:
 * Yr = Sry' * Y * Sry
 * Yr = Sry' * Xr * Tx * Sry
 *
 * This is where we'll see recursion take place (parenthesis for emphasis):
 * Zr = Srz' * (Sry' * Xr * Tx * Sry) * Ty * Srz
 *
 * Eventually, at n=1, the final permutation Ar and its turn Ta, when combine, result
 * in the identity I, which needs no reduction:
 * I = Ar * Ta
 *
 * Assume in the above case, this is where we end at the next move:
 * I = Srz' * (Sry' * Xr * Tx * Sry) * Ty * Srz * Tz
 *
 * We know that:
 * Sa' * X * Y * Sa = Sa' * X * Sa * Sa' * Y * Sa
 *
 * So we can begin to move out and cancel our symmetries:
 * I = Srz' * Sry' * Xr * Tx * Sry * Ty * Srz * Tz
 * I * Srz' = Srz' * Sry' * Xr * Tx * Sry * Ty * Srz * Tz * Srz
 * Srz * I * Srz' = Sry' * Xr * Tx * Sry * Ty * Srz * Tz * Srz
 * Srz * Srz' = Sry' * Xr * Tx * Sry * Ty * Srz * Tz * Srz
 * I = Sry' * Xr * Tx * Sry * Ty * Srz * Tz * Srz
 * Sry * I * Sry' = Sry * Sry' * Xr * Tx * Sry * Ty * Srz * Tz * Srz' * Sry
 * Sry * Sry' = Xr * Tx * Sry * Ty * Srz * Tz * Srz' * Sry
 * I = Xr * Tx * Sry * Ty * Srz * Tz * Srz' * Sry
 *
 * Let's replace Xr to get our goal in the formula:
 * I = Srx' * X * Srx * Tx * Sry * Ty * Srz * Tz * Srz' * Sry
 * Srx * I * Srx' = Srx * Srx' * X * Srx * Tx * Sry * Ty * Srz * Tz * Srz' * Sry' * Srx
 * Srx * Srx' = X * Srx * Tx * Sry * Ty * Srz * Tz * Srz' * Sry' * Srx
 * I = X * Srx * Tx * Sry * Ty * Srz * Tz * Srz' * Sry' * Srx
 * X' * I = X' * X * Srx * Tx * Sry * Ty * Srz * Tz * Srz' * Sry' * Srx
 * X' = Srx * Tx * Sry * Ty * Srz * Tz * Srz' * Sry' * Srx
 *
 * Now we finally have both sides as our target--the inverse of our scramble permutation.
 * However, we don't have it broken down nicely into turns.
 * We can do this by simply inserting some "garbage" symmetries that would cancel out
 * X' = Srx * Tx * Sry * Ty * Srz * Tz * Srz' * Sry' * Srx
 * X' = Srx * Tx * Srx' * Srx * Sry * Ty * Srz * Tz * Srz' * Sry' * Srx
 *
 * Since every permutation that is symmetrical to a turn is also a turn,
 * we've now found T0, and move on to the next:
 * T0 = Srx * Tx * Srx
 * X' = T0 * Srx * Sry * Ty * Srz * Tz * Srz' * Sry' * Srx
 * X' = T0 * Srx * Sry * Ty * Sry' * Sry * Srz * Tz * Srz' * Sry' * Srx
 *
 * Which reveals T1 and T2
 * T1 = Srx * Sry * Ty * Sry' * Srx
 * T2 = Srx * Sry * Srz * Tz * Srz' * Sry' * Srx
 * X' = T0 * T1 * T2
 *
 *
 * Our last consideration is when symmetry of inversion is involved.
 * And within this there are two cases to consider: the undo move is a "pre" move
 * which means it will affect the final order of the solves.
 * Or, the reduction from the "undone" permutation requires inversion.
 *
 * In the first case, the effect is temporary, we push to a left queue whenever
 * it occurs.
 *
 * The second effect cascades.  Every time we have to "invert," this inversion
 * takes affect until we encounter another.  When operating in this inverted mode,
 * we push to the opposite vector that we normally would and invert the position.
 */
// TODO: Use a HashMap<FaceletCube, NamedTurn> since NamedTurn can be an enum
// that would take up significantly less space in memory.
// TODO: Move table references/ownership don't quite add up
fn solve_by_move_table<T: PG + Eq + Hash + Clone + Copy, F: Fn(&T) -> (T, T, bool)>(reduce_symmetry: F, table: Vec<&HashMap<T, (T, bool)>>, scramble: &T) -> Option<Vec<T>> {
    let (scramble_r, s, pb) = reduce_symmetry(scramble);

    let mut n = 0;
    for hm in &table {
        if hm.get(&scramble_r) != None {
            break;
        }
        n += 1;
    }

    let mut right_side = Vec::with_capacity(n);
    let mut left_side = Vec::with_capacity(n);
    let mut r = scramble_r;
    let mut sym = s;
    let mut push_backwards = pb;
    for i in (0..n + 1).rev() {
        let r_clone = r.clone();

        // TODO: Or it is solved in more turns than the table holds
        let (turn, was_inverted) = table[i].get(&r_clone).expect("Move table is corrupt");

        let sym_fixed_turn = turn.apply_symmetry(sym.invert());

        let undone;
        if *was_inverted {
            undone = turn.permute(r_clone);
            if push_backwards {
                right_side.push(sym_fixed_turn.invert());
            } else {
                left_side.push(sym_fixed_turn);
            }
        } else {
            undone = r_clone.permute(*turn);
            if push_backwards {
                left_side.push(sym_fixed_turn.invert());
            } else {
                right_side.push(sym_fixed_turn);
            }
        }

        let (next_r, s, pb) = reduce_symmetry(&undone);
        r = next_r;
        sym = sym.permute(s);
        push_backwards = pb != push_backwards;
    }

    for i in (0..left_side.len()).rev() {
        right_side.push(left_side[i]);
    }
    Some(right_side)
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

    let mut syms = [PG::identity(); 48];
    for i in 0..48 {
        let urfs = i % 3;
        let fs = i / 3 % 2;
        let us = i / 6 % 4;
        let ms = i / 24;

        let mut c: FaceletCube = syms[i];
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
        syms[i] = c;
    }

    let f = u.apply_symmetry(syms[2]);
    let r = u.apply_symmetry(syms[1]);
    let b = u.apply_symmetry(syms[19]);
    let l = u.apply_symmetry(syms[4]);
    let d = u.apply_symmetry(syms[3]);

    let turns = [
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

    let neg_one: HashMap<FaceletCube, (FaceletCube, bool)> = HashMap::new();
    let mut zero: HashMap<FaceletCube, (FaceletCube, bool)> = HashMap::new();
    // Since there is no 'turn' that 'solves' this more, we insert the identity
    zero.insert(PG::identity(), (PG::identity(), false));
    let zero = zero;

    let reduce_syms = |perm: &FaceletCube| {
        greatest_equivalence(&syms, *perm)
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
