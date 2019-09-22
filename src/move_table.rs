use std::collections::HashMap;
use std::hash::Hash;
use std::sync::Mutex;

use permutation_group::PermutationGroup as PG;
use invertable::Invertable;
use equivalence_class::EquivalenceClass;
use super::util::{n_scoped_workers, while_iter_in_mutex_has_next };

#[derive(Debug)]
pub struct MoveTable<Stored: Eq + Hash, Used, Sym, Turn> {
    turns: Vec<Turn>,
    syms: Vec<Sym>,
    table: Vec<HashMap<Stored,(Turn,bool)>>,
    phantom: std::marker::PhantomData<Used>,
}

pub fn new<Stored: Hash + Eq + Send + Sync + Copy + From<Used>, Used: PG + Ord + Sync + Copy + EquivalenceClass<Sym> + From<Stored> + From<Turn>, Sym: Clone + Sync, Turn: Send + Sync + Copy + Invertable + EquivalenceClass<Sym>>(turns: Vec<Turn>, syms: Vec<Sym>, n: usize) -> MoveTable<Stored, Used, Sym, Turn> {
    let mut table = Vec::with_capacity(n);
    let neg_one: HashMap<Stored, (Turn, bool)> = HashMap::new();
    let mut zero: HashMap<Stored, (Turn, bool)> = HashMap::new();
    // Since there is no 'turn' that 'solves' this more, we insert the identity
    // TODO: inserting the first move seems pretty dumb
    zero.insert(Stored::from(PG::identity()), (turns[0], false));
    let zero = zero;
    table.push(zero);

    if n > 0 {
        table.push(gen_next_moves(&syms, &turns, &table[0], &neg_one));
        table[0].shrink_to_fit();
    }

    for i in 2..n {
        table.push(gen_next_moves(&syms, &turns, &table[i - 1], &table[i - 2]));
        table[i].shrink_to_fit();
    }

    MoveTable {
        table: table,
        turns: turns,
        syms: syms,
        phantom: std::marker::PhantomData,
    }
}


fn greatest_equivalence<Perm: Ord + PG + Copy + EquivalenceClass<Sym>, Sym: Clone>(syms: &Vec<Sym>, perm: Perm) -> (Perm, Sym, bool) {
    // at a minimum, the identity permutation must be included the sym list
    let mut sym: Sym = syms[0].clone();
    let mut greatest = perm.get_equivalent(&sym);
    let mut inverted = false;
    for s in syms {
        let e = perm.get_equivalent(&s);
        if e > greatest {
            greatest = e;
            sym = s.clone();
            inverted = false;
        }
        let e_inv = perm.invert().get_equivalent(&s);
        if e_inv > greatest {
            greatest = e_inv;
            sym = s.clone();
            inverted = true;
        }
    }
    // TODO: It seems like these would make more sense to be borrow?
    // However, it gets pretty weird because they have to last as long as
    // syms, syms_inv.  However, the user can always copy.
    ( greatest, sym, inverted )
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
 *
 * As a final wrinkle, when we consider inversion to be symmetry, we will not discover
 * all cases if we always push new moves to the front.  Instead we need to push
 * both to the front and the back (and while that doubles our work, compounding halving our
 * stored states is more than worth this trade off).
 *
 * To see this consiquence in affect, consider we were trying discover unique
 * sequences of bits in the same manor with two symmetry cases, flipping all bits
 * and reversing their order (inversion).  Once we add our fourth bit, we'll see
 * that we miss cases.
 *
 * All possible four bit combos are listed and paired with their reductions to the
 * smallest (big endian) value (if exist):
 * 0000
 * 0001
 * 0010
 * 0100 -> 0010
 * 1000 -> 0001
 * 0011
 * 0101
 * 1001 -> 0110
 * 0110
 * 1010 -> 0101
 * 1100 -> 0011
 * 0111 -> 0001
 * 1011 -> 0010
 * 1101 -> 0010
 * 1110 -> 0001
 * 1111 -> 0000
 *
 * this leaves us with six cases that we want to discover:
 * 0000
 * 0001
 * 0010
 * 0011
 * 0101
 * 0110
 *
 * However, to find these we must start with our ruduced three bit combos:
 * 000
 * 001
 * 010
 * 100 -> 001
 * 011 -> 001
 * 101 -> 010
 * 110 -> 001
 * 111 -> 000
 *
 * We take each of our reductions, add each unique bit, and ruduce again:
 * 0000
 * 0010
 * 0100 -> 0010
 * 0001
 * 0011
 * 0101
 *
 * Notice we've only generated five cases of the six we intended to find!
 * Had we checked "pre-moves" as well, the 0110 case would be found via 1 + 001,
 * which reduces to 0110.
 */
fn gen_next_moves<Stored: Hash + Eq + Copy + Send + Sync + From<Used>, Used: PG + Copy + Sync + Ord + EquivalenceClass<Sym> + From<Stored> + From<Turn>, Sym: Clone + Sync, Turn: Send + Sync + Copy + Invertable + EquivalenceClass<Sym>>(
    syms: &Vec<Sym>,
    turns: &Vec<Turn>,
    parent: &HashMap<Stored, (Turn, bool)>,
    grandparent: &HashMap<Stored, (Turn, bool)>,
) -> HashMap<Stored, (Turn, bool)> {
    let hsm: Mutex<HashMap<Stored, (Turn, bool)>> =
        Mutex::new(HashMap::new());
    let iter_m = Mutex::new(parent.iter());

    n_scoped_workers(8, || {
        while_iter_in_mutex_has_next(&iter_m, |(stored_perm, _): (&Stored, &(Turn, bool))| {
            let perm = Used::from(*stored_perm);
            for &as_premove in &[true, false] {
                for &turn in turns {
                    let pos = if as_premove {
                        Used::from(turn).permute(perm)
                    } else {
                        perm.permute(Used::from(turn))
                    };
                    let (ge, sym, was_inverted) = greatest_equivalence(&syms, pos);
                    if Option::is_none(&grandparent.get(&ge.into())) && Option::is_none(&parent.get(&ge.into())) {
                        let undo = if was_inverted {
                            turn.get_equivalent(&sym)
                        } else {
                            turn.invert().get_equivalent(&sym)
                        };
                        let mut guard = hsm.lock().unwrap();
                        (*guard).insert(ge.into(), (undo, was_inverted != as_premove));
                    }
                }
            }
        });
    });
    hsm.into_inner().unwrap()
}

impl<Stored: Eq + Hash + Copy + From<Used>, Used: PG + Copy + Ord + EquivalenceClass<Sym> + From<Stored> + From<Turn>, Sym: PG + Clone, Turn: Copy + Invertable + EquivalenceClass<Sym>> MoveTable<Stored, Used, Sym, Turn> {
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
     * Srx * I * Srx' = Srx * Srx' * X * Srx * Tx * Sry * Ty * Srz * Tz * Srz' * Sry' * Srx'
     * Srx * Srx' = X * Srx * Tx * Sry * Ty * Srz * Tz * Srz' * Sry' * Srx'
     * I = X * Srx * Tx * Sry * Ty * Srz * Tz * Srz' * Sry' * Srx'
     * X' * I = X' * X * Srx * Tx * Sry * Ty * Srz * Tz * Srz' * Sry' * Srx'
     * X' = Srx * Tx * Sry * Ty * Srz * Tz * Srz' * Sry' * Srx'
     *
     * Now we finally have both sides as our target--the inverse of our scramble permutation.
     * However, we don't have it broken down nicely into turns.
     * We can do this by simply inserting some "garbage" symmetries that would cancel out
     * X' = Srx * Tx * Sry * Ty * Srz * Tz * Srz' * Sry' * Srx'
     * X' = Srx * Tx * Srx' * Srx * Sry * Ty * Srz * Tz * Srz' * Sry' * Srx'
     *
     * Since every permutation that is symmetrical to a turn is also a turn,
     * we've now found T0, and move on to the next:
     * T0 = Srx * Tx * Srx'
     * X' = T0 * Srx * Sry * Ty * Srz * Tz * Srz' * Sry' * Srx'
     * X' = T0 * Srx * Sry * Ty * Sry' * Sry * Srz * Tz * Srz' * Sry' * Srx'
     *
     * Which reveals T1 and T2
     * T1 = Srx * Sry * Ty * Sry' * Srx'
     * T2 = Srx * Sry * Srz * Tz * Srz' * Sry' * Srx'
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
    // TODO: This requires that Sym be a permutation, which is interesting.
    // Theoretically a less efficient procedure would be to make a list of syms that
    // grows with each turn and apply them all to each new move.
    // However, syms certainly need to have an inverse here (and generally this makes sense,
    // if we can find some symmetry S that creates the greatest_equivalence of some perm P,
    // then there must be some inverse symmetry S-1 that creates tho original perm P from
    // that greatest_equivalence.  Notably, finding the greatest_equivalence mandates that
    // the identity be in the list of symmetries!  Ultimately the question is, must Sym be
    // a permutation?
    pub fn solve(&self, scramble: &Used) -> Option<Vec<Turn>> {
        let syms = &self.syms;
        let table = &self.table;
        let (scramble_r, s, pb) = greatest_equivalence(&syms, *scramble);

        let mut n = 0;
        for hm in table {
            if Option::is_some(&hm.get(&scramble_r.into())) {
                break;
            }
            n += 1;
        }

        if n == table.len() {
            None
        } else {
            let mut right_side = Vec::with_capacity(n);
            let mut left_side = Vec::with_capacity(n);
            let mut r = scramble_r;
            let mut sym = s;
            let mut push_backwards = pb;
            for i in (1..n + 1).rev() {
                let r_clone = r.clone();

                let (turn, was_inverted) = table[i].get(&r_clone.into()).expect("Move table is corrupt");

                let sym_fixed_turn = if *was_inverted {
                    turn.get_equivalent(&sym.invert()).invert()
                } else {
                    turn.get_equivalent(&sym.invert())
                };

                if *was_inverted == push_backwards {
                    right_side.push(sym_fixed_turn);
                } else {
                    left_side.push(sym_fixed_turn);
                }

                let undone = if *was_inverted {
                    Used::from(*turn).permute(r_clone)
                } else {
                    r_clone.permute(Used::from(*turn))
                };

                let (next_r, s, pb) = greatest_equivalence(&syms, undone);
                r = next_r;
                sym = sym.permute(s);
                push_backwards = pb != push_backwards;
            }

            for i in (0..left_side.len()).rev() {
                right_side.push(left_side[i].invert());
            }
            Some(right_side)
        }
    }
}
