use std::collections::HashSet;
use std::hash::Hash;
use std::sync::Mutex;

use permutation_group::PermutationGroup as PG;
use equivalence_class::EquivalenceClass;
use super::util::{n_scoped_workers, while_iter_in_mutex_has_next };

/* A pruning table is a series of n HashSets that store all permutations
 * that can be solved in i-moves.  It is much like a MoveTable but they
 * key difference is that solving a bit slower, but memory usage is slightly
 * reduced, inversion-as-symmetry is not consider, and some key restrictions
 * are removed.
 *
 * When solving with a MoveTable, it can directly solve because it stores
 * a series of "undo" moves, so we can backtrack.  With the PruningTable
 * we simply try applying every move and then looking up the result in the
 * n-1 move HashSet, if it's there we know we're backtracking.  Since we
 * must discover the correct turn, this adds additional operations.
 *
 * Inversion-as-symmetry and some restrictions are removed to make this more
 * suitable for usage for coordinates that may not fully encode the cube
 * state.  Certain symmetries (such as symmetry through piece permutation,
 * such as the UDSlice coord) cannot be represented as turns, so we cannot
 * backtrack through these symmetries.  This means that its not possible
 * to create a MoveTable with these reductions--so a PruningTable is used
 * instead.
 */
#[derive(Debug)]
pub struct PruningTable<Stored: Eq + Hash, Used, Sym, Turn> {
    turns: Vec<Turn>,
    syms: Vec<Sym>,
    table: Vec<HashSet<Stored>>,
    phantom: std::marker::PhantomData<Used>,
}

pub fn new<Stored: Hash + Eq + Ord + Send + Sync + Copy + From<Used>, Used: PG + Sync + Copy + EquivalenceClass<Sym> + From<Stored> + From<Turn>, Sym: Clone + Sync, Turn: Send + Sync + Copy + EquivalenceClass<Sym>>(turns: Vec<Turn>, syms: Vec<Sym>, n: usize) -> PruningTable<Stored, Used, Sym, Turn> {
    let mut table = Vec::with_capacity(n);
    let neg_one: HashSet<Stored> = HashSet::new();
    let mut zero: HashSet<Stored> = HashSet::new();
    zero.insert(Stored::from(PG::identity()));
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

    PruningTable {
        table: table,
        turns: turns,
        syms: syms,
        phantom: std::marker::PhantomData,
    }
}


fn greatest_equivalence<Stored: Ord + From<Used>, Used: PG + Copy + EquivalenceClass<Sym>, Sym: Clone>(syms: &Vec<Sym>, perm: Used) -> Stored {
    // at a minimum, the identity permutation must be included the sym list
    let mut greatest = perm.get_equivalent(&syms[0].clone()).into();
    for s in syms {
        let e = perm.get_equivalent(&s).into();
        if e > greatest {
            greatest = e;
        }
    }
    greatest
}

/* With multiple workers in parallel, we build each HashSet on top of the previous.
 * From any permutation in the previous HashSet, if we apply a turn, the resulting
 * permutation must require one more, one less, or the same number of turns to solve.
 * By iterating over all parent permutations and applying all possible turns, the
 * permutations that are not present in the parent or grandparent HashSets must
 * reqiure one additional turn to solve and belong in our new HashSet.
 *
 * At each step, we must reduce the permutation through symmetry.
 */
fn gen_next_moves<Stored: Hash + Eq + Copy + Send + Sync + Ord + From<Used>, Used: PG + Copy + Sync + EquivalenceClass<Sym> + From<Stored> + From<Turn>, Sym: Sync + Clone, Turn: Sync + Copy>(
    syms: &Vec<Sym>,
    turns: &Vec<Turn>,
    parent: &HashSet<Stored>,
    grandparent: &HashSet<Stored>,
) -> HashSet<Stored> {
    let hsm: Mutex<HashSet<Stored>> =
        Mutex::new(HashSet::new());
    let iter_m = Mutex::new(parent.iter());

    n_scoped_workers(8, || {
        while_iter_in_mutex_has_next(&iter_m, |stored_perm: &Stored| {
            let perm = Used::from(*stored_perm);
            for &turn in turns {
                let pos = perm.permute(Used::from(turn));
                let ge = greatest_equivalence(&syms, pos);
                if !grandparent.contains(&ge) && !parent.contains(&ge) {
                    let mut guard = hsm.lock().unwrap();
                    (*guard).insert(ge);
                }
            }
        });
    });
    hsm.into_inner().unwrap()
}

impl<Stored: Eq + Hash + Ord + From<Used>, Used: PG + Copy + EquivalenceClass<Sym> + From<Turn>, Sym: Clone, Turn: Copy> PruningTable<Stored, Used, Sym, Turn> {
    /* To solve a cube with a PruningTable it's simple to discover how many
     * moves it will take (check each HashSet for the reduced case), but more
     * work to discover the solve.
     *
     * Once we have a permutation and we know it can be solved in n moves,
     * we simply try applying all of them.  As soon as we find one that
     * undoes one move (the reduced result appears in the n-1 HashSet) we
     * add that turn to our solution and repeat the process on the permutation
     * that is one move more solved.
     */
    pub fn solve(&self, scramble: &Used) -> Option<Vec<Turn>> {
        let syms = &self.syms;
        let table = &self.table;
        let turns = &self.turns;
        let scramble_reduced = greatest_equivalence(&syms, *scramble);

        let mut n = 0;
        for hm in table {
            if Option::is_some(&hm.get(&scramble_reduced)) {
                break;
            }
            n += 1;
        }

        if n == table.len() {
            None
        } else {
            let mut solution = Vec::with_capacity(n);
            let mut cube_state = scramble.clone();
            for i in (0..n).rev() {
                let mut turn_iter = turns.into_iter();
                loop {
                    let turn = turn_iter.next().expect("Pruning table is corrupt!").clone();
                    let turn_result: Used = cube_state.permute(Used::from(turn));
                    if table[i].contains(&greatest_equivalence(&syms, turn_result)) {
                        solution.push(turn);
                        cube_state = turn_result;
                        break;
                    }
                }
            }

            Some(solution)
        }
    }
}
