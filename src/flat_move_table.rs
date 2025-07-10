use permutation_group::PermutationGroup as PG;
use invertable::Invertable;
use equivalence_class::EquivalenceClass;

use std::collections::HashMap;
use std::hash::Hash;

pub fn new<'a, Perm: Hash + Ord + PG + Clone + EquivalenceClass<Sym> + Into<usize>, Turn: Copy + Into<Perm> + Into<usize>, Sym>(turns: &Vec<Turn>, syms: &'a Vec< Sym>, initial: &Perm) -> Vec<(Perm, Option<&'a Sym>)> {
    let mut representants = HashMap::new();

    // TODO: With capacity hint?
    let mut table = Vec::new();

    let mut queue = Vec::new();
    queue.push(initial.clone());

    loop {
        let next = queue.pop();

        match next {
            None => {
              break;
            }
            Some(c) => {
                for t in turns {
                    let turned: Perm = c.clone().permute(<Turn as Into<Perm>>::into(*t));
                    let (r, s) = greatest_equivalence(&syms, &turned);
                    let i = match representants.get(&r) {
                      None => {
                        representants.insert(r.clone(), representants.len());
                        queue.push(r.clone());
                        representants.len()
                      },
                      Some(i) => *i,
                    };

                    let i: usize = <Turn as Into<usize>>::into(*t) + <Perm as Into<usize>>::into(c.clone()) * turns.len();
                    table[i] = (r, s)
                }
            }
        }

    }
    
    table
}

fn greatest_equivalence<'a, Perm: Ord + PG + Clone + EquivalenceClass<Sym>, Sym>(syms: &'a Vec<Sym>, perm: &Perm) -> (Perm, Option<&'a Sym>) {
    // Identity Sym should not be included in the Sym list (but hypothetically won't hurt anything?)
    let mut sym: Option<&Sym> = None;
    let mut greatest = perm.clone();
    for s in syms {
        // TODO: This clone is pretty wasteful, because we really only need a
        // reference, but get_equivalent takes self not &self, because so do
        // permute and apply.  We can't implement apply on &T, because then
        // we'd have &T -> &T -> &T, and it's not possible to return a
        // reference to nothing.  We also can't do something like BorrowMut<T>
        // -> BorrowMut<T> -> BorrowMut<T>, because we can't return the
        // original mutable reference, because that would make for two of them.
        // It seems to me that the functional crate maybe gets it wrong, for
        // Rust, it should still be &T -> &T -> T, because in functional
        // programming we never mutate anything!  We should only ever need a
        // reference to the first two parameters, but then create something new
        // at the end.
        let e = perm.clone().get_equivalent(&s);
        if e > greatest {
            greatest = e;
            sym = Some(s);
        }
    }
    ( greatest, sym )
}
