use permutation_group::PermutationGroup as PG;
use invertable::Invertable;
use equivalence_class::EquivalenceClass;
use representative_table::{ RepresentativeTable, RepIndex };

use std::sync::Arc;
use std::convert::TryFrom;
use enum_iterator::{ Sequence, all, cardinality };
use flat_move_table::MoveTable;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct CompositeIndex<RepIndexA, PermIndexB>(RepIndexA, PermIndexB);

impl<RepIndexA: Into<usize>, PermIndexB: Into<usize> + Sequence> Into<usize> for CompositeIndex<RepIndexA, PermIndexB> {
    fn into(self) -> usize {
        self.0.into() * cardinality::<PermIndexB>() + self.1.into()
    }
}

#[derive(Debug)]
// TODO: If MoveTable had a trait, then this could compose with itself, meaning
// we could get three+ move tables.
// TODO: Ordering of arguments is stupid
pub struct CompositeMoveTable<Sym, PermIndexA, PermIndexB, Turn> {
    a: Arc<MoveTable<Sym, PermIndexA, Turn>>,
    b: Arc<MoveTable<Sym, PermIndexB, Turn>>,
}

impl<Turn: Sequence + Copy + PartialEq + Into<usize> + EquivalenceClass<Sym>, Sym: Sequence + Copy + Clone + Into<usize> + Invertable + PG, PermIndexA: Sequence + Copy + Ord + TryFrom<usize> + Into<usize>, PermIndexB: Sequence + Copy + Ord + TryFrom<usize> + Into<usize>> CompositeMoveTable<Sym, PermIndexA, PermIndexB, Turn> where <PermIndexA as TryFrom<usize>>::Error: std::fmt::Debug, <PermIndexB as TryFrom<usize>>::Error: std::fmt::Debug {
    pub fn new(a: Arc<MoveTable<Sym, PermIndexA, Turn>>, b: Arc<MoveTable<Sym, PermIndexB, Turn>>) -> Self {
        CompositeMoveTable { a, b }
    }

    /* Some algebra.  The two sym-indexes be nested within one another. And we
     * found our sym-indexes from a raw-indexes like:
     *
     * S' * Xraw * S = Xrep
     *
     * So if we apply a turn to our incoming rep and raw index:
     *
     * Arep0 * T * Braw0 * T
     *
     * We can apply the turn directly to Arep0 to get Arep1, but we need to
     * turn our raw coordinate for B into a sym coordinate so we can actually
     * use our move table.
     *
     * Sa * Arep1 * Sa' * Sb0 * Brep0 * Sb0' * T
     *
     * Now, we can't directly apply T to Brep0, because it's been translated by
     * symmetry, we need to translate in the same way.  We want Tx where Tx is:
     *
     * T = Sb0 * Tx * Sb0'
     *
     * So:
     *
     * Tx = Sb0' * T * Sb0
     *
     * So Tx is just T.get_equivalent(Sb0).  We replace T now to get:
     *
     * Sa * Arep1 * Sa' * Sb0 * Brep0 * Sb0' * Sb0 * Tx * Sb0'
     *
     * And we can see that this lets symmetries cancel, so we can now apply Tx
     * to Brep0.
     *
     * Sa * Arep1 * Sa' * Sb0 * Brep0 * Tx * Sb0'
     *
     * Doing so creates a new rep with new symmetries:
     *
     * Sa * Arep1 * Sa' * Sb0 * Sb1 * Brep1 * Sb1' * Sb0'
     *
     * Lastly, we need to put everything inside Sa, and we do so by tacking on
     * the identity at the end in the form of Sa * Sa':
     *
     * Sa * Arep1 * Sa' * Sb0 * Sb1 * Brep1 * Sb1' * Sb0' * Sa * Sa'
     *
     * Now with some parens (which have no affect, since operations are
     * associative), we can see that we have the right strucuture:
     *
     * Sa * (Arep1 * ((Sa' * Sb0 * Sb1) * Brep1 * (Sb1' * Sb0' * Sa))) * Sa'
     *
     * Finally we can recover a raw coordinate, Bx:
     *
     * Bx = (Sa' * Sb0 * Sb1) * Brep1 * (Sb1' * Sb0' * Sa)
     *
     * (Sb1' * Sb0' * Sa) * Bx * (Sa' * Sb0 * Sb1) = Brep1
     */
    pub fn turn(&self, i: CompositeIndex<RepIndex<Sym, PermIndexA>, PermIndexB>, t: Turn) -> (CompositeIndex<RepIndex<Sym, PermIndexA>, PermIndexB>, Sym) {
        let (a_rep_1, s_a) = self.a.turn(i.0, t);
        let a_raw = self.a.sym_index_to_raw_index((a_rep_1, s_a));

        // If the algebra wasn't bad enough already, we also need to ensure
        // that we pick the symmetry that creates a global minimum--not just
        // the local minimum on the first index we get from s_a.  See more
        // details about this in `raw_index_to_sym_index`.
        let syms =
            if self.a.is_self_symmetric(a_rep_1) {
                itertools::Either::Left(
                    all::<Sym>().filter(|x| self.a.sym_index_to_raw_index((a_rep_1, *x)) == a_raw))
            } else {
                itertools::Either::Right(std::iter::once(s_a))
            };

        let mut smallest = PermIndexB::last().expect("PermIndexB has no members, but there must be at least an identity.");
        let mut sym = Sym::first().expect("Sym has no members, but there must be at least an identity.");

        for s in syms {
            let (b_rep_0, sb_0) = self.b.raw_index_to_sym_index(i.1);
            let (b_rep_1, sb_1) = self.b.turn(b_rep_0, t.get_equivalent(&sb_0));
            let b_raw_rep = self.b.sym_index_to_raw_index((b_rep_1, s.invert().permute(sb_0).permute(sb_1)));
            if b_raw_rep <= smallest {
                smallest = b_raw_rep;
                sym = s;
            }
        }

        (CompositeIndex(a_rep_1, smallest), sym)
    }

    pub fn len(&self) -> usize {
        self.a.len() * cardinality::<PermIndexB>()
    }

    /*
     * s_a' * pi.0 * s_a = a_rep
     *
     * s_b' * pi.1 * s_b = b_rep
     *
     * pi.0 * pi.1 = s_a * a_rep * s_a' * s_b * b_rep * s_b'
     *             = s_a * a_rep * s_a' * s_b * b_rep * s_b' * s_a * s_a'
     *
     * b_raw = s_a' * s_b * b_rep * s_b' * s_a
     *
     * s_b' * s_a * b_raw * s_a' * s_b = b_rep
     */
    pub fn raw_index_to_sym_index(&self, pi: (PermIndexA, PermIndexB)) -> (CompositeIndex<RepIndex<Sym, PermIndexA>, PermIndexB>, Sym) {
        let (a_rep, s_a) = self.a.raw_index_to_sym_index(pi.0);

        // Self-symmetric states cause us an additional headache here.  If
        // there is only one symmetry that reducees the first index, then that
        // local maximum must be the global maximum for the two indexes.
        // However, for self-symmetry, the default symmetry to reduce the first
        // index may not be the symmetry that finds a global minimum, because
        // there were multiple symmetries that could do it.  It was chosen
        // arbitrarily from those options.  This is an issue, because it means
        // that eqivalent paths won't always find the same composite state.
        // Minimizing the index is done to get consistent positions.  We need
        // to consider all symmetries that would locally minimize our first
        // index, and pick one that also minimize the second index too.
        let syms =
            if self.a.is_self_symmetric(a_rep) {
                itertools::Either::Left(
                    all::<Sym>().filter(|x| self.a.sym_index_to_raw_index((a_rep, *x)) == pi.0))
            } else {
                itertools::Either::Right(std::iter::once(s_a))
            };

        let mut smallest = PermIndexB::last().expect("PermIndexB has no members, but there must be at least an identity.");
        let mut sym = Sym::first().expect("Sym has no members, but there must be at least an identity.");

        for s in syms {
            let (b_rep, s_b) = self.b.raw_index_to_sym_index(pi.1);
            let b_raw = self.b.sym_index_to_raw_index((b_rep, s.invert().permute(s_b)));
            if b_raw <= smallest {
                smallest = b_raw;
                sym = s;
            }
        }

        (CompositeIndex(a_rep, smallest), sym)
    }

    /*
     * s_a' * a_raw * b_raw * s_a = a_rep * b_rep_raw
     *
     * a_raw * b_raw = s_a * a_rep * b_rep_raw * s_a'
     *               = s_a * a_rep * s_a' * s_a * b_rep_raw * s_a'
     *
     * b_raw = s_a * b_rep_raw * s_a'
     *
     * s_b' * b_rep_raw * s_b = b_rep
     *
     * b_rep_raw = s_b * b_rep * s_b'
     *
     * b_raw = s_a * s_b * b_rep * s_b' * s_a'
     *
     * s_b' * s_a' * b_raw * s_a * s_b = b_rep
     */
    pub fn sym_index_to_raw_index(&self, (CompositeIndex(a_rep, b_rep_raw), s_a): (CompositeIndex<RepIndex<Sym, PermIndexA>, PermIndexB>, Sym)) -> (PermIndexA, PermIndexB) {
        let a_raw = self.a.sym_index_to_raw_index((a_rep, s_a));
        let (b_rep, s_b) = self.b.raw_index_to_sym_index(b_rep_raw);
        let b_raw = self.b.sym_index_to_raw_index((b_rep, s_a.permute(s_b)));
        (a_raw, b_raw)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use three_triangles;
    use three_triangles_stack::*;
 
    #[test]
    fn composite_move_table_is_correct_for_double_three_triangles_even_parity_with_full_symmetry() {
        // NOTE: An exact double stack is equivalent to a single puzzle, the
        // pieces always move together.  So this test isn't very interesting,
        // but a decent sanity check.

        let rep_table = Arc::new(RepresentativeTable::new::<three_triangles::ThreeTriangles>());
        let tt_move_table: Arc<MoveTable<three_triangles::FullSymmetry, three_triangles::ThreeTrianglesEvenIndex, three_triangles::Turns>> = Arc::new(MoveTable::new::<three_triangles::ThreeTriangles>(rep_table.clone()));

        let move_table = CompositeMoveTable::new(tt_move_table.clone(), tt_move_table.clone());

        // Applying move_table moves is identical to applying permutations
        for top_rep_index in rep_table.rep_indexes() {
            let top_perm: three_triangles::ThreeTriangles = rep_table.rep_index_to_perm_index(top_rep_index).into();
            for bottom_perm_index in all::<three_triangles::ThreeTrianglesEvenIndex>() {
                let bottom_perm: three_triangles::ThreeTriangles = bottom_perm_index.into();
                for t in all::<three_triangles::Turns>() {
                    let top_by_perm = top_perm.permute(t.into());
                    let bottom_by_perm = bottom_perm.permute(t.into());
                    let (top_by_table, bottom_by_table) = move_table.sym_index_to_raw_index(move_table.turn(CompositeIndex(top_rep_index, bottom_perm_index), t));
                    assert_eq!(top_by_perm, top_by_table.into());
                    assert_eq!(bottom_by_perm, bottom_by_table.into());
                }
            }
        }

        // Raw to sym and sym to raw functions round trip
        for pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            let (pi_rt_0, pi_rt_1) = move_table.sym_index_to_raw_index(move_table.raw_index_to_sym_index((pi, pi)));
            assert_eq!(pi_rt_0, pi);
            assert_eq!(pi_rt_1, pi);
        }

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        for pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            for t in all::<three_triangles::Turns>() {
                // NOTE: Again, the only reachable states are where the top and
                // bottom are in the same state.
                let (ri_a, _) = move_table.raw_index_to_sym_index((pi, pi));
                let (ri_b, _) = move_table.turn(ri_a, t);
                let mut found = false;
                for t in all::<three_triangles::Turns>() {
                    let (ri_rt, _ ) = move_table.turn(ri_b, t);
                    if ri_a == ri_rt {
                        found = true;
                        break;
                    }
                }
                assert_eq!(found, true);
            }
        }
    }

    #[test]
    fn composite_move_table_is_correct_for_three_triangles_stack_even_parity_with_no_symmetry() {
        // TODO: Ideally these use the same move table!
        let top_rep_table = Arc::new(RepresentativeTable::new::<TopThreeTriangles>());
        let top_move_table: MoveTable<NoSymmetry, three_triangles::ThreeTrianglesEvenIndex, Turns> = MoveTable::new::<TopThreeTriangles>(top_rep_table.clone());

        let bottom_rep_table = Arc::new(RepresentativeTable::new::<BottomThreeTriangles>());
        let bottom_move_table: MoveTable<NoSymmetry, three_triangles::ThreeTrianglesEvenIndex, Turns> = MoveTable::new::<BottomThreeTriangles>(bottom_rep_table.clone());
        let move_table = CompositeMoveTable::new(Arc::new(top_move_table), Arc::new(bottom_move_table));

        // Raw to sym and sym to raw functions round trip
        for top_perm_index in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            for bottom_perm_index in all::<three_triangles::ThreeTrianglesEvenIndex>() {
                let (top_perm_index_rt, bottom_perm_index_rt) = move_table.sym_index_to_raw_index(move_table.raw_index_to_sym_index((top_perm_index, bottom_perm_index)));
                assert_eq!(top_perm_index_rt, top_perm_index);
                assert_eq!(bottom_perm_index_rt, bottom_perm_index);
            }
        }

        // Applying move_table moves is identical to applying permutations
        for top_rep_index in top_rep_table.rep_indexes() {
            let top_perm: TopThreeTriangles = top_rep_table.rep_index_to_perm_index(top_rep_index).into();
            for bottom_perm_index in all::<three_triangles::ThreeTrianglesEvenIndex>() {
                let bottom_perm: BottomThreeTriangles = bottom_perm_index.into();
                for t in all::<Turns>() {
                    let top_by_perm = top_perm.permute(t.into());
                    let bottom_by_perm = bottom_perm.permute(t.into());
                    let (top_by_table, bottom_by_table) = move_table.sym_index_to_raw_index(move_table.turn(CompositeIndex(top_rep_index, bottom_perm_index), t));
                    assert_eq!(top_by_perm, top_by_table.into());
                    assert_eq!(bottom_by_perm, bottom_by_table.into());
                }
            }
        }

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        for pi0 in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            for pi1 in all::<three_triangles::ThreeTrianglesEvenIndex>() {
                for t in all::<Turns>() {
                    let (ri_a, _) = move_table.raw_index_to_sym_index((pi0, pi1));
                    let (ri_b, _) = move_table.turn(ri_a, t);
                    let mut found = false;
                    for t in all::<Turns>() {
                        let (ri_rt, _) = move_table.turn(ri_b, t);
                        if ri_a == ri_rt {
                            found = true;
                            break;
                        }
                    }
                    assert_eq!(found, true);
                }
            }
        }
    }

    #[test]
    fn composite_move_table_is_correct_for_three_triangles_stack_even_parity_with_mirror_ud_symmetry() {
        // TODO: Ideally these use the same move table!
        let top_rep_table = Arc::new(RepresentativeTable::new::<TopThreeTriangles>());
        let top_move_table: MoveTable<MirrorUDSymmetry, three_triangles::ThreeTrianglesEvenIndex, Turns> = MoveTable::new::<TopThreeTriangles>(top_rep_table.clone());

        let bottom_rep_table = Arc::new(RepresentativeTable::new::<BottomThreeTriangles>());
        let bottom_move_table: MoveTable<MirrorUDSymmetry, three_triangles::ThreeTrianglesEvenIndex, Turns> = MoveTable::new::<BottomThreeTriangles>(bottom_rep_table.clone());
        let move_table = CompositeMoveTable::new(Arc::new(top_move_table), Arc::new(bottom_move_table));

        // Raw to sym and sym to raw functions round trip
        for top_perm_index in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            for bottom_perm_index in all::<three_triangles::ThreeTrianglesEvenIndex>() {
                let (top_perm_index_rt, bottom_perm_index_rt) = move_table.sym_index_to_raw_index(move_table.raw_index_to_sym_index((top_perm_index, bottom_perm_index)));
                assert_eq!(top_perm_index_rt, top_perm_index);
                assert_eq!(bottom_perm_index_rt, bottom_perm_index);
            }
        }

        // Applying move_table moves is identical to applying permutations
        for top_rep_index in top_rep_table.rep_indexes() {
            let top_perm: TopThreeTriangles = top_rep_table.rep_index_to_perm_index(top_rep_index).into();
            for bottom_perm_index in all::<three_triangles::ThreeTrianglesEvenIndex>() {
                let bottom_perm: BottomThreeTriangles = bottom_perm_index.into();
                for t in all::<Turns>() {
                    let top_by_perm = top_perm.permute(t.into());
                    let bottom_by_perm = bottom_perm.permute(t.into());
                    let (top_by_table, bottom_by_table) = move_table.sym_index_to_raw_index(move_table.turn(CompositeIndex(top_rep_index, bottom_perm_index), t));
                    assert_eq!(top_by_perm, top_by_table.into());
                    assert_eq!(bottom_by_perm, bottom_by_table.into());
                }
            }
        }

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        // TODO: This was actually very valuable to test that symmetry
        // reduction always finds the same exact rep, which is far from
        // trivial.  But ideally we'd test for that direcly.
        for pi0 in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            for pi1 in all::<three_triangles::ThreeTrianglesEvenIndex>() {
                for t in all::<Turns>() {
                    let (ri_a, _) = move_table.raw_index_to_sym_index((pi0, pi1));
                    let (ri_b, _) = move_table.turn(ri_a, t);
                    let mut found = false;
                    for t in all::<Turns>() {
                        let (ri_rt, _) = move_table.turn(ri_b, t);
                        if ri_a == ri_rt {
                            found = true;
                            break;
                        }
                    }
                    assert_eq!(found, true);
                }
            }
        }
    }

    #[test]
    fn composite_move_table_is_correct_for_three_triangles_stack_even_parity_with_rotational_symmetry() {
        // TODO: Ideally these use the same move table!
        let top_rep_table = Arc::new(RepresentativeTable::new::<TopThreeTriangles>());
        let top_move_table: MoveTable<RotationalSymmetry, three_triangles::ThreeTrianglesEvenIndex, Turns> = MoveTable::new::<TopThreeTriangles>(top_rep_table.clone());

        let bottom_rep_table = Arc::new(RepresentativeTable::new::<BottomThreeTriangles>());
        let bottom_move_table: MoveTable<RotationalSymmetry, three_triangles::ThreeTrianglesEvenIndex, Turns> = MoveTable::new::<BottomThreeTriangles>(bottom_rep_table.clone());
        let move_table = CompositeMoveTable::new(Arc::new(top_move_table), Arc::new(bottom_move_table));

        // Raw to sym and sym to raw functions round trip
        for top_perm_index in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            for bottom_perm_index in all::<three_triangles::ThreeTrianglesEvenIndex>() {
                let (top_perm_index_rt, bottom_perm_index_rt) = move_table.sym_index_to_raw_index(move_table.raw_index_to_sym_index((top_perm_index, bottom_perm_index)));
                assert_eq!(top_perm_index_rt, top_perm_index);
                assert_eq!(bottom_perm_index_rt, bottom_perm_index);
            }
        }

        // Applying move_table moves is identical to applying permutations
        for top_rep_index in top_rep_table.rep_indexes() {
            let top_perm: TopThreeTriangles = top_rep_table.rep_index_to_perm_index(top_rep_index).into();
            for bottom_perm_index in all::<three_triangles::ThreeTrianglesEvenIndex>() {
                let bottom_perm: BottomThreeTriangles = bottom_perm_index.into();
                for t in all::<Turns>() {
                    let top_by_perm = top_perm.permute(t.into());
                    let bottom_by_perm = bottom_perm.permute(t.into());
                    let (top_by_table, bottom_by_table) = move_table.sym_index_to_raw_index(move_table.turn(CompositeIndex(top_rep_index, bottom_perm_index), t));
                    assert_eq!(top_by_perm, top_by_table.into());
                    assert_eq!(bottom_by_perm, bottom_by_table.into());
                }
            }
        }

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        for pi0 in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            for pi1 in all::<three_triangles::ThreeTrianglesEvenIndex>() {
                for t in all::<Turns>() {
                    let (ri_a, _) = move_table.raw_index_to_sym_index((pi0, pi1));
                    let (ri_b, _) = move_table.turn(ri_a, t);
                    let mut found = false;
                    for t in all::<Turns>() {
                        let (ri_rt, _) = move_table.turn(ri_b, t);
                        if ri_a == ri_rt {
                            found = true;
                            break;
                        }
                    }
                    assert_eq!(found, true);
                }
            }
        }
    }

    #[test]
    fn composite_move_table_is_correct_for_three_triangles_stack_even_parity_with_full_symmetry() {
        // TODO: Ideally these use the same move table!
        let top_rep_table = Arc::new(RepresentativeTable::new::<TopThreeTriangles>());
        let top_move_table: MoveTable<FullSymmetry, three_triangles::ThreeTrianglesEvenIndex, Turns> = MoveTable::new::<TopThreeTriangles>(top_rep_table.clone());

        let bottom_rep_table = Arc::new(RepresentativeTable::new::<BottomThreeTriangles>());
        let bottom_move_table: MoveTable<FullSymmetry, three_triangles::ThreeTrianglesEvenIndex, Turns> = MoveTable::new::<BottomThreeTriangles>(bottom_rep_table.clone());
        let move_table = CompositeMoveTable::new(Arc::new(top_move_table), Arc::new(bottom_move_table));

        // Raw to sym and sym to raw functions round trip
        for top_perm_index in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            for bottom_perm_index in all::<three_triangles::ThreeTrianglesEvenIndex>() {
                let (top_perm_index_rt, bottom_perm_index_rt) = move_table.sym_index_to_raw_index(move_table.raw_index_to_sym_index((top_perm_index, bottom_perm_index)));
                assert_eq!(top_perm_index_rt, top_perm_index);
                assert_eq!(bottom_perm_index_rt, bottom_perm_index);
            }
        }

        // Applying move_table moves is identical to applying permutations
        for top_rep_index in top_rep_table.rep_indexes() {
            let top_perm: TopThreeTriangles = top_rep_table.rep_index_to_perm_index(top_rep_index).into();
            for bottom_perm_index in all::<three_triangles::ThreeTrianglesEvenIndex>() {
                let bottom_perm: BottomThreeTriangles = bottom_perm_index.into();
                for t in all::<Turns>() {
                    let top_by_perm = top_perm.permute(t.into());
                    let bottom_by_perm = bottom_perm.permute(t.into());
                    let (top_by_table, bottom_by_table) = move_table.sym_index_to_raw_index(move_table.turn(CompositeIndex(top_rep_index, bottom_perm_index), t));
                    assert_eq!(top_by_perm, top_by_table.into());
                    assert_eq!(bottom_by_perm, bottom_by_table.into());
                }
            }
        }

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        for pi0 in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            for pi1 in all::<three_triangles::ThreeTrianglesEvenIndex>() {
                for t in all::<Turns>() {
                    let (ri_a, _) = move_table.raw_index_to_sym_index((pi0, pi1));
                    let (ri_b, _) = move_table.turn(ri_a, t);
                    let mut found = false;
                    for t in all::<Turns>() {
                        let (ri_rt, _) = move_table.turn(ri_b, t);
                        if ri_a == ri_rt {
                            found = true;
                            break;
                        }
                    }
                    assert_eq!(found, true);
                }
            }
        }
    }
}
