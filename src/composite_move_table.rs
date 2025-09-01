use permutation_group::PermutationGroup as PG;
use invertable::Invertable;
use equivalence_class::EquivalenceClass;
use representative_table::{ RepresentativeTable, RepIndex };
use table_traits::{ TableTurn, TableSymTurn, TableRawIndexToSymIndex, TableSymIndexToRawIndex, TableRepCount };

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
// we could get three+ move tables.
pub struct CompositeMoveTable<Sym, PermIndexA, PermIndexB, Turn, RepIndexB, MoveTableB> {
a: Arc<MoveTable<Sym, PermIndexA, Turn>>,
     b: MoveTableB,
     pib: std::marker::PhantomData<PermIndexB>,
     rib: std::marker::PhantomData<RepIndexB>,
}

impl<Turn: Sequence + Copy + PartialEq + Into<usize> + EquivalenceClass<Sym>, Sym: Sequence + Copy + Clone + Into<usize> + Invertable + PG, PermIndexA: Sequence + Copy + Ord + TryFrom<usize> + Into<usize>, PermIndexB: Sequence + Ord + Copy, RepIndexB, MoveTableB: TableTurn<Sym, RepIndexB, Turn> + TableSymTurn<Sym, RepIndexB, Turn> + TableRawIndexToSymIndex<Sym, PermIndexB, RepIndexB> + TableSymIndexToRawIndex<Sym, PermIndexB, RepIndexB>> CompositeMoveTable<Sym, PermIndexA, PermIndexB, Turn, RepIndexB, MoveTableB> where <PermIndexA as TryFrom<usize>>::Error: std::fmt::Debug {
    pub fn new(a: Arc<MoveTable<Sym, PermIndexA, Turn>>, b: MoveTableB) -> Self {
        CompositeMoveTable {
            a,
            b,
            pib: std::marker::PhantomData,
            rib: std::marker::PhantomData,
        }
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

        // NOTE: We could alternatively chose "smallest inner symmetry" and
        // store a sym-index instead of a raw index, but then we must consider
        // self symmetry of both a and b, and find the smallest product.
        for s in syms {
            let (b_rep_0, sb_0) = self.b.table_raw_index_to_sym_index(i.1);
            let (b_rep_1, sb_1) = self.b.table_turn(b_rep_0, t.get_equivalent(&sb_0));
            let b_raw_rep = self.b.table_sym_index_to_raw_index((b_rep_1, s.invert().permute(sb_0).permute(sb_1)));
            if b_raw_rep <= smallest {
                smallest = b_raw_rep;
                sym = s;
            }
        }

        (CompositeIndex(a_rep_1, smallest), sym)
    }

    pub fn sym_turn(&self, (ri0, s0): (CompositeIndex<RepIndex<Sym, PermIndexA>, PermIndexB>, Sym), t: Turn) -> (CompositeIndex<RepIndex<Sym, PermIndexA>, PermIndexB>, Sym) {
        let (ri1, s1) = self.turn(ri0, t.get_equivalent(&s0));
        (ri1, s0.permute(s1))
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
            let (b_rep, s_b) = self.b.table_raw_index_to_sym_index(pi.1);
            let b_raw = self.b.table_sym_index_to_raw_index((b_rep, s.invert().permute(s_b)));
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
        let (b_rep, s_b) = self.b.table_raw_index_to_sym_index(b_rep_raw);
        let b_raw = self.b.table_sym_index_to_raw_index((b_rep, s_a.permute(s_b)));
        (a_raw, b_raw)
    }
}

// TODO: Don't need as many constraints (just the specfic ones to this trait)
// if we fully migrate to traits and the implementation is here
impl<Turn: Sequence + Copy + PartialEq + Into<usize> + EquivalenceClass<Sym>, Sym: Sequence + Copy + Clone + Into<usize> + Invertable + PG, PermIndexA: Sequence + Copy + Ord + TryFrom<usize> + Into<usize>, PermIndexB: Sequence + Ord + Copy, RepIndexB, MoveTableB: TableTurn<Sym, RepIndexB, Turn> + TableSymTurn<Sym, RepIndexB, Turn> + TableRawIndexToSymIndex<Sym, PermIndexB, RepIndexB> + TableSymIndexToRawIndex<Sym, PermIndexB, RepIndexB>> TableTurn<Sym, CompositeIndex<RepIndex<Sym, PermIndexA>, PermIndexB>, Turn> for CompositeMoveTable<Sym, PermIndexA, PermIndexB, Turn, RepIndexB, MoveTableB> where <PermIndexA as TryFrom<usize>>::Error: std::fmt::Debug {
    fn table_turn(&self, ri: CompositeIndex<RepIndex<Sym, PermIndexA>, PermIndexB>, t: Turn) -> (CompositeIndex<RepIndex<Sym, PermIndexA>, PermIndexB>, Sym) {
        self.turn(ri, t)
    }
}

impl<Turn: Sequence + Copy + PartialEq + Into<usize> + EquivalenceClass<Sym>, Sym: Sequence + Copy + Clone + Into<usize> + Invertable + PG, PermIndexA: Sequence + Copy + Ord + TryFrom<usize> + Into<usize>, PermIndexB: Sequence + Ord + Copy, RepIndexB, MoveTableB: TableTurn<Sym, RepIndexB, Turn> + TableSymTurn<Sym, RepIndexB, Turn> + TableRawIndexToSymIndex<Sym, PermIndexB, RepIndexB> + TableSymIndexToRawIndex<Sym, PermIndexB, RepIndexB>> TableSymTurn<Sym, CompositeIndex<RepIndex<Sym, PermIndexA>, PermIndexB>, Turn>  for CompositeMoveTable<Sym, PermIndexA, PermIndexB, Turn, RepIndexB, MoveTableB> where <PermIndexA as TryFrom<usize>>::Error: std::fmt::Debug {
    fn table_sym_turn(&self, si: (CompositeIndex<RepIndex<Sym, PermIndexA>, PermIndexB>, Sym), t: Turn) -> (CompositeIndex<RepIndex<Sym, PermIndexA>, PermIndexB>, Sym) {
        self.sym_turn(si, t)
    }
}

impl<Turn: Sequence + Copy + PartialEq + Into<usize> + EquivalenceClass<Sym>, Sym: Sequence + Copy + Clone + Into<usize> + Invertable + PG, PermIndexA: Sequence + Copy + Ord + TryFrom<usize> + Into<usize>, PermIndexB: Sequence + Ord + Copy, RepIndexB, MoveTableB: TableTurn<Sym, RepIndexB, Turn> + TableSymTurn<Sym, RepIndexB, Turn> + TableRawIndexToSymIndex<Sym, PermIndexB, RepIndexB> + TableSymIndexToRawIndex<Sym, PermIndexB, RepIndexB>> TableRawIndexToSymIndex<Sym, (PermIndexA, PermIndexB), CompositeIndex<RepIndex<Sym, PermIndexA>, PermIndexB>> for CompositeMoveTable<Sym, PermIndexA, PermIndexB, Turn, RepIndexB, MoveTableB> where <PermIndexA as TryFrom<usize>>::Error: std::fmt::Debug {
    fn table_raw_index_to_sym_index(&self, pi: (PermIndexA, PermIndexB)) -> (CompositeIndex<RepIndex<Sym, PermIndexA>, PermIndexB>, Sym) {
        self.raw_index_to_sym_index(pi)
    }
}

impl<Turn: Sequence + Copy + PartialEq + Into<usize> + EquivalenceClass<Sym>, Sym: Sequence + Copy + Clone + Into<usize> + Invertable + PG, PermIndexA: Sequence + Copy + Ord + TryFrom<usize> + Into<usize>, PermIndexB: Sequence + Ord + Copy, RepIndexB, MoveTableB: TableTurn<Sym, RepIndexB, Turn> + TableSymTurn<Sym, RepIndexB, Turn> + TableRawIndexToSymIndex<Sym, PermIndexB, RepIndexB> + TableSymIndexToRawIndex<Sym, PermIndexB, RepIndexB>> TableSymIndexToRawIndex<Sym, (PermIndexA, PermIndexB), CompositeIndex<RepIndex<Sym, PermIndexA>, PermIndexB>> for CompositeMoveTable<Sym, PermIndexA, PermIndexB, Turn, RepIndexB, MoveTableB> where <PermIndexA as TryFrom<usize>>::Error: std::fmt::Debug {
    fn table_sym_index_to_raw_index(&self, si: (CompositeIndex<RepIndex<Sym, PermIndexA>, PermIndexB>, Sym)) -> (PermIndexA, PermIndexB) {
        self.sym_index_to_raw_index(si)
    }
}

impl<Turn: Sequence + Copy + PartialEq + Into<usize> + EquivalenceClass<Sym>, Sym: Sequence + Copy + Clone + Into<usize> + Invertable + PG, PermIndexA: Sequence + Copy + Ord + TryFrom<usize> + Into<usize>, PermIndexB: Sequence + Ord + Copy, RepIndexB, MoveTableB: TableTurn<Sym, RepIndexB, Turn> + TableSymTurn<Sym, RepIndexB, Turn> + TableRawIndexToSymIndex<Sym, PermIndexB, RepIndexB> + TableSymIndexToRawIndex<Sym, PermIndexB, RepIndexB>> TableRepCount for CompositeMoveTable<Sym, PermIndexA, PermIndexB, Turn, RepIndexB, MoveTableB> where <PermIndexA as TryFrom<usize>>::Error: std::fmt::Debug {
    fn table_rep_count(&self) -> usize {
        self.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use three_triangles;
    use three_triangles_stack::*;
    use algebraic_actions::MagmaAction;
 
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

        // All routes to a permutation result in the same representative
        for pi0 in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            let top_p0: three_triangles::ThreeTriangles = pi0.into();
            let bottom_p0: three_triangles::ThreeTriangles = pi0.into();
            let si0 = move_table.raw_index_to_sym_index((pi0, pi0));
            for t in all::<three_triangles::Turns>() {
                let top_p1 = top_p0.permute(t.into());
                let bottom_p1 = bottom_p0.permute(t.into());
                let si1 = move_table.raw_index_to_sym_index((top_p1.into(), bottom_p1.into()));
                let si_rt = move_table.turn(si1.0, t.get_equivalent(&si1.1).invert());
                assert_eq!(si0.0, si_rt.0);
            }
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
                let (ri_b, s) = move_table.turn(ri_a, t);
                let (ri_rt, _ ) = move_table.turn(ri_b, t.invert().get_equivalent(&s));
                assert_eq!(ri_a, ri_rt);
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

        // All routes to a permutation result in the same representative
        for top_pi0 in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            for bottom_pi0 in all::<three_triangles::ThreeTrianglesEvenIndex>() {
                let top_p0: TopThreeTriangles = top_pi0.into();
                let bottom_p0: BottomThreeTriangles = bottom_pi0.into();
                let si0 = move_table.raw_index_to_sym_index((top_pi0, bottom_pi0));
                for t in all::<Turns>() {
                    let top_p1 = top_p0.permute(t.into());
                    let bottom_p1 = bottom_p0.permute(t.into());
                    let si1 = move_table.raw_index_to_sym_index((top_p1.into(), bottom_p1.into()));
                    let si_rt = move_table.turn(si1.0, t.get_equivalent(&si1.1).invert());
                    assert_eq!(si0.0, si_rt.0);
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
                    let (ri_b, s) = move_table.turn(ri_a, t);
                    let (ri_rt, _ ) = move_table.turn(ri_b, t.invert().get_equivalent(&s));
                    assert_eq!(ri_a, ri_rt);
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

        // All routes to a permutation result in the same representative
        for top_pi0 in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            for bottom_pi0 in all::<three_triangles::ThreeTrianglesEvenIndex>() {
                let top_p0: TopThreeTriangles = top_pi0.into();
                let bottom_p0: BottomThreeTriangles = bottom_pi0.into();
                let si0 = move_table.raw_index_to_sym_index((top_pi0, bottom_pi0));
                for t in all::<Turns>() {
                    let top_p1 = top_p0.permute(t.into());
                    let bottom_p1 = bottom_p0.permute(t.into());
                    let si1 = move_table.raw_index_to_sym_index((top_p1.into(), bottom_p1.into()));
                    let si_rt = move_table.turn(si1.0, t.get_equivalent(&si1.1).invert());
                    assert_eq!(si0.0, si_rt.0);
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
                    let (ri_b, s) = move_table.turn(ri_a, t);
                    let (ri_rt, _ ) = move_table.turn(ri_b, t.invert().get_equivalent(&s));
                    assert_eq!(ri_a, ri_rt);
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

        // All routes to a permutation result in the same representative
        for top_pi0 in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            for bottom_pi0 in all::<three_triangles::ThreeTrianglesEvenIndex>() {
                let top_p0: TopThreeTriangles = top_pi0.into();
                let bottom_p0: BottomThreeTriangles = bottom_pi0.into();
                let si0 = move_table.raw_index_to_sym_index((top_pi0, bottom_pi0));
                for t in all::<Turns>() {
                    let top_p1 = top_p0.permute(t.into());
                    let bottom_p1 = bottom_p0.permute(t.into());
                    let si1 = move_table.raw_index_to_sym_index((top_p1.into(), bottom_p1.into()));
                    let si_rt = move_table.turn(si1.0, t.get_equivalent(&si1.1).invert());
                    assert_eq!(si0.0, si_rt.0);
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
                    let (ri_b, s) = move_table.turn(ri_a, t);
                    let (ri_rt, _ ) = move_table.turn(ri_b, t.invert().get_equivalent(&s));
                    assert_eq!(ri_a, ri_rt);
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

        // All routes to a permutation result in the same representative
        for top_pi0 in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            for bottom_pi0 in all::<three_triangles::ThreeTrianglesEvenIndex>() {
                let top_p0: TopThreeTriangles = top_pi0.into();
                let bottom_p0: BottomThreeTriangles = bottom_pi0.into();
                let si0 = move_table.raw_index_to_sym_index((top_pi0, bottom_pi0));
                for t in all::<Turns>() {
                    let top_p1 = top_p0.permute(t.into());
                    let bottom_p1 = bottom_p0.permute(t.into());
                    let si1 = move_table.raw_index_to_sym_index((top_p1.into(), bottom_p1.into()));
                    let si_rt = move_table.turn(si1.0, t.get_equivalent(&si1.1).invert());
                    assert_eq!(si0.0, si_rt.0);
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
                    let (ri_b, s) = move_table.turn(ri_a, t);
                    let (ri_rt, _ ) = move_table.turn(ri_b, t.invert().get_equivalent(&s));
                    assert_eq!(ri_a, ri_rt);
                }
            }
        }
    }

    use three_trapezoids as tt;
    use three_trapezoids::inner as tt_inner;
    use three_trapezoids::outer as tt_outer;

    #[test]
    fn composite_move_table_is_correct_for_inner_and_outer_three_trapezoids_with_no_symmetry() {
        let inner_rep_table = Arc::new(RepresentativeTable::new::<tt_inner::ThreeTrapezoidsInner>());
        let inner_move_table: Arc<MoveTable<tt::NoSymmetry, tt_inner::ThreeTrapezoidsInnerIndex, tt::Turns>> = Arc::new(MoveTable::new_on_pattern::<tt::ThreeTrapezoids, tt_inner::ThreeTrapezoidsInner>(inner_rep_table.clone()));

        let outer_rep_table = Arc::new(RepresentativeTable::new::<tt_outer::ThreeTrapezoidsOuter>());
        let outer_move_table: MoveTable<tt::NoSymmetry, tt_outer::ThreeTrapezoidsOuterIndex, tt::Turns> = MoveTable::new_on_pattern::<tt::ThreeTrapezoids, tt_outer::ThreeTrapezoidsOuter>(outer_rep_table.clone());
        let move_table = CompositeMoveTable::new(inner_move_table.clone(), Arc::new(outer_move_table));

        // Raw to sym and sym to raw functions round trip
        for inner_perm_index in all::<tt_inner::ThreeTrapezoidsInnerIndex>() {
            for outer_perm_index in all::<tt_outer::ThreeTrapezoidsOuterIndex>() {
                let (inner_perm_index_rt, outer_perm_index_rt) = move_table.sym_index_to_raw_index(move_table.raw_index_to_sym_index((inner_perm_index, outer_perm_index)));
                assert_eq!(inner_perm_index_rt, inner_perm_index);
                assert_eq!(outer_perm_index_rt, outer_perm_index);
            }
        }

        // Applying move_table moves is identical to applying permutations
        for pi in all::<tt::ThreeTrapezoidsIndex>() {
            let p: tt::ThreeTrapezoids = pi.into();
            let p_inner: tt_inner::ThreeTrapezoidsInner = p.into();
            let p_outer: tt_outer::ThreeTrapezoidsOuter = p.into();
            let pi_inner: tt_inner::ThreeTrapezoidsInnerIndex = p_inner.into();
            let pi_outer: tt_outer::ThreeTrapezoidsOuterIndex = p_outer.into();
            for t in all::<tt::Turns>() {
                let turn: tt::ThreeTrapezoids = t.into();
                let inner_by_perm = p_inner.act(turn);
                let outer_by_perm = p_outer.act(turn);
                let (inner_by_table, outer_by_table) = move_table.sym_index_to_raw_index(move_table.sym_turn(move_table.raw_index_to_sym_index((pi_inner, pi_outer)), t));
                assert_eq!(inner_by_perm, inner_by_table.into());
                assert_eq!(outer_by_perm, outer_by_table.into());
            }
        }

        // All routes to a permutation result in the same representative
        for pi0 in all::<tt::ThreeTrapezoidsIndex>() {
            let p0: tt::ThreeTrapezoids = pi0.into();
            let p0_inner: tt_inner::ThreeTrapezoidsInner = p0.into();
            let p0_outer: tt_outer::ThreeTrapezoidsOuter = p0.into();
            let si0 = move_table.raw_index_to_sym_index((p0_inner.into(), p0_outer.into()));
            for t in all::<tt::Turns>() {
                let p1 = p0.permute(t.into());
                let p1_inner: tt_inner::ThreeTrapezoidsInner = p1.into();
                let p1_outer: tt_outer::ThreeTrapezoidsOuter = p1.into();
                let si1 = move_table.raw_index_to_sym_index((p1_inner.into(), p1_outer.into()));
                let si_rt = move_table.sym_turn(si1, t.invert());
                assert_eq!(si0.0, si_rt.0);
            }
        }

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        for pi in all::<tt::ThreeTrapezoidsIndex>() {
            let p: tt::ThreeTrapezoids = pi.into();
            let p_inner: tt_inner::ThreeTrapezoidsInner = p.into();
            let p_outer: tt_outer::ThreeTrapezoidsOuter = p.into();
            for t in all::<tt::Turns>() {
                let si0 = move_table.raw_index_to_sym_index((p_inner.into(), p_outer.into()));
                let si1 = move_table.sym_turn(si0, t);
                let si_rt = move_table.sym_turn(si1, t.invert());
                assert_eq!(si0.0, si_rt.0);
            }
        }
    }

    #[test]
    fn composite_move_table_is_correct_for_inner_and_outer_three_trapezoids_with_mirror_ud_symmetry() {
        let inner_rep_table = Arc::new(RepresentativeTable::new::<tt_inner::ThreeTrapezoidsInner>());
        let inner_move_table: Arc<MoveTable<tt::MirrorUDSymmetry, tt_inner::ThreeTrapezoidsInnerIndex, tt::Turns>> = Arc::new(MoveTable::new_on_pattern::<tt::ThreeTrapezoids, tt_inner::ThreeTrapezoidsInner>(inner_rep_table.clone()));

        let outer_rep_table = Arc::new(RepresentativeTable::new::<tt_outer::ThreeTrapezoidsOuter>());
        let outer_move_table: MoveTable<tt::MirrorUDSymmetry, tt_outer::ThreeTrapezoidsOuterIndex, tt::Turns> = MoveTable::new_on_pattern::<tt::ThreeTrapezoids, tt_outer::ThreeTrapezoidsOuter>(outer_rep_table.clone());
        let move_table = CompositeMoveTable::new(inner_move_table.clone(), Arc::new(outer_move_table));

        // Raw to sym and sym to raw functions round trip
        for inner_perm_index in all::<tt_inner::ThreeTrapezoidsInnerIndex>() {
            for outer_perm_index in all::<tt_outer::ThreeTrapezoidsOuterIndex>() {
                let (inner_perm_index_rt, outer_perm_index_rt) = move_table.sym_index_to_raw_index(move_table.raw_index_to_sym_index((inner_perm_index, outer_perm_index)));
                assert_eq!(inner_perm_index_rt, inner_perm_index);
                assert_eq!(outer_perm_index_rt, outer_perm_index);
            }
        }

        // Applying move_table moves is identical to applying permutations
        for pi in all::<tt::ThreeTrapezoidsIndex>() {
            let p: tt::ThreeTrapezoids = pi.into();
            let p_inner: tt_inner::ThreeTrapezoidsInner = p.into();
            let p_outer: tt_outer::ThreeTrapezoidsOuter = p.into();
            let pi_inner: tt_inner::ThreeTrapezoidsInnerIndex = p_inner.into();
            let pi_outer: tt_outer::ThreeTrapezoidsOuterIndex = p_outer.into();
            for t in all::<tt::Turns>() {
                let turn: tt::ThreeTrapezoids = t.into();
                let inner_by_perm = p_inner.act(turn);
                let outer_by_perm = p_outer.act(turn);
                let (inner_by_table, outer_by_table) = move_table.sym_index_to_raw_index(move_table.sym_turn(move_table.raw_index_to_sym_index((pi_inner, pi_outer)), t));
                assert_eq!(inner_by_perm, inner_by_table.into());
                assert_eq!(outer_by_perm, outer_by_table.into());
            }
        }

        // All routes to a permutation result in the same representative
        for pi0 in all::<tt::ThreeTrapezoidsIndex>() {
            let p0: tt::ThreeTrapezoids = pi0.into();
            let p0_inner: tt_inner::ThreeTrapezoidsInner = p0.into();
            let p0_outer: tt_outer::ThreeTrapezoidsOuter = p0.into();
            let si0 = move_table.raw_index_to_sym_index((p0_inner.into(), p0_outer.into()));
            for t in all::<tt::Turns>() {
                let p1 = p0.permute(t.into());
                let p1_inner: tt_inner::ThreeTrapezoidsInner = p1.into();
                let p1_outer: tt_outer::ThreeTrapezoidsOuter = p1.into();
                let si1 = move_table.raw_index_to_sym_index((p1_inner.into(), p1_outer.into()));
                let si_rt = move_table.sym_turn(si1, t.invert());
                assert_eq!(si0.0, si_rt.0);
            }
        }

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        for pi in all::<tt::ThreeTrapezoidsIndex>() {
            let p: tt::ThreeTrapezoids = pi.into();
            let p_inner: tt_inner::ThreeTrapezoidsInner = p.into();
            let p_outer: tt_outer::ThreeTrapezoidsOuter = p.into();
            for t in all::<tt::Turns>() {
                let si0 = move_table.raw_index_to_sym_index((p_inner.into(), p_outer.into()));
                let si1 = move_table.sym_turn(si0, t);
                let si_rt = move_table.sym_turn(si1, t.invert());
                assert_eq!(si0.0, si_rt.0);
            }
        }
    }

    #[test]
    fn composite_move_table_is_correct_for_inner_and_outer_three_trapezoids_with_rotational_symmetry() {
        let inner_rep_table = Arc::new(RepresentativeTable::new::<tt_inner::ThreeTrapezoidsInner>());
        let inner_move_table: Arc<MoveTable<tt::RotationalSymmetry, tt_inner::ThreeTrapezoidsInnerIndex, tt::Turns>> = Arc::new(MoveTable::new_on_pattern::<tt::ThreeTrapezoids, tt_inner::ThreeTrapezoidsInner>(inner_rep_table.clone()));

        let outer_rep_table = Arc::new(RepresentativeTable::new::<tt_outer::ThreeTrapezoidsOuter>());
        let outer_move_table: MoveTable<tt::RotationalSymmetry, tt_outer::ThreeTrapezoidsOuterIndex, tt::Turns> = MoveTable::new_on_pattern::<tt::ThreeTrapezoids, tt_outer::ThreeTrapezoidsOuter>(outer_rep_table.clone());
        let move_table = CompositeMoveTable::new(inner_move_table.clone(), Arc::new(outer_move_table));

        // Raw to sym and sym to raw functions round trip
        for inner_perm_index in all::<tt_inner::ThreeTrapezoidsInnerIndex>() {
            for outer_perm_index in all::<tt_outer::ThreeTrapezoidsOuterIndex>() {
                let (inner_perm_index_rt, outer_perm_index_rt) = move_table.sym_index_to_raw_index(move_table.raw_index_to_sym_index((inner_perm_index, outer_perm_index)));
                assert_eq!(inner_perm_index_rt, inner_perm_index);
                assert_eq!(outer_perm_index_rt, outer_perm_index);
            }
        }

        // Applying move_table moves is identical to applying permutations
        for pi in all::<tt::ThreeTrapezoidsIndex>() {
            let p: tt::ThreeTrapezoids = pi.into();
            let p_inner: tt_inner::ThreeTrapezoidsInner = p.into();
            let p_outer: tt_outer::ThreeTrapezoidsOuter = p.into();
            let pi_inner: tt_inner::ThreeTrapezoidsInnerIndex = p_inner.into();
            let pi_outer: tt_outer::ThreeTrapezoidsOuterIndex = p_outer.into();
            for t in all::<tt::Turns>() {
                let turn: tt::ThreeTrapezoids = t.into();
                let inner_by_perm = p_inner.act(turn);
                let outer_by_perm = p_outer.act(turn);
                let (inner_by_table, outer_by_table) = move_table.sym_index_to_raw_index(move_table.sym_turn(move_table.raw_index_to_sym_index((pi_inner, pi_outer)), t));
                assert_eq!(inner_by_perm, inner_by_table.into());
                assert_eq!(outer_by_perm, outer_by_table.into());
            }
        }

        // All routes to a permutation result in the same representative
        for pi0 in all::<tt::ThreeTrapezoidsIndex>() {
            let p0: tt::ThreeTrapezoids = pi0.into();
            let p0_inner: tt_inner::ThreeTrapezoidsInner = p0.into();
            let p0_outer: tt_outer::ThreeTrapezoidsOuter = p0.into();
            let si0 = move_table.raw_index_to_sym_index((p0_inner.into(), p0_outer.into()));
            for t in all::<tt::Turns>() {
                let p1 = p0.permute(t.into());
                let p1_inner: tt_inner::ThreeTrapezoidsInner = p1.into();
                let p1_outer: tt_outer::ThreeTrapezoidsOuter = p1.into();
                let si1 = move_table.raw_index_to_sym_index((p1_inner.into(), p1_outer.into()));
                let si_rt = move_table.sym_turn(si1, t.invert());
                assert_eq!(si0.0, si_rt.0);
            }
        }

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        for pi in all::<tt::ThreeTrapezoidsIndex>() {
            let p: tt::ThreeTrapezoids = pi.into();
            let p_inner: tt_inner::ThreeTrapezoidsInner = p.into();
            let p_outer: tt_outer::ThreeTrapezoidsOuter = p.into();
            for t in all::<tt::Turns>() {
                let si0 = move_table.raw_index_to_sym_index((p_inner.into(), p_outer.into()));
                let si1 = move_table.sym_turn(si0, t);
                let si_rt = move_table.sym_turn(si1, t.invert());
                assert_eq!(si0.0, si_rt.0);
            }
        }
    }

    #[test]
    fn composite_move_table_is_correct_for_inner_and_outer_three_trapezoids_with_full_symmetry() {
        let inner_rep_table = Arc::new(RepresentativeTable::new::<tt_inner::ThreeTrapezoidsInner>());
        let inner_move_table: Arc<MoveTable<tt::FullSymmetry, tt_inner::ThreeTrapezoidsInnerIndex, tt::Turns>> = Arc::new(MoveTable::new_on_pattern::<tt::ThreeTrapezoids, tt_inner::ThreeTrapezoidsInner>(inner_rep_table.clone()));

        let outer_rep_table = Arc::new(RepresentativeTable::new::<tt_outer::ThreeTrapezoidsOuter>());
        let outer_move_table: MoveTable<tt::FullSymmetry, tt_outer::ThreeTrapezoidsOuterIndex, tt::Turns> = MoveTable::new_on_pattern::<tt::ThreeTrapezoids, tt_outer::ThreeTrapezoidsOuter>(outer_rep_table.clone());
        let move_table = CompositeMoveTable::new(inner_move_table.clone(), Arc::new(outer_move_table));

        // Raw to sym and sym to raw functions round trip
        for inner_perm_index in all::<tt_inner::ThreeTrapezoidsInnerIndex>() {
            for outer_perm_index in all::<tt_outer::ThreeTrapezoidsOuterIndex>() {
                let (inner_perm_index_rt, outer_perm_index_rt) = move_table.sym_index_to_raw_index(move_table.raw_index_to_sym_index((inner_perm_index, outer_perm_index)));
                assert_eq!(inner_perm_index_rt, inner_perm_index);
                assert_eq!(outer_perm_index_rt, outer_perm_index);
            }
        }

        // Applying move_table moves is identical to applying permutations
        for pi in all::<tt::ThreeTrapezoidsIndex>() {
            let p: tt::ThreeTrapezoids = pi.into();
            let p_inner: tt_inner::ThreeTrapezoidsInner = p.into();
            let p_outer: tt_outer::ThreeTrapezoidsOuter = p.into();
            let pi_inner: tt_inner::ThreeTrapezoidsInnerIndex = p_inner.into();
            let pi_outer: tt_outer::ThreeTrapezoidsOuterIndex = p_outer.into();
            for t in all::<tt::Turns>() {
                let turn: tt::ThreeTrapezoids = t.into();
                let inner_by_perm = p_inner.act(turn);
                let outer_by_perm = p_outer.act(turn);
                let (inner_by_table, outer_by_table) = move_table.sym_index_to_raw_index(move_table.sym_turn(move_table.raw_index_to_sym_index((pi_inner, pi_outer)), t));
                assert_eq!(inner_by_perm, inner_by_table.into());
                assert_eq!(outer_by_perm, outer_by_table.into());
            }
        }

        // All routes to a permutation result in the same representative
        for pi0 in all::<tt::ThreeTrapezoidsIndex>() {
            let p0: tt::ThreeTrapezoids = pi0.into();
            let p0_inner: tt_inner::ThreeTrapezoidsInner = p0.into();
            let p0_outer: tt_outer::ThreeTrapezoidsOuter = p0.into();
            let si0 = move_table.raw_index_to_sym_index((p0_inner.into(), p0_outer.into()));
            for t in all::<tt::Turns>() {
                let p1 = p0.permute(t.into());
                let p1_inner: tt_inner::ThreeTrapezoidsInner = p1.into();
                let p1_outer: tt_outer::ThreeTrapezoidsOuter = p1.into();
                let si1 = move_table.raw_index_to_sym_index((p1_inner.into(), p1_outer.into()));
                let si_rt = move_table.sym_turn(si1, t.invert());
                assert_eq!(si0.0, si_rt.0);
            }
        }

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        for pi in all::<tt::ThreeTrapezoidsIndex>() {
            let p: tt::ThreeTrapezoids = pi.into();
            let p_inner: tt_inner::ThreeTrapezoidsInner = p.into();
            let p_outer: tt_outer::ThreeTrapezoidsOuter = p.into();
            for t in all::<tt::Turns>() {
                let si0 = move_table.raw_index_to_sym_index((p_inner.into(), p_outer.into()));
                let si1 = move_table.sym_turn(si0, t);
                let si_rt = move_table.sym_turn(si1, t.invert());
                assert_eq!(si0.0, si_rt.0);
            }
        }
    }

    // TODO: Tests for composites of composites
}
