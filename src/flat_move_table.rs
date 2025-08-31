use permutation_group::PermutationGroup as PG;
use invertable::Invertable;
use equivalence_class::EquivalenceClass;
use algebraic_actions::GroupAction;
use representative_table::{ RepresentativeTable, RepIndex };
use table_traits::{ TableTurn, TableSymTurn, TableRawIndexToSymIndex, TableSymIndexToRawIndex, TableRepCount };

use std::sync::Arc;
use std::convert::TryFrom;
use enum_iterator::{ Sequence, all, cardinality };

#[derive(Debug)]
pub struct MoveTable<Sym, PermIndex, Turn> {
    // TODO: Not even really sure we need this Arc, probably nothing else needs
    // to access this.  There are some upsides of making this an abstract T (we
    // don't propagate all the trait bounds), but the downside is that we need
    // a full trait definition of our RepTable to implement and error messages
    // get wierder.
    rep_table: Arc<RepresentativeTable<Sym, PermIndex>>,
    turn_table: Vec<(RepIndex<Sym, PermIndex>, Sym)>,
    sym_table: Vec<PermIndex>,
    turns: std::marker::PhantomData<Turn>,
}

// TODO: If the PermIndex is even all Turns must be even too, and if the
// PermIndex is odd then at least one Turn in the set must also be odd.  Can
// this be expressed through Traits to guarantee a match?
impl<Turn: Sequence + Copy + PartialEq + Into<usize> + EquivalenceClass<Sym>, Sym: PG + Sequence + Copy + Clone + Into<usize> + Invertable, PermIndex: Sequence + Copy + Ord + TryFrom<usize> + Into<usize>> MoveTable<Sym, PermIndex, Turn> where <PermIndex as TryFrom<usize>>::Error: std::fmt::Debug {
    // TODO: We can have an alterative method that automatically generates the
    // RepTable to simplify cases where it isn't shared with other MoveTables.
    pub fn new<Perm: PG + Clone + EquivalenceClass<Sym> + Into<PermIndex>>(rep_table: Arc<RepresentativeTable<Sym, PermIndex>>) -> Self where PermIndex: Into<Perm>, Turn: Into<Perm> {
        Self::new_on_pattern::<Perm, Perm>(rep_table)
    }

    pub fn new_on_pattern<FullPerm: PG, Perm: GroupAction<FullPerm> + Clone + EquivalenceClass<Sym> + Into<PermIndex>>(rep_table: Arc<RepresentativeTable<Sym, PermIndex>>) -> Self where PermIndex: Into<Perm>, Turn: Into<FullPerm> {
        let mut turn_table = Vec::with_capacity(rep_table.len() * cardinality::<Turn>());
        let mut sym_table = Vec::with_capacity(rep_table.len() * cardinality::<Sym>());

        for ri in rep_table.rep_indexes() {
            let p: Perm = rep_table.rep_index_to_perm_index(ri).into();
            for t in all::<Turn>() {
                let turn: FullPerm = t.into();
                let turned: Perm = p.clone().act(turn);
                turn_table.push(rep_table.raw_index_to_sym_index(turned.into()));
            }

            for s in all::<Sym>() {
                // NOTE: We invert the Sym, because we want to use this for
                // _undoing_ sym-coords.  We aren't storing the equivalents,
                // we're storing reverse equivalents.
                sym_table.push(p.clone().get_equivalent(&s.invert()).into());
            }
        }

        MoveTable {
            rep_table,
            turn_table,
            sym_table,
            turns: std::marker::PhantomData,
        }
    }

    pub fn turn(&self, ri: RepIndex<Sym, PermIndex>, t: Turn) -> (RepIndex<Sym, PermIndex>, Sym) {
        let i = <RepIndex<Sym, PermIndex> as Into<usize>>::into(ri) * cardinality::<Turn>() + <Turn as Into<usize>>::into(t);
        self.turn_table[i]
    }

    pub fn sym_turn(&self, si: (RepIndex<Sym, PermIndex>, Sym), t: Turn) -> (RepIndex<Sym, PermIndex>, Sym) {
        let (ri, s) = self.turn(si.0, t.get_equivalent(&si.1));
        (ri, si.1.permute(s))
    }

    // Count of Reps
    pub fn len(&self) -> usize {
        self.rep_table.len()
    }

    pub fn raw_index_to_sym_index(&self, pi: PermIndex) -> (RepIndex<Sym, PermIndex>, Sym) {
        self.rep_table.raw_index_to_sym_index(pi)
    }

    pub fn sym_index_to_raw_index(&self, si: (RepIndex<Sym, PermIndex>, Sym)) -> PermIndex {
        let (ri, s) = si;
        let i = <RepIndex<Sym, PermIndex> as Into<usize>>::into(ri) * cardinality::<Sym>() + <Sym as Into<usize>>::into(s);
        self.sym_table[i]
    }

    pub fn is_self_symmetric(&self, ri: RepIndex<Sym, PermIndex>) -> bool {
        self.rep_table.is_self_symmetric(ri)
    }
}

// TODO: Don't need as many constraints (just the specfic ones to this trait)
// if we fully migrate to traits and the implementation is here
impl<Turn: Sequence + Copy + PartialEq + Into<usize> + EquivalenceClass<Sym>, Sym: PG + Sequence + Copy + Clone + Into<usize> + Invertable, PermIndex: Sequence + Copy + Ord + TryFrom<usize> + Into<usize>> TableTurn<Sym, RepIndex<Sym, PermIndex>, Turn> for MoveTable<Sym, PermIndex, Turn> where <PermIndex as TryFrom<usize>>::Error: std::fmt::Debug {
    fn table_turn(&self, ri: RepIndex<Sym, PermIndex>, t: Turn) -> (RepIndex<Sym, PermIndex>, Sym) {
        self.turn(ri, t)
    }
}

impl<Turn: Sequence + Copy + PartialEq + Into<usize> + EquivalenceClass<Sym>, Sym: PG + Sequence + Copy + Clone + Into<usize> + Invertable, PermIndex: Sequence + Copy + Ord + TryFrom<usize> + Into<usize>> TableSymTurn<Sym, RepIndex<Sym, PermIndex>, Turn> for MoveTable<Sym, PermIndex, Turn> where <PermIndex as TryFrom<usize>>::Error: std::fmt::Debug {
    fn table_sym_turn(&self, si: (RepIndex<Sym, PermIndex>, Sym), t: Turn) -> (RepIndex<Sym, PermIndex>, Sym) {
        self.sym_turn(si, t)
    }
}

impl<Turn: Sequence + Copy + PartialEq + Into<usize> + EquivalenceClass<Sym>, Sym: PG + Sequence + Copy + Clone + Into<usize> + Invertable, PermIndex: Sequence + Copy + Ord + TryFrom<usize> + Into<usize>> TableRawIndexToSymIndex<Sym, PermIndex, RepIndex<Sym, PermIndex>> for MoveTable<Sym, PermIndex, Turn> where <PermIndex as TryFrom<usize>>::Error: std::fmt::Debug {
    fn table_raw_index_to_sym_index(&self, pi: PermIndex) -> (RepIndex<Sym, PermIndex>, Sym) {
        self.raw_index_to_sym_index(pi)
    }
}

impl<Turn: Sequence + Copy + PartialEq + Into<usize> + EquivalenceClass<Sym>, Sym: PG + Sequence + Copy + Clone + Into<usize> + Invertable, PermIndex: Sequence + Copy + Ord + TryFrom<usize> + Into<usize>> TableSymIndexToRawIndex<Sym, PermIndex, RepIndex<Sym, PermIndex>> for MoveTable<Sym, PermIndex, Turn> where <PermIndex as TryFrom<usize>>::Error: std::fmt::Debug {
    fn table_sym_index_to_raw_index(&self, si: (RepIndex<Sym, PermIndex>, Sym)) -> PermIndex {
        self.sym_index_to_raw_index(si)
    }
}

impl<Turn: Sequence + Copy + PartialEq + Into<usize> + EquivalenceClass<Sym>, Sym: PG + Sequence + Copy + Clone + Into<usize> + Invertable, PermIndex: Sequence + Copy + Ord + TryFrom<usize> + Into<usize>> TableRepCount for MoveTable<Sym, PermIndex, Turn> where <PermIndex as TryFrom<usize>>::Error: std::fmt::Debug {
    fn table_rep_count(&self) -> usize {
        self.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use two_triangles::*;

    fn test<Sym: PG + PartialEq + Sequence + Copy + Clone + Into<usize> + Invertable + std::fmt::Debug, PermIndex: std::fmt::Debug + Sequence + Copy + Ord + TryFrom<usize> + Into<usize>, Turn: Sequence + Copy + PartialEq + Into<usize> + EquivalenceClass<Sym> + Invertable, Perm: std::fmt::Debug + Eq + PG + Clone + EquivalenceClass<Sym> + Into<PermIndex>>() where PermIndex: Into<Perm>, Turn: Into<Perm>, <PermIndex as TryFrom<usize>>::Error: std::fmt::Debug {
        let rep_table = Arc::new(RepresentativeTable::new::<Perm>());
        // Even though either Left + Right generates all states, MoveTables
        // should basically always include turn inverses, so that they can go
        // forward or backwards.
        let move_table: MoveTable<Sym, PermIndex, Turn> = MoveTable::new::<Perm>(rep_table.clone());

        // Applying move_table moves is identical to applying permutations
        for ri in rep_table.rep_indexes() {
            let p: Perm = rep_table.rep_index_to_perm_index(ri).into();
            for t in all::<Turn>() {
                let by_perm = p.clone().permute(t.into());
                let by_table = move_table.sym_index_to_raw_index(move_table.turn(ri, t)).into();
                assert_eq!(by_perm, by_table);
            }
        }

        // Raw to sym and sym to raw functions round trip
        for pi in all::<PermIndex>() {
            let pi_rt = move_table.sym_index_to_raw_index(move_table.raw_index_to_sym_index(pi));
            assert_eq!(pi_rt, pi);
        }

        // All routes to a permutation result in the same representative (but
        // the sym coordinate is nondeterministic)
        for pi0 in all::<PermIndex>() {
            let p0: Perm = pi0.into();
            let si0 = move_table.raw_index_to_sym_index(pi0);
            for t in all::<Turn>() {
                let p1 = p0.clone().permute(t.into());
                let si1 = move_table.raw_index_to_sym_index(p1.into());
                let si_rt = move_table.turn(si1.0, t.get_equivalent(&si1.1).invert());
                assert_eq!(si0.0, si_rt.0);
            }
        }

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        for pi in all::<PermIndex>() {
            for t in all::<Turn>() {
                let (ri_a, _) = move_table.raw_index_to_sym_index(pi);
                let (ri_b, s) = move_table.turn(ri_a, t);
                let (ri_rt, _ ) = move_table.turn(ri_b, t.invert().get_equivalent(&s));
                assert_eq!(ri_a, ri_rt);
            }
        }
    }

    #[test]
    fn move_table_is_correct_for_two_triangles_without_symmetry() {
        test::<NoSymmetry, TwoTrianglesIndex, Turns, TwoTriangles>();
    }

    // TODO: There are many more valid combinations of turns and symmetries if
    // we only use one symmetry.
    #[test]
    fn move_table_is_correct_for_two_triangles_with_rotational_symmetry() {
        test::<RotationalSymmetry, TwoTrianglesIndex, Turns, TwoTriangles>();
    }

    #[test]
    fn move_table_is_correct_for_two_triangles_with_full_symmetry() {
        test::<FullSymmetry, TwoTrianglesIndex, Turns, TwoTriangles>();
    }

    #[test]
    fn move_table_is_correct_for_two_triangles_even_parity_without_symmetry() {
        test::<NoSymmetry, TwoTrianglesEvenIndex, Turns, TwoTriangles>();
    }

    #[test]
    fn move_table_is_correct_for_two_triangles_even_parity_with_rotational_symmetry() {
        test::<RotationalSymmetry, TwoTrianglesEvenIndex, Turns, TwoTriangles>();
    }

    #[test]
    fn move_table_is_correct_for_two_triangles_even_parity_with_full_symmetry() {
        test::<FullSymmetry, TwoTrianglesEvenIndex, Turns, TwoTriangles>();
    }

    use two_lines;

    #[test]
    fn move_table_is_correct_for_two_lines_without_symmetry() {
        test::<two_lines::NoSymmetry, two_lines::TwoLinesIndex, two_lines::Turns, two_lines::TwoLines>();
    }
    #[test]
    fn move_table_is_correct_for_two_lines_with_symmetry() {
        test::<two_lines::FullSymmetry, two_lines::TwoLinesIndex, two_lines::Turns, two_lines::TwoLines>();
    }

    use three_triangles;

    #[test]
    fn move_table_is_correct_for_three_triangles_without_symmetry() {
        test::<three_triangles::NoSymmetry, three_triangles::ThreeTrianglesIndex, three_triangles::Turns, three_triangles::ThreeTriangles>();
    }

    #[test]
    fn move_table_is_correct_for_three_triangles_with_mirror_ud_symmetry() {
        test::<three_triangles::MirrorUDSymmetry, three_triangles::ThreeTrianglesIndex, three_triangles::Turns, three_triangles::ThreeTriangles>();
    }

    #[test]
    fn move_table_is_correct_for_three_triangles_with_rotational_symmetry() {
        test::<three_triangles::RotationalSymmetry, three_triangles::ThreeTrianglesIndex, three_triangles::Turns, three_triangles::ThreeTriangles>();
    }

    #[test]
    fn move_table_is_correct_for_three_triangles_with_full_symmetry() {
        test::<three_triangles::FullSymmetry, three_triangles::ThreeTrianglesIndex, three_triangles::Turns, three_triangles::ThreeTriangles>();
    }

    #[test]
    fn move_table_is_correct_for_three_triangles_even_parity_without_symmetry() {
        test::<three_triangles::NoSymmetry, three_triangles::ThreeTrianglesEvenIndex, three_triangles::Turns, three_triangles::ThreeTriangles>();
    }

    #[test]
    fn move_table_is_correct_for_three_triangles_even_parity_with_mirror_ud_symmetry() {
        test::<three_triangles::MirrorUDSymmetry, three_triangles::ThreeTrianglesEvenIndex, three_triangles::Turns, three_triangles::ThreeTriangles>();
    }

    #[test]
    fn move_table_is_correct_for_three_triangles_even_parity_with_rotational_symmetry() {
        test::<three_triangles::RotationalSymmetry, three_triangles::ThreeTrianglesEvenIndex, three_triangles::Turns, three_triangles::ThreeTriangles>();
    }

    #[test]
    fn move_table_is_correct_for_three_triangles_even_parity_with_full_symmetry() {
        test::<three_triangles::FullSymmetry, three_triangles::ThreeTrianglesEvenIndex, three_triangles::Turns, three_triangles::ThreeTriangles>();
    }

    use three_triangles_stack;

    #[test]
    fn move_table_is_correct_for_top_three_triangles_even_parity_with_full_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new::<three_triangles_stack::TopThreeTriangles>());
        // Even though either Left + Right generates all states, MoveTables
        // should basically always include turn inverses, so that they can go
        // forward or backwards.
        let move_table: MoveTable<three_triangles_stack::FullSymmetry, three_triangles::ThreeTrianglesEvenIndex, three_triangles_stack::Turns> = MoveTable::new::<three_triangles_stack::TopThreeTriangles>(rep_table.clone());

        // Applying move_table moves is identical to applying permutations
        for ri in rep_table.rep_indexes() {
            let p: three_triangles_stack::TopThreeTriangles = rep_table.rep_index_to_perm_index(ri).into();
            for t in all::<three_triangles_stack::Turns>() {
                let by_perm = p.permute(t.into());
                let by_table = move_table.sym_index_to_raw_index(move_table.turn(ri, t)).into();
                assert_eq!(by_perm, by_table);
            }
        }

        // Raw to sym and sym to raw functions round trip
        for pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            let pi_rt = move_table.sym_index_to_raw_index(move_table.raw_index_to_sym_index(pi));
            assert_eq!(pi_rt, pi);
        }

        // All routes to a permutation result in the same representative (but
        // the sym coordinate is nondeterministic)
        for pi0 in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            let p0: three_triangles_stack::TopThreeTriangles = pi0.into();
            let si0 = move_table.raw_index_to_sym_index(pi0);
            for t in all::<three_triangles_stack::Turns>() {
                let p1 = p0.clone().permute(t.into());
                let si1 = move_table.raw_index_to_sym_index(p1.into());
                let si_rt = move_table.turn(si1.0, t.get_equivalent(&si1.1).invert());
                assert_eq!(si0.0, si_rt.0);
            }
        }

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        for pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            for t in all::<three_triangles_stack::Turns>() {
                let (ri_a, _) = move_table.raw_index_to_sym_index(pi);
                let (ri_b, s) = move_table.turn(ri_a, t);
                let (ri_rt, _ ) = move_table.turn(ri_b, t.invert().get_equivalent(&s));
                assert_eq!(ri_a, ri_rt);
            }
        }
    }

    #[test]
    fn move_table_is_correct_for_bottom_three_triangles_even_parity_with_full_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new::<three_triangles_stack::BottomThreeTriangles>());
        // Even though either Left + Right generates all states, MoveTables
        // should basically always include turn inverses, so that they can go
        // forward or backwards.
        let move_table: MoveTable<three_triangles_stack::FullSymmetry, three_triangles::ThreeTrianglesEvenIndex, three_triangles_stack::Turns> = MoveTable::new::<three_triangles_stack::BottomThreeTriangles>(rep_table.clone());

        // Applying move_table moves is identical to applying permutations
        for ri in rep_table.rep_indexes() {
            let p: three_triangles_stack::BottomThreeTriangles = rep_table.rep_index_to_perm_index(ri).into();
            for t in all::<three_triangles_stack::Turns>() {
                let by_perm = p.permute(t.into());
                let by_table = move_table.sym_index_to_raw_index(move_table.turn(ri, t)).into();
                assert_eq!(by_perm, by_table);
            }
        }

        // Raw to sym and sym to raw functions round trip
        for pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            let pi_rt = move_table.sym_index_to_raw_index(move_table.raw_index_to_sym_index(pi));
            assert_eq!(pi_rt, pi);
        }

        // All routes to a permutation result in the same representative (but
        // the sym coordinate is nondeterministic)
        for pi0 in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            let p0: three_triangles_stack::BottomThreeTriangles = pi0.into();
            let si0 = move_table.raw_index_to_sym_index(pi0);
            for t in all::<three_triangles_stack::Turns>() {
                let p1 = p0.clone().permute(t.into());
                let si1 = move_table.raw_index_to_sym_index(p1.into());
                let si_rt = move_table.turn(si1.0, t.get_equivalent(&si1.1).invert());
                assert_eq!(si0.0, si_rt.0);
            }
        }

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        for pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            for t in all::<three_triangles_stack::Turns>() {
                let (ri_a, _) = move_table.raw_index_to_sym_index(pi);
                let (ri_b, s) = move_table.turn(ri_a, t);
                let (ri_rt, _ ) = move_table.turn(ri_b, t.invert().get_equivalent(&s));
                assert_eq!(ri_a, ri_rt);
            }
        }
    }

    use three_trapezoids;

    #[test]
    fn move_table_is_correct_for_three_trapezoids_without_symmetry() {
        test::<three_trapezoids::NoSymmetry, three_trapezoids::ThreeTrapezoidsIndex, three_trapezoids::Turns, three_trapezoids::ThreeTrapezoids>();
    }

    #[test]
    fn move_table_is_correct_for_three_trapezoids_with_mirror_ud_symmetry() {
        test::<three_trapezoids::MirrorUDSymmetry, three_trapezoids::ThreeTrapezoidsIndex, three_trapezoids::Turns, three_trapezoids::ThreeTrapezoids>();
    }

    #[test]
    fn move_table_is_correct_for_three_trapezoids_with_rotational_symmetry() {
        test::<three_trapezoids::RotationalSymmetry, three_trapezoids::ThreeTrapezoidsIndex, three_trapezoids::Turns, three_trapezoids::ThreeTrapezoids>();
    }

    #[test]
    fn move_table_is_correct_for_three_trapezoids_with_full_symmetry() {
        test::<three_trapezoids::FullSymmetry, three_trapezoids::ThreeTrapezoidsIndex, three_trapezoids::Turns, three_trapezoids::ThreeTrapezoids>();
    }

    fn test_on_pattern<Sym: PG + PartialEq + Sequence + Copy + Clone + Into<usize> + Invertable + std::fmt::Debug, PatternIndex: std::fmt::Debug + Sequence + Copy + Ord + TryFrom<usize> + Into<usize>, Turn: Sequence + Copy + PartialEq + Into<usize> + EquivalenceClass<Sym> + Invertable + Into<Perm>, Perm: PG, Pattern: GroupAction<Perm> + std::fmt::Debug + Eq + Clone + EquivalenceClass<Sym> + Into<PatternIndex>>() where PatternIndex: Into<Pattern>, <PatternIndex as TryFrom<usize>>::Error: std::fmt::Debug {
        let rep_table = Arc::new(RepresentativeTable::new::<Pattern>());
        // Even though either Left + Right generates all states, MoveTables
        // should basically always include turn inverses, so that they can go
        // forward or backwards.
        let move_table: MoveTable<Sym, PatternIndex, Turn> = MoveTable::new_on_pattern::<Perm, Pattern>(rep_table.clone());

        // Applying move_table moves is identical to applying permutations
        for ri in rep_table.rep_indexes() {
            let p: Pattern = rep_table.rep_index_to_perm_index(ri).into();
            for t in all::<Turn>() {
                let turn: Perm = t.into();
                let by_perm = p.clone().act(turn);
                let by_table = move_table.sym_index_to_raw_index(move_table.turn(ri, t)).into();
                assert_eq!(by_perm, by_table);
            }
        }

        // Raw to sym and sym to raw functions round trip
        for pi in all::<PatternIndex>() {
            let pi_rt = move_table.sym_index_to_raw_index(move_table.raw_index_to_sym_index(pi));
            assert_eq!(pi_rt, pi);
        }

        // All routes to a permutation result in the same representative (but
        // the sym coordinate is nondeterministic)
        for pi0 in all::<PatternIndex>() {
            let p0: Pattern = pi0.into();
            let si0 = move_table.raw_index_to_sym_index(pi0);
            for t in all::<Turn>() {
                let turn: Perm = t.into();
                let p1 = p0.clone().act(turn);
                let si1 = move_table.raw_index_to_sym_index(p1.into());
                let si_rt = move_table.turn(si1.0, t.get_equivalent(&si1.1).invert());
                assert_eq!(si0.0, si_rt.0);
            }
        }

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        for pi in all::<PatternIndex>() {
            for t in all::<Turn>() {
                let (ri_a, _) = move_table.raw_index_to_sym_index(pi);
                let (ri_b, s) = move_table.turn(ri_a, t);
                let (ri_rt, _ ) = move_table.turn(ri_b, t.invert().get_equivalent(&s));
                assert_eq!(ri_a, ri_rt);
            }
        }
    }

    #[test]
    fn move_table_is_correct_for_three_trapezoids_inner_with_no_symmetry() {
        test_on_pattern::<three_trapezoids::NoSymmetry, three_trapezoids::inner::ThreeTrapezoidsInnerIndex, three_trapezoids::Turns, three_trapezoids::ThreeTrapezoids, three_trapezoids::inner::ThreeTrapezoidsInner>();
    }

    #[test]
    fn move_table_is_correct_for_three_trapezoids_inner_with_mirror_ud_symmetry() {
        test_on_pattern::<three_trapezoids::MirrorUDSymmetry, three_trapezoids::inner::ThreeTrapezoidsInnerIndex, three_trapezoids::Turns, three_trapezoids::ThreeTrapezoids, three_trapezoids::inner::ThreeTrapezoidsInner>();
    }

    #[test]
    fn move_table_is_correct_for_three_trapezoids_inner_with_rotational_symmetry() {
        test_on_pattern::<three_trapezoids::RotationalSymmetry, three_trapezoids::inner::ThreeTrapezoidsInnerIndex, three_trapezoids::Turns, three_trapezoids::ThreeTrapezoids, three_trapezoids::inner::ThreeTrapezoidsInner>();
    }

    #[test]
    fn move_table_is_correct_for_three_trapezoids_inner_with_full_symmetry() {
        test_on_pattern::<three_trapezoids::FullSymmetry, three_trapezoids::inner::ThreeTrapezoidsInnerIndex, three_trapezoids::Turns, three_trapezoids::ThreeTrapezoids, three_trapezoids::inner::ThreeTrapezoidsInner>();
    }

    use three_trapezoids::outer;

    #[test]
    fn move_table_is_correct_for_three_trapezoids_outer_with_no_symmetry() {
        test_on_pattern::<three_trapezoids::NoSymmetry, three_trapezoids::outer::ThreeTrapezoidsOuterIndex, three_trapezoids::Turns, three_trapezoids::ThreeTrapezoids, three_trapezoids::outer::ThreeTrapezoidsOuter>();
    }

    #[test]
    fn move_table_is_correct_for_three_trapezoids_outer_with_mirror_ud_symmetry() {
        test_on_pattern::<three_trapezoids::MirrorUDSymmetry, three_trapezoids::outer::ThreeTrapezoidsOuterIndex, three_trapezoids::Turns, three_trapezoids::ThreeTrapezoids, three_trapezoids::outer::ThreeTrapezoidsOuter>();
    }

    #[test]
    fn move_table_is_correct_for_three_trapezoids_outer_with_rotational_symmetry() {
        test_on_pattern::<three_trapezoids::RotationalSymmetry, three_trapezoids::outer::ThreeTrapezoidsOuterIndex, three_trapezoids::Turns, three_trapezoids::ThreeTrapezoids, three_trapezoids::outer::ThreeTrapezoidsOuter>();
    }

    #[test]
    fn move_table_is_correct_for_three_trapezoids_outer_with_full_symmetry() {
        test_on_pattern::<three_trapezoids::FullSymmetry, three_trapezoids::outer::ThreeTrapezoidsOuterIndex, three_trapezoids::Turns, three_trapezoids::ThreeTrapezoids, three_trapezoids::outer::ThreeTrapezoidsOuter>();
    }
}
