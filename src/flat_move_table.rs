use permutation_group::PermutationGroup as PG;
use equivalence_class::EquivalenceClass;
use representative_table::{ RepresentativeTable, RepIndex };

use std::sync::Arc;
use std::convert::TryFrom;
use enum_iterator::{ Sequence, all, cardinality };

#[derive(Debug, Clone)]
pub struct MoveTable<Perm, Sym, PermIndex, Turn> {
    // TODO: Not even really sure we need this Arc, probably nothing else needs
    // to access this.  There are some upsides of making this an abstract T (we
    // don't propagate all the trait bounds), but the downside is that we need
    // a full trait definition of our RepTable to implement and error messages
    // get wierder.
    rep_table: Arc<RepresentativeTable<Perm, Sym, PermIndex>>,
    table: Vec<(RepIndex<PermIndex>, Sym)>,
    turns: std::marker::PhantomData<Turn>,
}

// TODO: Turn: Into<Perm> won't work for patterns (partial permutations).
// Instead we need some sort of Turnable trait, which can automatically be
// satisfied if Perm is a PermutationGroup, and Turn is Into<Perm> (which
// possibly it can implement too).
impl<Perm: PG + Clone + EquivalenceClass<Sym> + Into<PermIndex>, Turn: Sequence + Copy + Into<Perm> + PartialEq + Into<usize> + EquivalenceClass<Sym>, Sym: Sequence + Copy + Clone, PermIndex: Sequence + Copy + Ord + TryFrom<usize> + Into<usize> + Into<Perm>> MoveTable<Perm, Sym, PermIndex, Turn> where <PermIndex as TryFrom<usize>>::Error: std::fmt::Debug {
    pub fn new(rep_table: Arc<RepresentativeTable<Perm, Sym, PermIndex>>) -> Self {
        let mut table = Vec::with_capacity(rep_table.len() * cardinality::<Turn>());

        for ri in rep_table.rep_indexes() {
            let p = rep_table.rep_index_to_perm(ri);
            for t in all::<Turn>() {
                let turned: Perm = p.clone().permute(<Turn as Into<Perm>>::into(t));
                table.push(rep_table.perm_to_indexes(&turned));
            }
        }

        MoveTable {
            rep_table,
            table,
            turns: std::marker::PhantomData,
        }
    }

    pub fn turn(&self, ri: RepIndex<PermIndex>, t: Turn) -> (RepIndex<PermIndex>, Sym) {
        let i = <RepIndex<PermIndex> as Into<usize>>::into(ri) * cardinality::<Turn>() + <Turn as Into<usize>>::into(t);
        self.table[i]
    }

    // TODO: The value of this method is much more dubious, now that we can
    // just iterate over Turn.  This _might_ be more performant, but by how
    // much could it really be anyway?
    pub fn apply_turns(&self, ri: RepIndex<PermIndex>) -> impl Iterator<Item = (RepIndex<PermIndex>, Sym)> + '_ {
        let start = <RepIndex<PermIndex> as Into<usize>>::into(ri) * cardinality::<Turn>();
        (start..(start + cardinality::<Turn>())).map(move |i| self.table[i])
    }

    // Count of Reps
    pub fn len(&self) -> usize {
        self.rep_table.len()
    }

    pub fn perm_to_indexes(&self, perm: &Perm) -> (RepIndex<PermIndex>, Sym) {
        self.rep_table.perm_to_indexes(perm)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use two_triangles::*;

    // TODO: Time to parameterize these tests
    #[test]
    fn move_table_is_correct_for_two_triangles_without_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new());
        // Even though either Left + Right generates all states, MoveTables
        // should basically always include turn inverses, so that they can go
        // forward or backwards.
        let move_table: MoveTable<TwoTriangles, NoSymmetry, TwoTrianglesIndex, Turns> = MoveTable::new(rep_table.clone());

        // Applying move_table moves is identical to applying permutations
        for ri in rep_table.rep_indexes() {
            let p: TwoTriangles = rep_table.rep_index_to_perm(ri);
            for t in all::<Turns>() {
                let by_perm = p.permute(t.into());
                let (ri, sym) = move_table.turn(ri, t);
                let before_sym = rep_table.rep_index_to_perm(ri);
                let by_table = before_sym.get_equivalent(&sym);
                assert_eq!(by_perm, by_table);
            }
        }

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        for pi in all::<TwoTrianglesIndex>() {
            let p = pi.into();
            for t in all::<Turns>() {
                let (ri_a, _) = move_table.perm_to_indexes(&p);
                let (ri_b, _) = move_table.turn(ri_a, t);
                let mut found = false;
                for t in all::<Turns>() {
                    let (ri_rt, _ ) = move_table.turn(ri_b, t);
                    found |= ri_a == ri_rt;
                }
                assert_eq!(found, true);
            }
        }
    }

    // TODO: There are many more valid combinations of turns and symmetries if
    // we only use one symmetry.
    #[test]
    fn move_table_is_correct_for_two_triangles_with_rotational_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new());
        // Even though just Left + Right is sufficient and symmetric,
        // MoveTables should basically always include turn inverses, so that
        // they can go forward or backwards.
        let move_table: MoveTable<TwoTriangles, RotationalSymmetry, TwoTrianglesIndex, Turns> = MoveTable::new(rep_table.clone());

        // Applying move_table moves is identical to applying permutations
        for ri in rep_table.rep_indexes() {
            let p: TwoTriangles = rep_table.rep_index_to_perm(ri);
            for t in all::<Turns>() {
                let by_perm = p.permute(t.into());
                let (ri, sym) = move_table.turn(ri, t);
                let before_sym = rep_table.rep_index_to_perm(ri);
                let by_table = before_sym.get_equivalent(&sym);
                assert_eq!(by_perm, by_table);
            }
        }

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        for pi in all::<TwoTrianglesIndex>() {
            let p = pi.into();
            for t in all::<Turns>() {
                let (ri_a, _) = move_table.perm_to_indexes(&p);
                let (ri_b, _) = move_table.turn(ri_a, t);
                let mut found = false;
                for t in all::<Turns>() {
                    let (ri_rt, _ ) = move_table.turn(ri_b, t);
                    found |= ri_a == ri_rt;
                }
                assert_eq!(found, true);
            }
        }
    }

    #[test]
    fn move_table_is_correct_for_two_triangles_with_full_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new());
        let move_table: MoveTable<TwoTriangles, FullSymmetry, TwoTrianglesIndex, Turns> = MoveTable::new(rep_table.clone());

        // Applying move_table moves is identical to applying permutations
        for ri in rep_table.rep_indexes() {
            let p: TwoTriangles = rep_table.rep_index_to_perm(ri);
            for t in all::<Turns>() {
                let by_perm = p.permute(t.into());
                let (ri, sym) = move_table.turn(ri, t);
                let before_sym = rep_table.rep_index_to_perm(ri);
                let by_table = before_sym.get_equivalent(&sym);
                assert_eq!(by_perm, by_table);
            }
        }

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        for pi in all::<TwoTrianglesIndex>() {
            let p = pi.into();
            for t in all::<Turns>() {
                let (ri_a, _) = move_table.perm_to_indexes(&p);
                let (ri_b, _) = move_table.turn(ri_a, t);
                let mut found = false;
                for t in all::<Turns>() {
                    let (ri_rt, _ ) = move_table.turn(ri_b, t);
                    found |= ri_a == ri_rt;
                }
                assert_eq!(found, true);
            }
        }
    }

    #[test]
    fn move_table_is_correct_for_two_triangles_even_parity_without_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new());
        // Even though either Left + Right generates all states, MoveTables
        // should basically always include turn inverses, so that they can go
        // forward or backwards.
        let move_table: MoveTable<TwoTriangles, NoSymmetry, TwoTrianglesEvenIndex, Turns> = MoveTable::new(rep_table.clone());

        // Applying move_table moves is identical to applying permutations
        for ri in rep_table.rep_indexes() {
            let p: TwoTriangles = rep_table.rep_index_to_perm(ri);
            for t in all::<Turns>() {
                let by_perm = p.permute(t.into());
                let (ri, sym) = move_table.turn(ri, t);
                let before_sym = rep_table.rep_index_to_perm(ri);
                let by_table = before_sym.get_equivalent(&sym);
                assert_eq!(by_perm, by_table);
            }
        }

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        for pi in all::<TwoTrianglesEvenIndex>() {
            let p = pi.into();
            for t in all::<Turns>() {
                let (ri_a, _) = move_table.perm_to_indexes(&p);
                let (ri_b, _) = move_table.turn(ri_a, t);
                let mut found = false;
                for t in all::<Turns>() {
                    let (ri_rt, _ ) = move_table.turn(ri_b, t);
                    found |= ri_a == ri_rt;
                }
                assert_eq!(found, true);
            }
        }
    }

    #[test]
    fn move_table_is_correct_for_two_triangles_even_parity_with_rotational_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new());
        // Even though just Left + Right is sufficient and symmetric,
        // MoveTables should basically always include turn inverses, so that
        // they can go forward or backwards.
        let move_table: MoveTable<TwoTriangles, RotationalSymmetry, TwoTrianglesIndex, Turns> = MoveTable::new(rep_table.clone());

        // Applying move_table moves is identical to applying permutations
        for ri in rep_table.rep_indexes() {
            let p: TwoTriangles = rep_table.rep_index_to_perm(ri);
            for t in all::<Turns>() {
                let by_perm = p.permute(t.into());
                let (ri, sym) = move_table.turn(ri, t);
                let before_sym = rep_table.rep_index_to_perm(ri);
                let by_table = before_sym.get_equivalent(&sym);
                assert_eq!(by_perm, by_table);
            }
        }

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        for pi in all::<TwoTrianglesEvenIndex>() {
            let p = pi.into();
            for t in all::<Turns>() {
                let (ri_a, _) = move_table.perm_to_indexes(&p);
                let (ri_b, _) = move_table.turn(ri_a, t);
                let mut found = false;
                for t in all::<Turns>() {
                    let (ri_rt, _ ) = move_table.turn(ri_b, t);
                    found |= ri_a == ri_rt;
                }
                assert_eq!(found, true);
            }
        }
    }

    #[test]
    fn move_table_is_correct_for_two_triangles_even_parity_with_full_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new());
        let move_table: MoveTable<TwoTriangles, FullSymmetry, TwoTrianglesIndex, Turns> = MoveTable::new(rep_table.clone());

        // Applying move_table moves is identical to applying permutations
        for ri in rep_table.rep_indexes() {
            let p: TwoTriangles = rep_table.rep_index_to_perm(ri);
            for t in all::<Turns>() {
                let by_perm = p.permute(t.into());
                let (ri, sym) = move_table.turn(ri, t);
                let before_sym = rep_table.rep_index_to_perm(ri);
                let by_table = before_sym.get_equivalent(&sym);
                assert_eq!(by_perm, by_table);
            }
        }

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        for pi in all::<TwoTrianglesEvenIndex>() {
            let p = pi.into();
            for t in all::<Turns>() {
                let (ri_a, _) = move_table.perm_to_indexes(&p);
                let (ri_b, _) = move_table.turn(ri_a, t);
                let mut found = false;
                for t in all::<Turns>() {
                    let (ri_rt, _ ) = move_table.turn(ri_b, t);
                    found |= ri_a == ri_rt;
                }
                assert_eq!(found, true);
            }
        }
    }
}
