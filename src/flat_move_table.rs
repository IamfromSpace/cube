use permutation_group::PermutationGroup as PG;
use invertable::Invertable;
use equivalence_class::EquivalenceClass;
use representative_table::{ RepresentativeTable, RepIndex };

use std::sync::Arc;
use std::convert::TryFrom;
use enum_iterator::{ Sequence, all, cardinality };

#[derive(Debug)]
pub struct MoveTable<Perm, Sym, PermIndex, Turn> {
    // TODO: Not even really sure we need this Arc, probably nothing else needs
    // to access this.  There are some upsides of making this an abstract T (we
    // don't propagate all the trait bounds), but the downside is that we need
    // a full trait definition of our RepTable to implement and error messages
    // get wierder.
    rep_table: Arc<RepresentativeTable<Perm, Sym, PermIndex>>,
    turn_table: Vec<(RepIndex<PermIndex>, Sym)>,
    sym_table: Vec<PermIndex>,
    turns: std::marker::PhantomData<Turn>,
}

// TODO: Turn: Into<Perm> won't work for patterns (partial permutations).
// Instead we need some sort of Turnable trait, which can automatically be
// satisfied if Perm is a PermutationGroup, and Turn is Into<Perm> (which
// possibly it can implement too).
// TODO: If the PermIndex is even all Turns must be even too, and if the
// PermIndex is odd then at least one Turn in the set must also be odd.  Can
// this be expressed through Traits to guarantee a match?
impl<Perm: PG + Clone + EquivalenceClass<Sym> + Into<PermIndex>, Turn: Sequence + Copy + Into<Perm> + PartialEq + Into<usize> + EquivalenceClass<Sym>, Sym: Sequence + Copy + Clone + Into<usize> + Invertable, PermIndex: Sequence + Copy + Ord + TryFrom<usize> + Into<usize> + Into<Perm>> MoveTable<Perm, Sym, PermIndex, Turn> where <PermIndex as TryFrom<usize>>::Error: std::fmt::Debug {
    // TODO: We can have an alterative method that automatically generates the
    // RepTable to simplify cases where it isn't shared with other MoveTables.
    pub fn new(rep_table: Arc<RepresentativeTable<Perm, Sym, PermIndex>>) -> Self {
        let mut turn_table = Vec::with_capacity(rep_table.len() * cardinality::<Turn>());
        let mut sym_table = Vec::with_capacity(rep_table.len() * cardinality::<Sym>());

        for ri in rep_table.rep_indexes() {
            let p = rep_table.rep_index_to_perm(ri);
            for t in all::<Turn>() {
                let turned: Perm = p.clone().permute(<Turn as Into<Perm>>::into(t));
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

    pub fn turn(&self, ri: RepIndex<PermIndex>, t: Turn) -> (RepIndex<PermIndex>, Sym) {
        let i = <RepIndex<PermIndex> as Into<usize>>::into(ri) * cardinality::<Turn>() + <Turn as Into<usize>>::into(t);
        self.turn_table[i]
    }

    // TODO: The value of this method is much more dubious, now that we can
    // just iterate over Turn.  This _might_ be more performant, but by how
    // much could it really be anyway?
    pub fn apply_turns(&self, ri: RepIndex<PermIndex>) -> impl Iterator<Item = (RepIndex<PermIndex>, Sym)> + '_ {
        let start = <RepIndex<PermIndex> as Into<usize>>::into(ri) * cardinality::<Turn>();
        (start..(start + cardinality::<Turn>())).map(move |i| self.turn_table[i])
    }

    // Count of Reps
    pub fn len(&self) -> usize {
        self.rep_table.len()
    }

    pub fn raw_index_to_sym_index(&self, pi: PermIndex) -> (RepIndex<PermIndex>, Sym) {
        self.rep_table.raw_index_to_sym_index(pi)
    }

    pub fn sym_index_to_raw_index(&self, si: (RepIndex<PermIndex>, Sym)) -> PermIndex {
        let (ri, s) = si;
        let i = <RepIndex<PermIndex> as Into<usize>>::into(ri) * cardinality::<Sym>() + <Sym as Into<usize>>::into(s);
        self.sym_table[i]
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
                let by_table = move_table.sym_index_to_raw_index(move_table.turn(ri, t)).into();
                assert_eq!(by_perm, by_table);
            }
        }

        // Raw to sym and sym to raw functions round trip
        for pi in all::<TwoTrianglesIndex>() {
            let pi_rt = move_table.sym_index_to_raw_index(move_table.raw_index_to_sym_index(pi));
            assert_eq!(pi_rt, pi);
        }

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        for pi in all::<TwoTrianglesIndex>() {
            for t in all::<Turns>() {
                let (ri_a, _) = move_table.raw_index_to_sym_index(pi);
                let (ri_b, _) = move_table.turn(ri_a, t);
                let mut found = false;
                for t in all::<Turns>() {
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
                let by_table = move_table.sym_index_to_raw_index(move_table.turn(ri, t)).into();
                assert_eq!(by_perm, by_table);
            }
        }

        // Raw to sym and sym to raw functions round trip
        for pi in all::<TwoTrianglesIndex>() {
            let pi_rt = move_table.sym_index_to_raw_index(move_table.raw_index_to_sym_index(pi));
            assert_eq!(pi_rt, pi);
        }

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        for pi in all::<TwoTrianglesIndex>() {
            for t in all::<Turns>() {
                let (ri_a, _) = move_table.raw_index_to_sym_index(pi);
                let (ri_b, _) = move_table.turn(ri_a, t);
                let mut found = false;
                for t in all::<Turns>() {
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
    fn move_table_is_correct_for_two_triangles_with_full_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new());
        let move_table: MoveTable<TwoTriangles, FullSymmetry, TwoTrianglesIndex, Turns> = MoveTable::new(rep_table.clone());

        // Applying move_table moves is identical to applying permutations
        for ri in rep_table.rep_indexes() {
            let p: TwoTriangles = rep_table.rep_index_to_perm(ri);
            for t in all::<Turns>() {
                let by_perm = p.permute(t.into());
                let by_table = move_table.sym_index_to_raw_index(move_table.turn(ri, t)).into();
                assert_eq!(by_perm, by_table);
            }
        }

        // Raw to sym and sym to raw functions round trip
        for pi in all::<TwoTrianglesIndex>() {
            let pi_rt = move_table.sym_index_to_raw_index(move_table.raw_index_to_sym_index(pi));
            assert_eq!(pi_rt, pi);
        }

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        for pi in all::<TwoTrianglesIndex>() {
            for t in all::<Turns>() {
                let (ri_a, _) = move_table.raw_index_to_sym_index(pi);
                let (ri_b, _) = move_table.turn(ri_a, t);
                let mut found = false;
                for t in all::<Turns>() {
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
                let by_table = move_table.sym_index_to_raw_index(move_table.turn(ri, t)).into();
                assert_eq!(by_perm, by_table);
            }
        }

        // Raw to sym and sym to raw functions round trip
        for pi in all::<TwoTrianglesEvenIndex>() {
            let pi_rt = move_table.sym_index_to_raw_index(move_table.raw_index_to_sym_index(pi));
            assert_eq!(pi_rt, pi);
        }

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        for pi in all::<TwoTrianglesEvenIndex>() {
            for t in all::<Turns>() {
                let (ri_a, _) = move_table.raw_index_to_sym_index(pi);
                let (ri_b, _) = move_table.turn(ri_a, t);
                let mut found = false;
                for t in all::<Turns>() {
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
    fn move_table_is_correct_for_two_triangles_even_parity_with_rotational_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new());
        // Even though just Left + Right is sufficient and symmetric,
        // MoveTables should basically always include turn inverses, so that
        // they can go forward or backwards.
        let move_table: MoveTable<TwoTriangles, RotationalSymmetry, TwoTrianglesEvenIndex, Turns> = MoveTable::new(rep_table.clone());

        // Applying move_table moves is identical to applying permutations
        for ri in rep_table.rep_indexes() {
            let p: TwoTriangles = rep_table.rep_index_to_perm(ri);
            for t in all::<Turns>() {
                let by_perm = p.permute(t.into());
                let by_table = move_table.sym_index_to_raw_index(move_table.turn(ri, t)).into();
                assert_eq!(by_perm, by_table);
            }
        }

        // Raw to sym and sym to raw functions round trip
        for pi in all::<TwoTrianglesEvenIndex>() {
            let pi_rt = move_table.sym_index_to_raw_index(move_table.raw_index_to_sym_index(pi));
            assert_eq!(pi_rt, pi);
        }

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        for pi in all::<TwoTrianglesEvenIndex>() {
            for t in all::<Turns>() {
                let (ri_a, _) = move_table.raw_index_to_sym_index(pi);
                let (ri_b, _) = move_table.turn(ri_a, t);
                let mut found = false;
                for t in all::<Turns>() {
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
    fn move_table_is_correct_for_two_triangles_even_parity_with_full_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new());
        let move_table: MoveTable<TwoTriangles, FullSymmetry, TwoTrianglesEvenIndex, Turns> = MoveTable::new(rep_table.clone());

        // Applying move_table moves is identical to applying permutations
        for ri in rep_table.rep_indexes() {
            let p: TwoTriangles = rep_table.rep_index_to_perm(ri);
            for t in all::<Turns>() {
                let by_perm = p.permute(t.into());
                let by_table = move_table.sym_index_to_raw_index(move_table.turn(ri, t)).into();
                assert_eq!(by_perm, by_table);
            }
        }

        // Raw to sym and sym to raw functions round trip
        for pi in all::<TwoTrianglesEvenIndex>() {
            let pi_rt = move_table.sym_index_to_raw_index(move_table.raw_index_to_sym_index(pi));
            assert_eq!(pi_rt, pi);
        }

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        for pi in all::<TwoTrianglesEvenIndex>() {
            for t in all::<Turns>() {
                let (ri_a, _) = move_table.raw_index_to_sym_index(pi);
                let (ri_b, _) = move_table.turn(ri_a, t);
                let mut found = false;
                for t in all::<Turns>() {
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

    use two_lines;

    #[test]
    fn move_table_is_correct_for_two_lines_without_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new());
        // Even though either Left + Right generates all states, MoveTables
        // should basically always include turn inverses, so that they can go
        // forward or backwards.
        let move_table: MoveTable<two_lines::TwoLines, two_lines::NoSymmetry, two_lines::TwoLinesIndex, two_lines::Turns> = MoveTable::new(rep_table.clone());

        // Applying move_table moves is identical to applying permutations
        for ri in rep_table.rep_indexes() {
            let p: two_lines::TwoLines = rep_table.rep_index_to_perm(ri);
            for t in all::<two_lines::Turns>() {
                let by_perm = p.permute(t.into());
                let by_table = move_table.sym_index_to_raw_index(move_table.turn(ri, t)).into();
                assert_eq!(by_perm, by_table);
            }
        }

        // Raw to sym and sym to raw functions round trip
        for pi in all::<two_lines::TwoLinesIndex>() {
            let pi_rt = move_table.sym_index_to_raw_index(move_table.raw_index_to_sym_index(pi));
            assert_eq!(pi_rt, pi);
        }

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        for pi in all::<two_lines::TwoLinesIndex>() {
            for t in all::<two_lines::Turns>() {
                let (ri_a, _) = move_table.raw_index_to_sym_index(pi);
                let (ri_b, _) = move_table.turn(ri_a, t);
                let mut found = false;
                for t in all::<two_lines::Turns>() {
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
    fn move_table_is_correct_for_two_lines_with_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new());
        // Even though either Left + Right generates all states, MoveTables
        // should basically always include turn inverses, so that they can go
        // forward or backwards.
        let move_table: MoveTable<two_lines::TwoLines, two_lines::FullSymmetry, two_lines::TwoLinesIndex, two_lines::Turns> = MoveTable::new(rep_table.clone());

        // Applying move_table moves is identical to applying permutations
        for ri in rep_table.rep_indexes() {
            let p: two_lines::TwoLines = rep_table.rep_index_to_perm(ri);
            for t in all::<two_lines::Turns>() {
                let by_perm = p.permute(t.into());
                let by_table = move_table.sym_index_to_raw_index(move_table.turn(ri, t)).into();
                assert_eq!(by_perm, by_table);
            }
        }

        // Raw to sym and sym to raw functions round trip
        for pi in all::<two_lines::TwoLinesIndex>() {
            let pi_rt = move_table.sym_index_to_raw_index(move_table.raw_index_to_sym_index(pi));
            assert_eq!(pi_rt, pi);
        }

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        for pi in all::<two_lines::TwoLinesIndex>() {
            for t in all::<two_lines::Turns>() {
                let (ri_a, _) = move_table.raw_index_to_sym_index(pi);
                let (ri_b, _) = move_table.turn(ri_a, t);
                let mut found = false;
                for t in all::<two_lines::Turns>() {
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

    use three_triangles;

    #[test]
    fn move_table_is_correct_for_three_triangles_without_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new());
        // Even though either Left + Right generates all states, MoveTables
        // should basically always include turn inverses, so that they can go
        // forward or backwards.
        let move_table: MoveTable<three_triangles::ThreeTriangles, three_triangles::NoSymmetry, three_triangles::ThreeTrianglesIndex, three_triangles::Turns> = MoveTable::new(rep_table.clone());

        // Applying move_table moves is identical to applying permutations
        for ri in rep_table.rep_indexes() {
            let p: three_triangles::ThreeTriangles = rep_table.rep_index_to_perm(ri);
            for t in all::<three_triangles::Turns>() {
                let by_perm = p.permute(t.into());
                let by_table = move_table.sym_index_to_raw_index(move_table.turn(ri, t)).into();
                assert_eq!(by_perm, by_table);
            }
        }

        // Raw to sym and sym to raw functions round trip
        for pi in all::<three_triangles::ThreeTrianglesIndex>() {
            let pi_rt = move_table.sym_index_to_raw_index(move_table.raw_index_to_sym_index(pi));
            assert_eq!(pi_rt, pi);
        }

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        for pi in all::<three_triangles::ThreeTrianglesIndex>() {
            for t in all::<three_triangles::Turns>() {
                let (ri_a, _) = move_table.raw_index_to_sym_index(pi);
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
    fn move_table_is_correct_for_three_triangles_with_rotational_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new());
        // Even though either Left + Right generates all states, MoveTables
        // should basically always include turn inverses, so that they can go
        // forward or backwards.
        let move_table: MoveTable<three_triangles::ThreeTriangles, three_triangles::RotationalSymmetry, three_triangles::ThreeTrianglesIndex, three_triangles::Turns> = MoveTable::new(rep_table.clone());

        // Applying move_table moves is identical to applying permutations
        for ri in rep_table.rep_indexes() {
            let p: three_triangles::ThreeTriangles = rep_table.rep_index_to_perm(ri);
            for t in all::<three_triangles::Turns>() {
                let by_perm = p.permute(t.into());
                let by_table = move_table.sym_index_to_raw_index(move_table.turn(ri, t)).into();
                assert_eq!(by_perm, by_table);
            }
        }

        // Raw to sym and sym to raw functions round trip
        for pi in all::<three_triangles::ThreeTrianglesIndex>() {
            let pi_rt = move_table.sym_index_to_raw_index(move_table.raw_index_to_sym_index(pi));
            assert_eq!(pi_rt, pi);
        }

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        for pi in all::<three_triangles::ThreeTrianglesIndex>() {
            for t in all::<three_triangles::Turns>() {
                let (ri_a, _) = move_table.raw_index_to_sym_index(pi);
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
    fn move_table_is_correct_for_three_triangles_with_full_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new());
        // Even though either Left + Right generates all states, MoveTables
        // should basically always include turn inverses, so that they can go
        // forward or backwards.
        let move_table: MoveTable<three_triangles::ThreeTriangles, three_triangles::FullSymmetry, three_triangles::ThreeTrianglesIndex, three_triangles::Turns> = MoveTable::new(rep_table.clone());

        // Applying move_table moves is identical to applying permutations
        for ri in rep_table.rep_indexes() {
            let p: three_triangles::ThreeTriangles = rep_table.rep_index_to_perm(ri);
            for t in all::<three_triangles::Turns>() {
                let by_perm = p.permute(t.into());
                let by_table = move_table.sym_index_to_raw_index(move_table.turn(ri, t)).into();
                assert_eq!(by_perm, by_table);
            }
        }

        // Raw to sym and sym to raw functions round trip
        for pi in all::<three_triangles::ThreeTrianglesIndex>() {
            let pi_rt = move_table.sym_index_to_raw_index(move_table.raw_index_to_sym_index(pi));
            assert_eq!(pi_rt, pi);
        }

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        for pi in all::<three_triangles::ThreeTrianglesIndex>() {
            for t in all::<three_triangles::Turns>() {
                let (ri_a, _) = move_table.raw_index_to_sym_index(pi);
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
    fn move_table_is_correct_for_three_triangles_even_parity_with_rotational_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new());
        // Even though either Left + Right generates all states, MoveTables
        // should basically always include turn inverses, so that they can go
        // forward or backwards.
        let move_table: MoveTable<three_triangles::ThreeTriangles, three_triangles::RotationalSymmetry, three_triangles::ThreeTrianglesEvenIndex, three_triangles::Turns> = MoveTable::new(rep_table.clone());

        // Applying move_table moves is identical to applying permutations
        for ri in rep_table.rep_indexes() {
            let p: three_triangles::ThreeTriangles = rep_table.rep_index_to_perm(ri);
            for t in all::<three_triangles::Turns>() {
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

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        for pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            for t in all::<three_triangles::Turns>() {
                let (ri_a, _) = move_table.raw_index_to_sym_index(pi);
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
    fn move_table_is_correct_for_three_triangles_even_parity_without_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new());
        // Even though either Left + Right generates all states, MoveTables
        // should basically always include turn inverses, so that they can go
        // forward or backwards.
        let move_table: MoveTable<three_triangles::ThreeTriangles, three_triangles::NoSymmetry, three_triangles::ThreeTrianglesEvenIndex, three_triangles::Turns> = MoveTable::new(rep_table.clone());

        // Applying move_table moves is identical to applying permutations
        for ri in rep_table.rep_indexes() {
            let p: three_triangles::ThreeTriangles = rep_table.rep_index_to_perm(ri);
            for t in all::<three_triangles::Turns>() {
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

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        for pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            for t in all::<three_triangles::Turns>() {
                let (ri_a, _) = move_table.raw_index_to_sym_index(pi);
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
    fn move_table_is_correct_for_three_triangles_even_parity_with_full_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new());
        // Even though either Left + Right generates all states, MoveTables
        // should basically always include turn inverses, so that they can go
        // forward or backwards.
        let move_table: MoveTable<three_triangles::ThreeTriangles, three_triangles::FullSymmetry, three_triangles::ThreeTrianglesEvenIndex, three_triangles::Turns> = MoveTable::new(rep_table.clone());

        // Applying move_table moves is identical to applying permutations
        for ri in rep_table.rep_indexes() {
            let p: three_triangles::ThreeTriangles = rep_table.rep_index_to_perm(ri);
            for t in all::<three_triangles::Turns>() {
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

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        for pi in all::<three_triangles::ThreeTrianglesEvenIndex>() {
            for t in all::<three_triangles::Turns>() {
                let (ri_a, _) = move_table.raw_index_to_sym_index(pi);
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
}
