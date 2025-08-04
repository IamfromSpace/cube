use permutation_group::PermutationGroup as PG;
use invertable::Invertable;
use equivalence_class::EquivalenceClass;
use representative_table::{ RepresentativeTable, RepIndex, SymIndex };

use std::sync::Arc;
use std::convert::TryFrom;

// Opaque type to prevent accidental misuse
#[derive(Debug, Clone, Copy)]
pub struct TurnIndex(u8);

#[derive(Debug, Clone)]
pub struct MoveTable<Perm, Sym, PermIndex, Turn> {
    // TODO: Not even really sure we need this Arc, probably nothing else needs
    // to access this.  There are some upsides of making this an abstract T (we
    // don't propagate all the trait bounds), but the downside is that we need
    // a full trait definition of our RepTable to implement and error messages
    // get wierder.
    rep_table: Arc<RepresentativeTable<Perm, Sym, PermIndex>>,
    table: Vec<(RepIndex<PermIndex>, SymIndex)>,
    turns: Vec<Turn>,
}

// TODO: Turn: Into<Perm> won't work for patterns (partial permutations).
// Instead we need some sort of Turnable trait, which can automatically be
// satisfied if Perm is a PermutationGroup, and Turn is Into<Perm> (which
// possibly it can implement too).
impl<Perm: PG + Clone + EquivalenceClass<Sym> + Into<PermIndex>, Turn: Copy + Into<Perm> + PartialEq, Sym: Clone, PermIndex: Copy + Ord + TryFrom<usize> + Into<usize> + Into<Perm>> MoveTable<Perm, Sym, PermIndex, Turn> where <PermIndex as TryFrom<usize>>::Error: std::fmt::Debug {
    pub fn new(turns: Vec<Turn>, rep_table: Arc<RepresentativeTable<Perm, Sym, PermIndex>>) -> Self {
        let mut table = Vec::with_capacity(rep_table.len() * turns.len());

        for ri in rep_table.rep_indexes() {
            let p = rep_table.rep_index_to_perm(ri);
            for ti in 0..turns.len() {
                let t = turns[ti];
                let turned: Perm = p.clone().permute(<Turn as Into<Perm>>::into(t));
                table.push(rep_table.perm_to_indexes(&turned));
            }
        }

        MoveTable {
            rep_table,
            table,
            turns,
        }
    }

    pub fn turn_to_turn_index(&self, t: Turn) -> TurnIndex {
        TurnIndex(self.turns.iter().position(|x| *x == t).unwrap() as u8)
    }

    pub fn turn_index_to_turn(&self, ti: TurnIndex) -> Turn {
        self.turns[ti.0 as usize]
    }

    pub fn turn(&self, ri: RepIndex<PermIndex>, ti: TurnIndex) -> (RepIndex<PermIndex>, SymIndex) {
        let i = <RepIndex<PermIndex> as Into<usize>>::into(ri) * self.turns.len() + ti.0 as usize;
        self.table[i]
    }

    pub fn apply_turns(&self, ri: RepIndex<PermIndex>) -> impl Iterator<Item = (RepIndex<PermIndex>, SymIndex)> + '_ {
        let start = <RepIndex<PermIndex> as Into<usize>>::into(ri) * self.turns.len();
        (start..(start + self.turns.len())).map(move |i| self.table[i])
    }

    // Count of Reps
    pub fn len(&self) -> usize {
        self.rep_table.len()
    }

    pub fn perm_to_indexes(&self, perm: &Perm) -> (RepIndex<PermIndex>, SymIndex) {
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
        // Even though either Left + Right generates all states, MoveTables
        // should basically always include turn inverses, so that they can go
        // forward or backwards.
        let turns = vec![Turns::Left, Turns::LeftPrime, Turns::Right, Turns::RightPrime];
        let syms = vec![];
        let all_perms = (0..120u8).map(|i| i.into());

        let rep_table: RepresentativeTable<TwoTriangles, Sym, u8> = RepresentativeTable::new(syms, all_perms.clone());
        let rep_table = Arc::new(rep_table);
        let move_table: MoveTable<TwoTriangles, Sym, u8, Turns> = MoveTable::new(turns.clone(), rep_table.clone());

        // Applying move_table moves is identical to applying permutations
        for ri in rep_table.rep_indexes() {
            let p: TwoTriangles = rep_table.rep_index_to_perm(ri);
            for t in &turns {
                let by_perm = p.permute((*t).into());

                let ti = move_table.turn_to_turn_index(*t);
                let (ri, si) = move_table.turn(ri, ti);
                let before_sym = rep_table.rep_index_to_perm(ri);
                let by_table = match rep_table.sym_index_to_sym(si) {
                    None => before_sym,
                    Some(sym) => before_sym.get_equivalent(&sym),
                };
                assert_eq!(by_perm, by_table);
            }
        }

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        for p in all_perms {
            for t in &turns {
                let ti = move_table.turn_to_turn_index(*t);
                let (ri_a, _) = move_table.perm_to_indexes(&p);
                let (ri_b, _) = move_table.turn(ri_a, ti);
                let mut found = false;
                for t in &turns {
                    let ti_inv = move_table.turn_to_turn_index(*t);
                    let (ri_rt, _ ) = move_table.turn(ri_b, ti_inv);
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
        // Even though just Left + Right is sufficient and symmetric,
        // MoveTables should basically always include turn inverses, so that
        // they can go forward or backwards.
        let turns = vec![Turns::Left, Turns::LeftPrime, Turns::Right, Turns::RightPrime];
        let syms = vec![Sym::MirrorBoth];
        let all_perms = (0..120u8).map(|i| i.into());

        let rep_table: RepresentativeTable<TwoTriangles, Sym, u8> = RepresentativeTable::new(syms, all_perms.clone());
        let rep_table = Arc::new(rep_table);
        let move_table: MoveTable<TwoTriangles, Sym, u8, Turns> = MoveTable::new(turns.clone(), rep_table.clone());

        // Applying move_table moves is identical to applying permutations
        for ri in rep_table.rep_indexes() {
            let p: TwoTriangles = rep_table.rep_index_to_perm(ri);
            for t in &turns {
                let by_perm = p.permute((*t).into());

                let ti = move_table.turn_to_turn_index(*t);
                let (ri, si) = move_table.turn(ri, ti);
                let before_sym = rep_table.rep_index_to_perm(ri);
                let by_table = match rep_table.sym_index_to_sym(si) {
                    None => before_sym,
                    Some(sym) => before_sym.get_equivalent(&sym),
                };
                assert_eq!(by_perm, by_table);
            }
        }

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        for p in all_perms {
            for t in &turns {
                let ti = move_table.turn_to_turn_index(*t);
                let (ri_a, _) = move_table.perm_to_indexes(&p);
                let (ri_b, _) = move_table.turn(ri_a, ti);
                let mut found = false;
                for t in &turns {
                    let ti_inv = move_table.turn_to_turn_index(*t);
                    let (ri_rt, _ ) = move_table.turn(ri_b, ti_inv);
                    found |= ri_a == ri_rt;
                }
                assert_eq!(found, true);
            }
        }
    }

    #[test]
    fn move_table_is_correct_for_two_triangles_with_full_symmetry() {
        let turns = vec![Turns::Left, Turns::LeftPrime, Turns::Right, Turns::RightPrime];
        let syms = vec![Sym::MirrorLR, Sym::MirrorTD, Sym::MirrorBoth];
        let all_perms = (0..120u8).map(|i| i.into());

        let rep_table: RepresentativeTable<TwoTriangles, Sym, u8> = RepresentativeTable::new(syms, all_perms.clone());
        let rep_table = Arc::new(rep_table);
        let move_table: MoveTable<TwoTriangles, Sym, u8, Turns> = MoveTable::new(turns.clone(), rep_table.clone());

        // Applying move_table moves is identical to applying permutations
        for ri in rep_table.rep_indexes() {
            let p: TwoTriangles = rep_table.rep_index_to_perm(ri);
            for t in &turns {
                let by_perm = p.permute((*t).into());

                let ti = move_table.turn_to_turn_index(*t);
                let (ri, si) = move_table.turn(ri, ti);
                let before_sym = rep_table.rep_index_to_perm(ri);
                let by_table = match rep_table.sym_index_to_sym(si) {
                    None => before_sym,
                    Some(sym) => before_sym.get_equivalent(&sym),
                };
                assert_eq!(by_perm, by_table);
            }
        }

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        for p in all_perms {
            for t in &turns {
                let ti = move_table.turn_to_turn_index(*t);
                let (ri_a, _) = move_table.perm_to_indexes(&p);
                let (ri_b, _) = move_table.turn(ri_a, ti);
                let mut found = false;
                for t in &turns {
                    let ti_inv = move_table.turn_to_turn_index(*t);
                    let (ri_rt, _ ) = move_table.turn(ri_b, ti_inv);
                    found |= ri_a == ri_rt;
                }
                assert_eq!(found, true);
            }
        }
    }

    #[test]
    fn move_table_is_correct_for_two_triangles_even_parity_without_symmetry() {
        // Even though either Left + Right generates all states, MoveTables
        // should basically always include turn inverses, so that they can go
        // forward or backwards.
        let turns = vec![Turns::Left, Turns::LeftPrime, Turns::Right, Turns::RightPrime];
        let syms = vec![];
        let all_perms = (0..120u8).map(|i| i.into()).filter(|t: &TwoTriangles| t.is_even_parity());

        let rep_table: RepresentativeTable<TwoTriangles, Sym, u8> = RepresentativeTable::new(syms, all_perms.clone());
        let rep_table = Arc::new(rep_table);
        let move_table: MoveTable<TwoTriangles, Sym, u8, Turns> = MoveTable::new(turns.clone(), rep_table.clone());

        // Applying move_table moves is identical to applying permutations
        for ri in rep_table.rep_indexes() {
            let p: TwoTriangles = rep_table.rep_index_to_perm(ri);
            for t in &turns {
                let by_perm = p.permute((*t).into());

                let ti = move_table.turn_to_turn_index(*t);
                let (ri, si) = move_table.turn(ri, ti);
                let before_sym = rep_table.rep_index_to_perm(ri);
                let by_table = match rep_table.sym_index_to_sym(si) {
                    None => before_sym,
                    Some(sym) => before_sym.get_equivalent(&sym),
                };
                assert_eq!(by_perm, by_table);
            }
        }

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        for p in all_perms {
            for t in &turns {
                let ti = move_table.turn_to_turn_index(*t);
                let (ri_a, _) = move_table.perm_to_indexes(&p);
                let (ri_b, _) = move_table.turn(ri_a, ti);
                let mut found = false;
                for t in &turns {
                    let ti_inv = move_table.turn_to_turn_index(*t);
                    let (ri_rt, _ ) = move_table.turn(ri_b, ti_inv);
                    found |= ri_a == ri_rt;
                }
                assert_eq!(found, true);
            }
        }
    }

    #[test]
    fn move_table_is_correct_for_two_triangles_even_parity_with_rotational_symmetry() {
        // Even though just Left + Right is sufficient and symmetric,
        // MoveTables should basically always include turn inverses, so that
        // they can go forward or backwards.
        let turns = vec![Turns::Left, Turns::LeftPrime, Turns::Right, Turns::RightPrime];
        let syms = vec![Sym::MirrorBoth];
        let all_perms = (0..120u8).map(|i| i.into()).filter(|t: &TwoTriangles| t.is_even_parity());

        let rep_table: RepresentativeTable<TwoTriangles, Sym, u8> = RepresentativeTable::new(syms, all_perms.clone());
        let rep_table = Arc::new(rep_table);
        let move_table: MoveTable<TwoTriangles, Sym, u8, Turns> = MoveTable::new(turns.clone(), rep_table.clone());

        // Applying move_table moves is identical to applying permutations
        for ri in rep_table.rep_indexes() {
            let p: TwoTriangles = rep_table.rep_index_to_perm(ri);
            for t in &turns {
                let by_perm = p.permute((*t).into());

                let ti = move_table.turn_to_turn_index(*t);
                let (ri, si) = move_table.turn(ri, ti);
                let before_sym = rep_table.rep_index_to_perm(ri);
                let by_table = match rep_table.sym_index_to_sym(si) {
                    None => before_sym,
                    Some(sym) => before_sym.get_equivalent(&sym),
                };
                assert_eq!(by_perm, by_table);
            }
        }

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        for p in all_perms {
            for t in &turns {
                let ti = move_table.turn_to_turn_index(*t);
                let (ri_a, _) = move_table.perm_to_indexes(&p);
                let (ri_b, _) = move_table.turn(ri_a, ti);
                let mut found = false;
                for t in &turns {
                    let ti_inv = move_table.turn_to_turn_index(*t);
                    let (ri_rt, _ ) = move_table.turn(ri_b, ti_inv);
                    found |= ri_a == ri_rt;
                }
                assert_eq!(found, true);
            }
        }
    }

    #[test]
    fn move_table_is_correct_for_two_triangles_even_parity_with_full_symmetry() {
        let turns = vec![Turns::Left, Turns::LeftPrime, Turns::Right, Turns::RightPrime];
        let syms = vec![Sym::MirrorLR, Sym::MirrorTD, Sym::MirrorBoth];
        let all_perms = (0..120u8).map(|i| i.into()).filter(|t: &TwoTriangles| t.is_even_parity());

        let rep_table: RepresentativeTable<TwoTriangles, Sym, u8> = RepresentativeTable::new(syms, all_perms.clone());
        let rep_table = Arc::new(rep_table);
        let move_table: MoveTable<TwoTriangles, Sym, u8, Turns> = MoveTable::new(turns.clone(), rep_table.clone());

        // Applying move_table moves is identical to applying permutations
        for ri in rep_table.rep_indexes() {
            let p: TwoTriangles = rep_table.rep_index_to_perm(ri);
            for t in &turns {
                let by_perm = p.permute((*t).into());

                let ti = move_table.turn_to_turn_index(*t);
                let (ri, si) = move_table.turn(ri, ti);
                let before_sym = rep_table.rep_index_to_perm(ri);
                let by_table = match rep_table.sym_index_to_sym(si) {
                    None => before_sym,
                    Some(sym) => before_sym.get_equivalent(&sym),
                };
                assert_eq!(by_perm, by_table);
            }
        }

        // All entries are bi-directional (this holds because all turns in the
        // turn set also have an inverse in the turn set).  If there's a move
        // that can put you in state b from a, then there must exist an inverse
        // turn that puts you from state a to state b.
        for p in all_perms {
            for t in &turns {
                let ti = move_table.turn_to_turn_index(*t);
                let (ri_a, _) = move_table.perm_to_indexes(&p);
                let (ri_b, _) = move_table.turn(ri_a, ti);
                let mut found = false;
                for t in &turns {
                    let ti_inv = move_table.turn_to_turn_index(*t);
                    let (ri_rt, _ ) = move_table.turn(ri_b, ti_inv);
                    found |= ri_a == ri_rt;
                }
                assert_eq!(found, true);
            }
        }
    }
}
