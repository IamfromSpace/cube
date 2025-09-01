use equivalence_class::EquivalenceClass;
use table_traits::{ TableTurn, TableSymTurn, TableRawIndexToSymIndex, TableSymIndexToRawIndex, TableRepCount };

use enum_iterator::{ Sequence, all };

#[derive(Debug)]
pub struct TranslatedMoveTable<TSym, MoveTable> {
    move_table: MoveTable,
    translation: TSym,
}

impl<TSym, MoveTable> TranslatedMoveTable<TSym, MoveTable> {
    #[deprecated(note = "This struct is dangerous, because it doesn't compose, and that's its primary purpose: to combine two partial patterns to make a large whole.  See comment by warning for more details.")]
    // NOTE: This struct is dangerous; it doesn't compose.
    //
    // If you only need one pattern, you should probably just build the
    // translation natively into the move table, that will be faster anyway.
    // It doesn't compose because symmetries are applied in the wrong order,
    // and they aren't guaranteed to commute.  This only works if the
    // translation symmetry is the _first_ symmetry applied, but when its the
    // second entry in a CompositeMovetable, the CompositeIndex's symmetry
    // applies first.  As of yet, I can't think of anyway to fix this or
    // prevent accidental misuse of this struct (by introducing some sort of
    // constraint on commuting symmetries), so it's very risky to use it.
    //
    // As an example, consider ThreeTrapezoidsEdge, considering
    // MirrorUDSymmetry, applying a Left clockwise turn to the identity
    // position.  However, we want the composite of both the standard right
    // edge, and the lower right edge, so the second index of the composite is
    // a TranslatedMoveTable with a RotateCounterClock translation.
    //
    // In this case, a valid sym-index for the composite's initial state has
    // mirrored symmetry.  The identity is self-symmetric, so there's nothing
    // wrong with this choice (and forcing it to use identity just causes
    // problems elsewhere).  That means that instead of applying a Left
    // clockwise turn, we'll be applying a Left counter-clockwise turn to the
    // composite.  Then, we'll apply that to each component, and it will become
    // a lower right counter-clockwise turn on the tranlated move table.
    //
    // But this results in the incorrect turn.  We needed to apply these
    // symmetries in reverse.  If we first apply the translated table's
    // symmetry we go from Left -> LowerRight, and then when we apply the UD
    // mirror we get LowerRight -> UpperRightPrime.
    //
    // Sometimes we'll get the right answer by pure luck of the symmetries
    // chosen for sym-indexs, or if the symmetries naturally commute, but
    // generally this can result in applying an incorrect turn.
    pub fn new(move_table: MoveTable, translation: TSym) -> Self {
        TranslatedMoveTable { move_table, translation }
    }
}

impl<TSym, MoveTable, Sym, RepIndex, Turn> TableTurn<Sym, RepIndex, Turn> for TranslatedMoveTable<TSym, MoveTable> where
    MoveTable: TableTurn<Sym, RepIndex, Turn>,
    Turn: EquivalenceClass<TSym> {
        fn table_turn(&self, ri: RepIndex, t: Turn) -> (RepIndex, Sym) {
            self.move_table.table_turn(ri, t.get_equivalent(&self.translation))
        }
}

impl<TSym, MoveTable, Sym, RepIndex, Turn> TableSymTurn<Sym, RepIndex, Turn> for TranslatedMoveTable<TSym, MoveTable> where
    MoveTable: TableSymTurn<Sym, RepIndex, Turn>,
    Turn: EquivalenceClass<TSym> {
        fn table_sym_turn(&self, si: (RepIndex, Sym), t: Turn) -> (RepIndex, Sym) {
            self.move_table.table_sym_turn(si, t.get_equivalent(&self.translation))
        }
}

impl<TSym, MoveTable, Sym, PermIndex, RepIndex> TableRawIndexToSymIndex<Sym, PermIndex, RepIndex> for TranslatedMoveTable<TSym, MoveTable> where
    MoveTable: TableRawIndexToSymIndex<Sym, PermIndex, RepIndex> {
        fn table_raw_index_to_sym_index(&self, pi: PermIndex) -> (RepIndex, Sym) {
            self.move_table.table_raw_index_to_sym_index(pi)
        }
}

impl<TSym, MoveTable, Sym, PermIndex, RepIndex> TableSymIndexToRawIndex<Sym, PermIndex, RepIndex> for TranslatedMoveTable<TSym, MoveTable> where
    MoveTable: TableSymIndexToRawIndex<Sym, PermIndex, RepIndex> {
        fn table_sym_index_to_raw_index(&self, si: (RepIndex, Sym)) -> PermIndex {
            self.move_table.table_sym_index_to_raw_index(si)
        }
}

impl<TSym, MoveTable> TableRepCount for TranslatedMoveTable<TSym, MoveTable> where
    MoveTable: TableRepCount {
        fn table_rep_count(&self) -> usize {
            self.move_table.table_rep_count()
        }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::sync::Arc;
    use std::convert::TryFrom;

    use permutation_group::PermutationGroup as PG;
    use invertable::Invertable;
    use representative_table::RepresentativeTable;
    use flat_move_table::MoveTable;
    use three_triangles::*;

    fn test<TSym, Sym, Perm, PermIndex, Turn>() where
        TSym: Sequence + Copy + Invertable,
        Turn: Sequence + Copy + PartialEq + Into<usize> + EquivalenceClass<Sym> + EquivalenceClass<TSym> + Into<Perm>,
        Sym: PG + Sequence + Copy + Clone + Into<usize> + Invertable,
        PermIndex: Sequence + Copy + Ord + TryFrom<usize> + Into<usize> + Into<Perm> + std::fmt::Debug,
        Perm: PG + Clone + EquivalenceClass<Sym> + EquivalenceClass<TSym> + Into<PermIndex>,
        <PermIndex as TryFrom<usize>>::Error: std::fmt::Debug
    {
        let rep_table = Arc::new(RepresentativeTable::new::<Perm>());
        let inner_move_table = Arc::new(MoveTable::new::<Perm>(rep_table));

        // Double wrapping through inversion is identical
        for s in all::<TSym>() {
            let move_table: TranslatedMoveTable<TSym, TranslatedMoveTable<TSym, Arc<MoveTable<Sym, PermIndex, Turn>>>> = TranslatedMoveTable::new(TranslatedMoveTable::new(inner_move_table.clone(), s), s.invert());
            for pi in all::<PermIndex>() {
                for t in all::<Turn>() {
                    let via_inner = inner_move_table.table_sym_index_to_raw_index(inner_move_table.table_sym_turn(inner_move_table.table_raw_index_to_sym_index(pi), t));
                    let via_double = move_table.table_sym_index_to_raw_index(move_table.table_sym_turn(move_table.table_raw_index_to_sym_index(pi), t));
                    assert_eq!(via_inner, via_double);
                }
            }
        }

        // If we apply the same turns via a normal table to a complete perm and
        // an translated table on the translated permutation, then the result
        // is identical
        for s in all::<TSym>() {
            let move_table: TranslatedMoveTable<TSym, Arc<MoveTable<Sym, PermIndex, Turn>>> = TranslatedMoveTable::new(inner_move_table.clone(), s);
            for pi0 in all::<PermIndex>() {
                let p0: Perm = pi0.into();
                let spi0: PermIndex = p0.get_equivalent(&s).into();
                for t in all::<Turn>() {
                    let pi1 = inner_move_table.table_sym_index_to_raw_index(inner_move_table.table_sym_turn(inner_move_table.table_raw_index_to_sym_index(pi0), t));
                    let spi1 = move_table.table_sym_index_to_raw_index(move_table.table_sym_turn(move_table.table_raw_index_to_sym_index(spi0), t));
                    let sp1: Perm = spi1.into();
                    let spi2: PermIndex = sp1.get_equivalent(&s.invert()).into();
                    assert_eq!(pi1, spi2);
                }
            }
        }
    }

    #[test]
    fn three_triangles_works_when_the_translation_is_mirror_ud_symmetry_and_the_move_table_is_no_symmetry() {
        test::<MirrorUDSymmetry, NoSymmetry, ThreeTriangles, ThreeTrianglesEvenIndex, Turns>();
    }

    #[test]
    fn three_triangles_works_when_the_translation_is_mirror_ud_symmetry_and_the_move_table_is_mirror_ud_symmetry() {
        test::<MirrorUDSymmetry, MirrorUDSymmetry, ThreeTriangles, ThreeTrianglesEvenIndex, Turns>();
    }

    #[test]
    fn three_triangles_works_when_the_translation_is_mirror_ud_symmetry_and_the_move_table_is_rotational_symmetry() {
        test::<MirrorUDSymmetry, RotationalSymmetry, ThreeTriangles, ThreeTrianglesEvenIndex, Turns>();
    }

    #[test]
    fn three_triangles_works_when_the_translation_is_mirror_ud_symmetry_and_the_move_table_is_full_symmetry() {
        test::<MirrorUDSymmetry, FullSymmetry, ThreeTriangles, ThreeTrianglesEvenIndex, Turns>();
    }

    #[test]
    fn three_triangles_works_when_the_translation_is_rotational_symmetry_and_the_move_table_is_no_symmetry() {
        test::<RotationalSymmetry, NoSymmetry, ThreeTriangles, ThreeTrianglesEvenIndex, Turns>();
    }

    #[test]
    fn three_triangles_works_when_the_translation_is_rotational_symmetry_and_the_move_table_is_mirror_ud_symmetry() {
        test::<RotationalSymmetry, MirrorUDSymmetry, ThreeTriangles, ThreeTrianglesEvenIndex, Turns>();
    }

    #[test]
    fn three_triangles_works_when_the_translation_is_rotational_symmetry_and_the_move_table_is_rotational_symmetry() {
        test::<RotationalSymmetry, RotationalSymmetry, ThreeTriangles, ThreeTrianglesEvenIndex, Turns>();
    }

    #[test]
    fn three_triangles_works_when_the_translation_is_rotational_symmetry_and_the_move_table_is_full_symmetry() {
        test::<RotationalSymmetry, FullSymmetry, ThreeTriangles, ThreeTrianglesEvenIndex, Turns>();
    }
}
