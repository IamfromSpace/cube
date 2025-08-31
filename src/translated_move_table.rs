use equivalence_class::EquivalenceClass;
use table_traits::{ TableTurn, TableSymTurn, TableRawIndexToSymIndex, TableSymIndexToRawIndex, TableRepCount };

use enum_iterator::{ Sequence, all };

#[derive(Debug)]
pub struct TranslatedMoveTable<TSym, MoveTable> {
    move_table: MoveTable,
    translation: TSym,
}

impl<TSym, MoveTable> TranslatedMoveTable<TSym, MoveTable> {
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
