use equivalence_class::EquivalenceClass;
use table_traits::{ TableSearchToken, TableSearch };

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
// Opaque type to prevent accidental misuse
pub struct TranslatedLowerBoundToken<SearchToken>(SearchToken);

impl<SearchToken> TranslatedLowerBoundToken<SearchToken> {
    pub fn new(a: SearchToken) -> Self {
        Self(a)
    }
}

impl<SearchToken: TableSearchToken> TableSearchToken for TranslatedLowerBoundToken<SearchToken> {
    type Index = SearchToken::Index;

    fn table_get_index(&self) -> Self::Index {
        self.0.table_get_index()
    }

    fn table_get_lower_bound(&self) -> u8 {
        self.0.table_get_lower_bound()
    }
}

#[derive(Debug)]
pub struct TranslatedPruningTable<Sym, PruningTable, Turn> {
    pruning_table: PruningTable,
    translation: Sym,
    turn: std::marker::PhantomData<Turn>,
}

impl<Sym, PruningTable, Turn> TranslatedPruningTable<Sym, PruningTable, Turn> {
    pub fn new(pruning_table: PruningTable, translation: Sym) -> Self {
        TranslatedPruningTable { pruning_table, translation, turn: std::marker::PhantomData }
    }
}

impl<Sym, PruningTable, Turn> TableSearch<Turn> for TranslatedPruningTable<Sym, PruningTable, Turn> where
    Turn: EquivalenceClass<Sym>,
    PruningTable: TableSearch<Turn>,
    PruningTable::SearchToken: TableSearchToken,
{
    type Index = PruningTable::Index;
    type SearchToken = TranslatedLowerBoundToken<PruningTable::SearchToken>;

    fn table_start_search(&self, i: Self::Index) -> TranslatedLowerBoundToken<PruningTable::SearchToken> {
        TranslatedLowerBoundToken::new(self.pruning_table.table_start_search(i))
    }

    fn table_continue_search(&self, lbt: TranslatedLowerBoundToken<PruningTable::SearchToken>, t: Turn) -> TranslatedLowerBoundToken<PruningTable::SearchToken> {
        TranslatedLowerBoundToken::new(self.pruning_table.table_continue_search(lbt.0, t.get_equivalent(&self.translation)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::sync::Arc;
    use enum_iterator::all;
    use permutation_group::PermutationGroup;
    use representative_table::RepresentativeTable;
    use flat_move_table::MoveTable;
    use flat_pruning_table::PruningTable;

    use three_trapezoids::*;

    #[test]
    fn pruning_table_is_correct_for_three_trapezoids_outer_with_mirror_ud_symmetry_offset_by_rotational_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new::<ThreeTrapezoids>());
        let move_table = Arc::new(MoveTable::new_on_pattern::<ThreeTrapezoids, ThreeTrapezoids>(rep_table));
        let inner_pruning_table: Arc<PruningTable<MirrorUDSymmetry, ThreeTrapezoidsIndex, _, Turns, _>> = Arc::new(PruningTable::new(move_table.clone(), std::iter::once(ThreeTrapezoids::from(ThreeTrapezoids::identity()).into())));

        // Our simple implementation (ThreeTrapezoids is small enough to solve naively) matches our more complex one
        let tt_table = moves_to_solve();

        for translation in all::<RotationalSymmetry>() {
            let pruning_table = TranslatedPruningTable::new(inner_pruning_table.clone(), translation);
            for pi in all::<ThreeTrapezoidsIndex>() {
                let lbt_id = inner_pruning_table.table_start_search(pi);
                let lbt = pruning_table.table_start_search(pi);
                let initial_count = lbt.table_get_lower_bound();
                assert_eq!(lbt_id.table_get_lower_bound(), initial_count);
                for t in all::<Turns>() {
                    let lbt = pruning_table.table_continue_search(lbt, t);
                    let turned_count = lbt.table_get_lower_bound();
                    assert_eq!(turned_count == initial_count || turned_count == initial_count + 1 || turned_count + 1 == initial_count, true);
                }
            }
        }
    }

    #[test]
    fn pruning_table_is_correct_for_three_trapezoids_outer_with_rotational_symmetry_offset_by_mirror_ud_symmetry() {
        let rep_table = Arc::new(RepresentativeTable::new::<ThreeTrapezoids>());
        let move_table = Arc::new(MoveTable::new_on_pattern::<ThreeTrapezoids, ThreeTrapezoids>(rep_table));
        let inner_pruning_table: Arc<PruningTable<RotationalSymmetry, ThreeTrapezoidsIndex, _, Turns, _>> = Arc::new(PruningTable::new(move_table.clone(), std::iter::once(ThreeTrapezoids::from(ThreeTrapezoids::identity()).into())));

        // Our simple implementation (ThreeTrapezoids is small enough to solve naively) matches our more complex one
        let tt_table = moves_to_solve();

        for translation in all::<MirrorUDSymmetry>() {
            let pruning_table = TranslatedPruningTable::new(inner_pruning_table.clone(), translation);
            for pi in all::<ThreeTrapezoidsIndex>() {
                let lbt_id = inner_pruning_table.table_start_search(pi);
                let lbt = pruning_table.table_start_search(pi);
                let initial_count = lbt.table_get_lower_bound();
                assert_eq!(lbt_id.table_get_lower_bound(), initial_count);
                for t in all::<Turns>() {
                    let lbt = pruning_table.table_continue_search(lbt, t);
                    let turned_count = lbt.table_get_lower_bound();
                    assert_eq!(turned_count == initial_count || turned_count == initial_count + 1 || turned_count + 1 == initial_count, true);
                }
            }
        }
    }
}
