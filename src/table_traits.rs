use std::sync::Arc;

pub trait TableTurn<Sym, RepIndex, Turn> {
    fn table_turn(self: &Self, i: RepIndex, t: Turn) -> (RepIndex, Sym);
}

impl<Sym, RepIndex, Turn, T: TableTurn<Sym, RepIndex, Turn>> TableTurn<Sym, RepIndex, Turn> for Arc<T> {
    fn table_turn(self: &Self, i: RepIndex, t: Turn) -> (RepIndex, Sym) {
        (**self).table_turn(i, t)
    }
}

pub trait TableSymTurn<Sym, RepIndex, Turn> {
    fn table_sym_turn(self: &Self, i: (RepIndex, Sym), t: Turn) -> (RepIndex, Sym);
}

impl<Sym, RepIndex, Turn, T: TableSymTurn<Sym, RepIndex, Turn>> TableSymTurn<Sym, RepIndex, Turn> for Arc<T> {
    fn table_sym_turn(self: &Self, i: (RepIndex, Sym), t: Turn) -> (RepIndex, Sym) {
        (**self).table_sym_turn(i, t)
    }
}

pub trait TableRawIndexToSymIndex<Sym, PermIndex, RepIndex> {
    fn table_raw_index_to_sym_index(self: &Self, pi: PermIndex) -> (RepIndex, Sym);
}

impl<Sym, PermIndex, RepIndex, T: TableRawIndexToSymIndex<Sym, PermIndex, RepIndex>> TableRawIndexToSymIndex<Sym, PermIndex, RepIndex> for Arc<T> {
    fn table_raw_index_to_sym_index(self: &Self, pi: PermIndex) -> (RepIndex, Sym) {
        (**self).table_raw_index_to_sym_index(pi)
    }
}

pub trait TableSymIndexToRawIndex<Sym, PermIndex, RepIndex> {
    fn table_sym_index_to_raw_index(self: &Self, si: (RepIndex, Sym)) -> PermIndex;
}

impl<Sym, PermIndex, RepIndex, T: TableSymIndexToRawIndex<Sym, PermIndex, RepIndex>> TableSymIndexToRawIndex<Sym, PermIndex, RepIndex> for Arc<T> {
    fn table_sym_index_to_raw_index(self: &Self, si: (RepIndex, Sym)) -> PermIndex {
        (**self).table_sym_index_to_raw_index(si)
    }
}

pub trait TableRepCount {
    fn table_rep_count(self: &Self) -> usize;
}

impl<T: TableRepCount> TableRepCount for Arc<T> {
    fn table_rep_count(self: &Self) -> usize {
        (**self).table_rep_count()
    }
}

pub trait TableSearchToken<Index> {
    fn table_get_index(&self) -> Index;

    // TODO: u8 seems _probably_ big enough....
    fn table_get_lower_bound(&self) -> u8;
}

pub trait TableSearch<Index, Turn> {
    type SearchToken;

    fn table_start_search(&self, i: Index) -> Self::SearchToken;

    fn table_continue_search(&self, st: Self::SearchToken, t: Turn) -> Self::SearchToken;
}
