use permutation_group::PermutationGroup as PG;
use invertable::Invertable;
use equivalence_class::EquivalenceClass;

use std::collections::BTreeMap;
use std::collections::VecDeque;
use std::convert::{TryInto, TryFrom};
use enum_iterator::{all, Sequence};

// We have a simple little puzzle with two "faces" that share a middle that can
// rotate.  A left rotation swaps the left two pieces, and a right rotation
// swaps the right two pieces.
//
// 0 1 2
//
// This is symmetric through mirroring.  With 6 possible states and 2
// symmetries (counting the identity), we expect that there at least unique 3
// representants.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct TwoLines([u8; 3]);

const fn permute_arr(a: &[u8; 3], b: &[u8; 3]) -> [u8; 3] {
    [
        a[b[0] as usize],
        a[b[1] as usize],
        a[b[2] as usize],
    ]
}

const fn arr_inv(a: &[u8; 3]) -> [u8; 3] {
    let mut r = [0; 3];
    r[a[0] as usize] = 0;
    r[a[1] as usize] = 1;
    r[a[2] as usize] = 2;
    r
}

impl functional::BinaryOperation<TwoLines> for TwoLines {
    fn apply(a: TwoLines, b: TwoLines) -> TwoLines {
        TwoLines(permute_arr(&a.0, &b.0))
    }
}

impl functional::AssociativeOperation<TwoLines> for TwoLines { }

impl functional::Monoid<TwoLines> for TwoLines {
    fn one() -> TwoLines {
        TwoLines([0, 1, 2])
    }
}

impl Invertable for TwoLines {
    fn invert(&self) -> TwoLines {
        TwoLines(arr_inv(&self.0))
    }
}

impl PG for TwoLines {}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash, Sequence)]
#[repr(u8)]
pub enum Turns {
    Left,
    Right,
}

impl Invertable for Turns {
    fn invert(&self) -> Turns {
        match self {
            Turns::Left => Turns::Left,
            Turns::Right => Turns::Right,
        }
    }
}

impl Into<usize> for Turns {
    fn into(self) -> usize {
        self as usize
    }
}

impl Into<TwoLines> for Turns {
    fn into(self) -> TwoLines {
        match self {
            Turns::Left => TwoLines([1, 0, 2]),
            Turns::Right => TwoLines([0, 2, 1]),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash, Sequence)]
#[repr(u8)]
pub enum FullSymmetry {
    Identity,
    Rotate180,
}

impl Into<usize> for FullSymmetry {
    fn into(self) -> usize {
        self as usize
    }
}

impl functional::BinaryOperation<FullSymmetry> for FullSymmetry {
    fn apply(a: FullSymmetry, b: FullSymmetry) -> FullSymmetry {
        match (a, b) {
            (FullSymmetry::Identity, FullSymmetry::Identity) => FullSymmetry::Identity,
            (FullSymmetry::Identity, FullSymmetry::Rotate180) => FullSymmetry::Rotate180,
            (FullSymmetry::Rotate180, FullSymmetry::Identity) => FullSymmetry::Rotate180,
            (FullSymmetry::Rotate180, FullSymmetry::Rotate180) => FullSymmetry::Identity,
        }
    }
}

impl functional::AssociativeOperation<FullSymmetry> for FullSymmetry { }

impl functional::Monoid<FullSymmetry> for FullSymmetry {
    fn one() -> FullSymmetry {
        FullSymmetry::Identity
    }
}

impl Invertable for FullSymmetry {
    fn invert(&self) -> FullSymmetry {
        match self {
            FullSymmetry::Identity => FullSymmetry::Identity,
            FullSymmetry::Rotate180 => FullSymmetry::Rotate180,
        }
    }
}

impl PG for FullSymmetry {}

impl Into<TwoLines> for FullSymmetry {
    fn into(self) -> TwoLines {
        match self {
            FullSymmetry::Identity => TwoLines([0, 1, 2]),
            FullSymmetry::Rotate180 => TwoLines([2, 1, 0]),
        }
    }
}

impl EquivalenceClass<FullSymmetry> for TwoLines {
    fn get_equivalent(self, sym: &FullSymmetry) -> TwoLines {
        let x: TwoLines = sym.clone().into();
        x.invert().permute(self).permute(x)
    }
}

impl EquivalenceClass<FullSymmetry> for Turns {
    fn get_equivalent(self, sym: &FullSymmetry) -> Turns {
        match (sym, self) {
            (FullSymmetry::Identity, Turns::Left) => Turns::Left,
            (FullSymmetry::Identity, Turns::Right) => Turns::Right,
            (FullSymmetry::Rotate180, Turns::Left) => Turns::Right,
            (FullSymmetry::Rotate180, Turns::Right) => Turns::Left,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash, Sequence)]
#[repr(u8)]
pub enum NoSymmetry {
    Identity,
}

impl Into<usize> for NoSymmetry {
    fn into(self) -> usize {
        self as usize
    }
}

impl functional::BinaryOperation<NoSymmetry> for NoSymmetry {
    fn apply(_: NoSymmetry, _: NoSymmetry) -> NoSymmetry {
        NoSymmetry::Identity
    }
}

impl functional::AssociativeOperation<NoSymmetry> for NoSymmetry { }

impl functional::Monoid<NoSymmetry> for NoSymmetry {
    fn one() -> NoSymmetry {
        NoSymmetry::Identity
    }
}

impl Invertable for NoSymmetry {
    fn invert(&self) -> NoSymmetry {
        NoSymmetry::Identity
    }
}

impl PG for NoSymmetry {}

impl Into<FullSymmetry> for NoSymmetry {
    fn into(self) -> FullSymmetry {
        match self {
            NoSymmetry::Identity => FullSymmetry::Identity,
        }
    }
}

impl Into<TwoLines> for NoSymmetry {
    fn into(self) -> TwoLines {
        let full: FullSymmetry = self.into();
        full.into()
    }
}

impl EquivalenceClass<NoSymmetry> for TwoLines {
    fn get_equivalent(self, sym: &NoSymmetry) -> TwoLines {
        let x: TwoLines = sym.clone().into();
        x.invert().permute(self).permute(x)
    }
}

impl EquivalenceClass<NoSymmetry> for Turns {
    fn get_equivalent(self, sym: &NoSymmetry) -> Turns {
        let full: FullSymmetry = (*sym).into();
        self.get_equivalent(&full)
    }
}

// TODO: There are faster algorithms than this
fn to_lehmer(p: TwoLines) -> u8 {
    let mut x = p.0.clone();
    for i in 0..3 {
        for j in (i+1)..3 {
            if x[j] > x[i] {
                x[j] -= 1;
            }
        }
    }
    x[0] + 3 * (x[1] + 2 * x[2])
}

// TODO: There are faster algorithms than this
fn from_lehmer(i: u8) -> TwoLines {
    let mut i = i;
    let mut x = [0; 3];
    for j in 0..3 {
        x[j as usize] = i % (3 - j);
        i = i / (3 - j);
    }
    for i in (0..3).rev() {
        for j in (i+1)..3 {
            if x[j as usize] >= x[i] {
                x[j as usize] += 1;
            }
        }
    }
    TwoLines(x)
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
pub struct TwoLinesIndex(u8);

impl Sequence for TwoLinesIndex {
    const CARDINALITY: usize = 6;

    fn next(&self) -> Option<Self> {
        if self.0 == (Self::CARDINALITY - 1) as u8 {
            None
        } else {
            Some(TwoLinesIndex(self.0 + 1))
        }
    }

    fn previous(&self) -> Option<Self> {
        if self.0 == 0 {
            None
        } else {
            Some(TwoLinesIndex(self.0 - 1))
        }
    }

    fn first() -> Option<Self> {
        Some(TwoLinesIndex(0))
    }

    fn last() -> Option<Self> {
        Some(TwoLinesIndex((Self::CARDINALITY - 1) as u8))
    }
}

impl Into<TwoLines> for TwoLinesIndex {
    fn into(self) -> TwoLines {
        from_lehmer(self.0)
    }
}

impl Into<usize> for TwoLinesIndex {
    fn into(self) -> usize {
        self.0 as usize
    }
}

impl TryFrom<usize> for TwoLinesIndex {
    type Error = std::num::TryFromIntError;

    fn try_from(i: usize) -> Result<Self, Self::Error> {
        let j = i.try_into()?;
        Ok(TwoLinesIndex(j))
    }
}

impl Into<TwoLinesIndex> for TwoLines {
    fn into(self) -> TwoLinesIndex {
        TwoLinesIndex(to_lehmer(self))
    }
}

pub fn moves_to_solve() -> BTreeMap<TwoLinesIndex, usize> {
    let mut queue = VecDeque::new();
    let mut map = BTreeMap::new();

    map.insert(TwoLines::identity().into(), 0);
    queue.push_back((TwoLines::identity(), 1));

    loop {
        match queue.pop_front() {
            None => break,
            Some((p, count)) => {
                for t in all::<Turns>() {
                    let turned = p.clone().permute(t.into());
                    match map.get(&turned.into()) {
                        None => {
                            map.insert(turned.into(), count);
                            queue.push_back((turned, count + 1))
                        },
                        Some(_) => (),
                    }
                }
            },
        }
    }
    map
}

#[cfg(test)]
mod tests {
    use super::*;
    use enum_iterator::all;

    #[test]
    fn all_lehmer_codes_round_trip() {
        for i in 0..6u8 {
            let t: TwoLines = from_lehmer(i);
            assert_eq!(i, to_lehmer(t));
        }
    }

    #[test]
    fn perm_and_turn_no_symmetries_are_equivalent() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s in all::<NoSymmetry>() {
            for pi in all::<TwoLinesIndex>() {
                let p: TwoLines = pi.into();
                for t in all::<Turns>() {
                    let after_permute = p.permute(t.into()).get_equivalent(&s);
                    let before_permute = p.get_equivalent(&s).permute(t.get_equivalent(&s).into());
                    assert_eq!(after_permute, before_permute)
                }
            }
        }
    }

    #[test]
    fn perm_and_turn_full_symmetries_are_equivalent() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s in all::<FullSymmetry>() {
            for pi in all::<TwoLinesIndex>() {
                let p: TwoLines = pi.into();
                for t in all::<Turns>() {
                    let after_permute = p.permute(t.into()).get_equivalent(&s);
                    let before_permute = p.get_equivalent(&s).permute(t.get_equivalent(&s).into());
                    assert_eq!(after_permute, before_permute)
                }
            }
        }
    }

    #[test]
    fn direct_and_indirect_sym_multiplication_are_equivalent_for_full_symmetry() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s0 in all::<FullSymmetry>() {
            for s1 in all::<FullSymmetry>() {
                for pi in all::<TwoLinesIndex>() {
                    let p: TwoLines = pi.into();
                    let as_tt = p.permute(s0.into()).permute(s1.into());
                    let as_sym = p.permute(s0.permute(s1).into());
                    assert_eq!(as_tt, as_sym);
                }
            }
        }
    }

    #[test]
    fn direct_and_indirect_sym_multiplication_are_equivalent_for_no_symmetry() {
        // Typically we need to use quickcheck here, but we can be exhaustive for a puzzle this size
        for s0 in all::<NoSymmetry>() {
            for s1 in all::<NoSymmetry>() {
                for pi in all::<TwoLinesIndex>() {
                    let p: TwoLines = pi.into();
                    let as_tt = p.permute(s0.into()).permute(s1.into());
                    let as_sym = p.permute(s0.permute(s1).into());
                    assert_eq!(as_tt, as_sym);
                }
            }
        }
    }
}
