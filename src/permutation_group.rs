extern crate functional;

use super::invertable::Invertable;

pub trait PermutationGroup: functional::Monoid<Self> where
  Self: Invertable + functional::AssociativeOperation<Self> {
    fn permute(self: Self, other: Self) -> Self {
        Self::apply(self, other)
    }

    fn identity() -> Self {
        functional::Monoid::one()
    }
}

use super::equivalence_class::EquivalenceClass;
impl<T: PermutationGroup + Copy> EquivalenceClass<T> for T {
    fn get_equivalent(self, t: &T) -> T {
        t.invert().permute(self).permute(*t)
    }
}
