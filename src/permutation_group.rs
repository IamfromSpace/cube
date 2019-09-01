extern crate functional;

pub trait PermutationGroup: functional::Monoid<Self> where
  Self: functional::AssociativeOperation<Self> {
    fn invert(&self) -> Self;

    fn permute(self: Self, other: Self) -> Self {
        Self::apply(self, other)
    }

    fn identity() -> Self {
        functional::Monoid::one()
    }
}

use super::equivalence_class::EquivalenceClass;
impl<T: PermutationGroup> EquivalenceClass<T> for T {
    fn get_equivalent(self, t: T) -> T {
        t.invert().permute(self).permute(t)
    }
}
