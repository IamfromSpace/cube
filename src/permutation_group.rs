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

    fn apply_symmetry(self, perm: Self) -> Self {
        perm.invert().permute(self).permute(perm)
    }
}