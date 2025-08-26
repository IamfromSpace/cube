use permutation_group::PermutationGroup;

// Right Actions

pub trait MagmaAction<T> where
  T: functional::BinaryOperation<T> {
    fn act(self: Self, t: T) -> Self;
  }

// All binary operations are also magma actions
impl<T: functional::BinaryOperation<T>> MagmaAction<T> for T {
    fn act(self: Self, t: T) -> T {
        <Self as functional::BinaryOperation<T>>::apply(self, t)
    }
}

pub trait SemigroupAction<T> where
  T: functional::AssociativeOperation<T>, Self: MagmaAction<T> {}

// If we can perform a MagmaAction, and the Magma is also a Semigroup, then it must also be a SemigroupAction
impl<G: functional::AssociativeOperation<G>, X: MagmaAction<G>> SemigroupAction<G> for X {}

pub trait MonoidAction<T> where
  T: functional::Monoid<T> + functional::AssociativeOperation<T>, Self: SemigroupAction<T> {}

// MonoidAction must still be explicitly implemented (to confirm that the identity law holds), even if we can perform SemigroupAction and the Semigroup is also a Monoid.

// All monoid operations are also monoid actions
impl<T: functional::Monoid<T> + functional::AssociativeOperation<T>> MonoidAction<T> for T {}

pub trait GroupAction<T> where
  T: PermutationGroup, Self: MonoidAction<T> {}

// If we can perform a MonoidAction, and the Monoid is also a Group, then it must also be a GroupAction
impl<G: PermutationGroup, X: MonoidAction<G>> GroupAction<G> for X {}


// Left Actions

pub trait LeftMagmaAction<T> where
  T: functional::BinaryOperation<T> {
    fn act_left(a: T, b: Self) -> Self;
  }

// All binary operations are also magma actions
impl<T: functional::BinaryOperation<T>> LeftMagmaAction<T> for T {
    fn act_left(a: T, b: Self) -> T {
        <Self as functional::BinaryOperation<T>>::apply(a, b)
    }
}

pub trait LeftSemigroupAction<T> where
  T: functional::AssociativeOperation<T>, Self: LeftMagmaAction<T> {}

// If we can perform a LeftMagmaAction, and the Magma is also a Semigroup, then it must also be a LeftSemigroupAction
impl<G: functional::AssociativeOperation<G>, X: LeftMagmaAction<G>> LeftSemigroupAction<G> for X {}

pub trait LeftMonoidAction<T> where
  T: functional::Monoid<T> + functional::AssociativeOperation<T>, Self: LeftSemigroupAction<T> {}

// LeftMonoidAction must still be explicitly implemented (to confirm that the identity law holds), even if we can perform LeftSemigroupAction and the Semigroup is also a Monoid.

// All monoid operations are also monoid actions
impl<T: functional::Monoid<T> + functional::AssociativeOperation<T>> LeftMonoidAction<T> for T {}

pub trait LeftGroupAction<T> where
  T: PermutationGroup, Self: LeftMonoidAction<T> {}

// If we can perform a LeftMonoidAction, and the Monoid is also a Group, then it must also be a LeftGroupAction
impl<G: PermutationGroup, X: LeftMonoidAction<G>> LeftGroupAction<G> for X {}
