pub trait EquivalenceClass<T> where {
    fn get_equivalent(self, sym: &T) -> Self;
}
