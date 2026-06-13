// TODO: swap count is not guaranteed to be _optimal_, which means that even
// though the maximum optimal swap count cannot exceed usize, the swap count
// that we find may not be optimal and exceed it.  This isn't likely enough in
// practice to worry about--we're not sorting huge slices.
// TODO: We could get optimal if we re-index the sort.  The idea is, sort the
// array, then create a new array based on the original where we find the index
// of the value in the sorted array, and use that as this index.  That gives us
// the permutation the sort used, and we could then count the swaps on that
// permutation.  So [5, 3, 0] sorted is [0, 3, 5], and the sort's permutation
// is [2, 0, 1], which has two swaps.  That eliminates the need entirely for a
// hand-rolled sorting function, but does allocate a permutation.
pub fn sort_and_count_swaps<T: Ord>(xs_arg: &mut [T]) -> usize {
    let mut xs = xs_arg;
    let mut pivot = xs.len().saturating_sub(1);
    let mut i = 0;
    let mut swaps = 0;

    loop {
        if xs.len() <= 1 {
            break;
        }

        if i == pivot {
            let (left, pivot_and_right) = xs.split_at_mut(pivot);
            let (_, right) = pivot_and_right.split_at_mut(1);
            let (recurse_on, loop_on) =
                if left.len() < right.len() {
                    (left, right)
                } else {
                    (right, left)
                };

            i = 0;
            xs = loop_on;
            pivot = xs.len().saturating_sub(1);

            swaps += sort_and_count_swaps(recurse_on);
        } else {
            debug_assert!(xs[i] != xs[pivot], "Cannot count swaps on a slice with duplicate entries, because swap count becomes ambiguous.");

            if xs[i] >= xs[pivot] {
                if pivot - i > 1 {
                    xs.swap(i, pivot - 1);
                    xs.swap(pivot - 1, pivot);
                    swaps += 2;
                } else {
                    xs.swap(i, pivot);
                    swaps += 1;
                }
                pivot -= 1;
            } else {
                i += 1;
            }
        }
    }
    swaps
}

// usize is a tight upper bound, a permutation can require at most len - 1
// swaps, and a slice can have at most usize elements.
pub fn count_swaps<T: Into<usize> + Copy>(xs: &[T]) -> usize {
    let mut visited_in_look_ahead = vec![false; xs.len()];
    let mut swaps = 0;
    for i in 0..xs.len() {
        if visited_in_look_ahead[i] == false {
            let mut j = xs[i];
            debug_assert!(j.into() < xs.len(), "Can only count swaps on arrays that are valid permutations, uniquely using all values from 0..len()");

            if j.into() != i {
                let mut piece_count = 2; // i & j
                loop {
                    visited_in_look_ahead[j.into()] = true;
                    j = xs[j.into()];
                    if j.into() == i {
                        if piece_count % 2 == 0 {
                            swaps += 1;
                        }
                        break;
                    } else {
                        piece_count += 1;
                    }
                }
            }
        }
    }
    swaps
}

#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck::Gen;
    use rand::Rng;

    #[derive(Debug, Clone)]
    pub struct TestPerm(Vec<usize>);
    impl quickcheck::Arbitrary for TestPerm {
        fn arbitrary<G: Gen>(g: &mut G) -> TestPerm {
            let len = std::cmp::max(usize::arbitrary(g), 1000);
            let mut vec = Vec::with_capacity(len);
            for i in 0..len {
                vec.push(i);
            }
            g.shuffle(&mut vec);
            TestPerm(vec)
        }
    }

    #[derive(Debug, Clone)]
    pub struct TestSortedPattern(Vec<u8>);
    impl quickcheck::Arbitrary for TestSortedPattern {
        fn arbitrary<G: Gen>(g: &mut G) -> TestSortedPattern {
            let len = std::cmp::max(usize::arbitrary(g), 256);
            let mut vec = Vec::with_capacity(len);
            let mut last = 0u8;
            for _ in 0..len {
                last = last.saturating_add(u8::arbitrary(g)).saturating_add(1);
                if last == 255 {
                    break;
                }
                vec.push(last);
            }
            TestSortedPattern(vec)
        }
    }

    quickcheck! {
        fn sort_and_count_swaps_is_sorted(p: TestSortedPattern) -> bool {
            let mut p = p; // Quickcheck doesn't like mut in type signatures
            let _ = sort_and_count_swaps(&mut p.0);
            let mut b = true;
            for i in 1..p.0.len() {
                b &= p.0[i-1] <= p.0[i]
            }
            b
        }
    }

    quickcheck! {
        fn sort_and_count_swaps_matches_parity_calculation(p: TestPerm) -> bool {
            let mut p = p; // Quickcheck doesn't like mut in type signatures
            let pre_sort_swaps = count_swaps(&p.0);
            let post_sort_swaps = sort_and_count_swaps(&mut p.0);
            // NOTE: Swaps used to sort may not be optimal, so we can only validate a parity match
            pre_sort_swaps % 2 == post_sort_swaps % 2
        }
    }
}
