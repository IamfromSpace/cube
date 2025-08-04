use permutation_group::PermutationGroup as PG;
use invertable::Invertable;
use equivalence_class::EquivalenceClass;

use std::collections::BTreeMap;
use std::collections::VecDeque;

// We have a simple little puzzle with two "faces" that share a middle.
// we can rotate either clockwise or counter-clockwise.  Even though
// just clockwise turns generate all reachable states, we get more
// symmetries if we include clockwise turns (because the turns must also
// follow the symmetries).
//
// 0   3
//   2
// 1   4
//
// While there are 120 hypothetical permutations, only 60 of them are
// reachable by moves, because we always perform an even number of
// swaps--odd permutations are not representable.
//
// We consider symmetry through mirroring, left-right and top-down, which
// in combination is the same as a 180 degree rotation.
//
// With 60 reachable states and 4 symmetries (counting the identity), we
// expect that there at least unique 15 representants.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct TwoTriangles([u8; 5]);

impl TwoTriangles {
    // TODO: this can be generalized, also, isn't this a property of being a permutation group?
    pub fn is_even_parity(&self) -> bool {
        let mut visited_in_look_ahead = [false; 5];
        let mut even_cycle_count = 0;
        for i in 0..5 {
            if visited_in_look_ahead[i] == false {
                let mut j = self.0[i];
                if j != i as u8 {
                    let mut piece_count = 2; // i & j
                    loop {
                        visited_in_look_ahead[j as usize] = true;
                        j = self.0[j as usize];
                        if j == i as u8 {
                            if piece_count % 2 == 0 {
                                even_cycle_count += 1;
                            }
                            break;
                        } else {
                            piece_count += 1;
                        }
                    }
                }
            }
        }
        even_cycle_count % 2 == 0
    }
}

const fn permute_arr(a: &[u8; 5], b: &[u8; 5]) -> [u8; 5] {
    [
        a[b[0] as usize],
        a[b[1] as usize],
        a[b[2] as usize],
        a[b[3] as usize],
        a[b[4] as usize],
    ]
}

const fn arr_inv(a: &[u8; 5]) -> [u8; 5] {
    let mut r = [0; 5];
    r[a[0] as usize] = 0;
    r[a[1] as usize] = 1;
    r[a[2] as usize] = 2;
    r[a[3] as usize] = 3;
    r[a[4] as usize] = 4;
    r
}

impl functional::BinaryOperation<TwoTriangles> for TwoTriangles {
    fn apply(a: TwoTriangles, b: TwoTriangles) -> TwoTriangles {
        TwoTriangles(permute_arr(&a.0, &b.0))
    }
}

impl functional::AssociativeOperation<TwoTriangles> for TwoTriangles { }

impl functional::Monoid<TwoTriangles> for TwoTriangles {
    fn one() -> TwoTriangles {
        TwoTriangles([0, 1, 2, 3, 4])
    }
}

impl Invertable for TwoTriangles {
    fn invert(&self) -> TwoTriangles {
        TwoTriangles(arr_inv(&self.0))
    }
}

impl PG for TwoTriangles {}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Turns {
    Left,
    LeftPrime,
    Right,
    RightPrime,
}

impl Into<TwoTriangles> for Turns {
    fn into(self) -> TwoTriangles {
        match self {
            // 2 -> 0 -> 1
            Turns::Left => TwoTriangles([1, 2, 0, 3, 4]),
            // 2 <- 0 <- 1
            Turns::LeftPrime => TwoTriangles([2, 0, 1, 3, 4]),
            // 3 -> 2 -> 4
            Turns::Right => TwoTriangles([0, 1, 4, 2, 3]),
            // 3 <- 2 <- 4
            Turns::RightPrime => TwoTriangles([0, 1, 3, 4, 2]),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Sym {
    MirrorLR,
    MirrorTD,
    MirrorBoth,
}

impl Into<TwoTriangles> for Sym {
    fn into(self) -> TwoTriangles {
        match self {
            // 0 -> 3
            // 1 -> 4
            Sym::MirrorLR => TwoTriangles([3, 4, 2, 0, 1]),
            // 0 -> 1
            // 3 -> 4
            Sym::MirrorTD => TwoTriangles([1, 0, 2, 4, 3]),
            // 0 -> 4
            // 1 -> 3
            Sym::MirrorBoth => TwoTriangles([4, 3, 2, 1, 0]),
        }
    }
}

impl EquivalenceClass<Sym> for TwoTriangles {
    fn get_equivalent(self, sym: &Sym) -> TwoTriangles {
        let x: TwoTriangles = sym.clone().into();
        x.invert().permute(self).permute(x)
    }
}

// TODO: There are faster algorithms than this
impl Into<u8> for TwoTriangles {
    fn into(self) -> u8 {
        let mut x = self.0.clone();
        for i in 0..5 {
            for j in (i+1)..5 {
                if x[j] > x[i] {
                    x[j] -= 1;
                }
            }
        }
        x[0] + 5 * (x[1] + 4 * (x[2] + 3 * (x[3] + 2 * x[4])))
    }
}

// TODO: There are faster algorithms than this
impl From<u8> for TwoTriangles {
    fn from(i: u8) -> Self {
        let mut i = i;
        let mut x = [0; 5];
        for j in 0..5 {
            x[j as usize] = i % (5 - j);
            i = i / (5 - j);
        }
        for i in (0..5).rev() {
            for j in (i+1)..5 {
                if x[j as usize] >= x[i] {
                    x[j as usize] += 1;
                }
            }
        }
        TwoTriangles(x)
    }
}

impl From<usize> for TwoTriangles {
    fn from(x: usize) -> Self {
        (x as u8).into()
    }
}

pub fn moves_to_solve(turns: &Vec<Turns>) -> BTreeMap<TwoTriangles, usize> {
    let mut queue = VecDeque::new();
    let mut map = BTreeMap::new();

    map.insert(TwoTriangles::identity(), 0);
    queue.push_back((TwoTriangles::identity(), 1));

    loop {
        match queue.pop_front() {
            None => break,
            Some((p, count)) => {
                for t in turns {
                    let turned = p.clone().permute((*t).into());
                    match map.get(&turned) {
                        None => {
                            map.insert(turned, count);
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

    #[test]
    fn all_lehmer_codes_round_trip() {
        for i in 0..120u8 {
            let t: TwoTriangles = i.into();
            assert_eq!(i, t.into());
        }
    }

    #[test]
    fn half_of_parities_are_even() {
        let mut even_count = 0;
        for i in 0..120u8 {
            let t: TwoTriangles = i.into();
            if t.is_even_parity() {
                even_count += 1;
            }
        }
        assert_eq!(even_count, 60);
    }

    #[test]
    fn one_swap_is_odd() {
        let t = TwoTriangles([1, 0, 2, 3, 4]);
        assert_eq!(t.is_even_parity(), false);
    }

    #[test]
    fn two_independent_swaps_are_even() {
        let t = TwoTriangles([1, 0, 3, 2, 4]);
        assert_eq!(t.is_even_parity(), true);
    }

    #[test]
    fn two_cycle_and_three_cycle_is_odd() {
        let t = TwoTriangles([1, 0, 4, 2, 3]);
        assert_eq!(t.is_even_parity(), false);
    }

    #[test]
    fn four_cycle_is_odd() {
        let t = TwoTriangles([0, 4, 1, 2, 3]);
        assert_eq!(t.is_even_parity(), false);
    }

    #[test]
    fn five_cycle_is_even() {
        let t = TwoTriangles([4, 0, 1, 2, 3]);
        assert_eq!(t.is_even_parity(), true);
    }
}
