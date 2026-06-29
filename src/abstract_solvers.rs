pub mod ida_basic {
    use table_traits::{TableSearch, TableSearchToken, SolveOptimal};

    struct IdaBasic<PruningTable, Turn> {
        pruning_table: PruningTable,
        turns: std::marker::PhantomData<Turn>,
    }

    impl<Turn, PruningTable: TableSearch<Turn>> IdaBasic<PruningTable, Turn> {
        pub fn new(pruning_table: PruningTable) -> Self {
            Self {
                pruning_table,
                turns: std::marker::PhantomData,
            }
        }
    }

    impl<Turn: enum_iterator::Sequence + Copy, PruningTable: TableSearch<Turn>> SolveOptimal<Turn> for IdaBasic<PruningTable, Turn> where PruningTable::SearchToken: TableSearchToken + Copy {
        type Index = PruningTable::Index;

        fn solve_optimal(&self, pi: PruningTable::Index) -> Vec<Turn> {
            let first = self.pruning_table.table_start_search(pi);
            let mut max = first.table_get_lower_bound();
            let mut t_path = Vec::new();
            let mut lbt_path = vec![first];

            // TODO: Stupid variables that can probably be optimized better
            let mut first = true;
            let mut back = false;

            loop {
                let curr_lbt = lbt_path[lbt_path.len() - 1];
                let curr_lower_bound = curr_lbt.table_get_lower_bound();
                if curr_lower_bound == 0 {
                    break;
                }

                if t_path.len() == 0 {
                    let t = enum_iterator::first::<Turn>().expect("Invariant Violation: must have at least one turn");
                    let lbt = self.pruning_table.table_continue_search(curr_lbt, t);
                    t_path.push(t);
                    lbt_path.push(lbt);

                    // TODO: This is stupid
                    if first {
                        first = false;
                    } else {
                        max += 1;
                    }
                    back = false;
                } else if back || (curr_lower_bound + t_path.len() as u8) > max { // TODO: multiply curr_lower_bound to weight
                    let t_curr = t_path.pop().expect("Invariant Violation: Already checked that t_path wasn't empty!");
                    lbt_path.pop();
                    match enum_iterator::next::<Turn>(&t_curr) {
                        None => {
                            back = true;
                        },
                        Some(t) => {
                            back = false;
                            let lbt = self.pruning_table.table_continue_search(lbt_path[lbt_path.len() - 1], t);
                            t_path.push(t);
                            lbt_path.push(lbt);
                        }
                    }
                } else {
                    let t = enum_iterator::first::<Turn>().expect("Invariant Violation: must have at least one turn");
                    let lbt = self.pruning_table.table_continue_search(curr_lbt, t);
                    t_path.push(t);
                    lbt_path.push(lbt);
                    back = false;
                }
            }

            t_path
        }
    }

    #[cfg(test)]
    mod test {
        use super::*;

        use std::collections::BTreeMap;
        use quickcheck::Arbitrary;
        use quickcheck::Gen;
        use rand::prelude::IteratorRandom;

        use three_trapezoids::{ThreeTrapezoids, ThreeTrapezoidsIndex, Turns, moves_to_solve};
        use table_traits::{TableSearchToken, TableSearch};
        use permutation_group::PermutationGroup;

        #[derive(Debug, Clone, Copy)]
        struct MockTTLowerBoundToken(ThreeTrapezoidsIndex, u8);

        impl TableSearchToken for MockTTLowerBoundToken {
            type Index = ThreeTrapezoidsIndex;

            fn table_get_index(&self) -> Self::Index {
                self.0
            }

            fn table_get_lower_bound(&self) -> u8 {
                self.1
            }
        }

        #[derive(Debug, Clone)]
        struct MockAdmissableThreeTrapezoidsPruningTable(BTreeMap<ThreeTrapezoidsIndex, usize>);

        impl TableSearch<Turns> for MockAdmissableThreeTrapezoidsPruningTable {
            type Index = ThreeTrapezoidsIndex;
            type SearchToken = MockTTLowerBoundToken;

            fn table_start_search(&self, i: Self::Index) -> Self::SearchToken {
                MockTTLowerBoundToken(i, *self.0.get(&i).unwrap() as u8)
            }

            fn table_continue_search(&self, st: Self::SearchToken, t: Turns) -> Self::SearchToken {
                let i: ThreeTrapezoidsIndex = Into::<ThreeTrapezoids>::into(st.0).permute(Into::<ThreeTrapezoids>::into(t)).into();
                MockTTLowerBoundToken(i, *self.0.get(&i).unwrap() as u8)
            }
        }

        // We can convert a perfect pruning table into an arbitrary admissable
        // one by choosing any lower bound at or below the true optimal, while
        // still reserving zero for the goals.
        impl Arbitrary for MockAdmissableThreeTrapezoidsPruningTable {
            fn arbitrary<G: Gen>(g: &mut G) -> Self {
                let mut table = moves_to_solve();
                for (_, value) in table.iter_mut() {
                    if *value != 0 {
                        *value = (1..=*value).choose(g).unwrap();
                    }
                }
                MockAdmissableThreeTrapezoidsPruningTable(table)
            }
        }

        quickcheck! {
            // It _is_ tractable to exhaustively check all positions for a
            // given table, but it takes >30s in debug mode to run them all.
            // And there are an enormous number of possible admissable pruning
            // tables, so we can never be truly exhaustive anyway.
            fn solves_any_admissable_three_trapezoids_pruning_table(table: MockAdmissableThreeTrapezoidsPruningTable, i: ThreeTrapezoidsIndex) -> bool {
                let solver = IdaBasic::new(table);
                let exact_table = moves_to_solve();
                let solution = solver.solve_optimal(i);
                let mut solved: ThreeTrapezoids = i.into();
                // TODO: since Vec<Turns> forms a group, this loop could just be solved = i.into().act(solution);
                for t in solution.clone() {
                    solved = solved.permute(Into::<ThreeTrapezoids>::into(t));
                }
                solution.len() == *exact_table.get(&i).unwrap()
                    && solved == ThreeTrapezoids::identity()
            }
        }

        use enum_iterator::{Sequence, all, cardinality};
        use std::collections::{VecDeque, BTreeSet};

        #[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Sequence)]
        enum MockSolvableNodes {
            Zero,
            One,
            Two,
            Three,
            Four,
            Five,
            Six,
            Seven,
        }

        impl Arbitrary for MockSolvableNodes {
            fn arbitrary<G: Gen>(g: &mut G) -> Self {
                all::<Self>().choose(g).unwrap()
            }
        }

        #[derive(Debug, Clone, Copy)]
        struct MockSolvableLowerBoundToken(MockSolvableNodes, u8);

        impl TableSearchToken for MockSolvableLowerBoundToken {
            type Index = MockSolvableNodes;

            fn table_get_index(&self) -> Self::Index {
                self.0
            }

            fn table_get_lower_bound(&self) -> u8 {
                self.1
            }
        }

        #[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Sequence)]
        enum MockSolvableTurns {
            A,
            B,
            C,
            D,
        }

        impl Arbitrary for MockSolvableTurns {
            fn arbitrary<G: Gen>(g: &mut G) -> Self {
                all::<Self>().choose(g).unwrap()
            }
        }

        // A co-accessible automaton that is also deterministic and total. Aka,
        // a graph that is always solvable from any position.  Plus an
        // imperfect pruning table that ida will actually use.  Three key
        // considerations a) if turn A' from position X moves to position Y,
        // then no other position can reach Y via A', as Y cannot go two
        // different positions via A b) we cannot leave orphaned nodes, so if
        // we randomly leave orphans, we correct this by forcing a turn to
        // instead choose an orphan c) every node must support every move, so
        // as we walk backwards, we must eventually discover all turns on all
        // moves.
        #[derive(Debug, Clone)]
        struct MockSolvable {
            move_table: BTreeMap<(MockSolvableNodes, MockSolvableTurns), MockSolvableNodes>,
            perfect_pruning_table: BTreeMap<MockSolvableNodes, u8>,
            imperfect_pruning_table: BTreeMap<MockSolvableNodes, u8>,
            goal_states: BTreeSet<MockSolvableNodes>,
        }

        impl TableSearch<MockSolvableTurns> for MockSolvable {
            type Index = MockSolvableNodes;
            type SearchToken = MockSolvableLowerBoundToken;

            fn table_start_search(&self, i: Self::Index) -> Self::SearchToken {
                MockSolvableLowerBoundToken(i, *self.imperfect_pruning_table.get(&i).unwrap())
            }

            fn table_continue_search(&self, st: Self::SearchToken, t: MockSolvableTurns) -> Self::SearchToken {
                let i = self.move_table.get(&(st.0, t)).unwrap();
                let lb = self.imperfect_pruning_table.get(i).unwrap();
                MockSolvableLowerBoundToken(*i, *lb as u8)
            }
        }

        impl Arbitrary for MockSolvable {
            fn arbitrary<G: Gen>(g: &mut G) -> Self {
                let mut inverse_move_table = BTreeSet::new();
                let mut perfect_pruning_table = BTreeMap::new();
                let mut imperfect_pruning_table = BTreeMap::new();
                let mut goal_states = BTreeSet::new();

                let mut not_yet_discovered = BTreeSet::new();
                for n in all::<MockSolvableNodes>() {
                    not_yet_discovered.insert(n);
                }

                let mut allowed_connections = BTreeSet::new();
                for t in all::<MockSolvableTurns>() {
                    for n in all::<MockSolvableNodes>() {
                        allowed_connections.insert((n, t));
                    }
                }

                let mut queue = VecDeque::new();
                let goal_count = (1..cardinality::<MockSolvableNodes>()).choose(g).unwrap();
                for n in all::<MockSolvableNodes>().take(goal_count) {
                    perfect_pruning_table.insert(n, 0);
                    imperfect_pruning_table.insert(n, 0);
                    goal_states.insert(n);
                    queue.push_back((n, 1));
                    not_yet_discovered.remove(&n);
                }

                loop {
                    match queue.pop_front() {
                        None => break,
                        Some((node, depth)) => {
                            let c = (0..=allowed_connections.len()).choose(g).unwrap();
                            for _ in 0..c {
                                let (n, t) = *allowed_connections.iter().choose(g).unwrap();
                                allowed_connections.remove(&(n, t));
                                inverse_move_table.insert((node, t, n));

                                // Newly found node
                                if not_yet_discovered.contains(&n) {
                                    perfect_pruning_table.insert(n, depth);
                                    imperfect_pruning_table.insert(n, (1..=depth).choose(g).unwrap());
                                    queue.push_back((n, depth + 1));
                                    not_yet_discovered.remove(&n);
                                }
                            }

                            if queue.len() == 0 {
                                match not_yet_discovered.pop_first() {
                                    // If we would leave an orphaned node,
                                    // instead also choose it.
                                    Some(n) => {
                                        let t = all::<MockSolvableTurns>().choose(g).unwrap();
                                        allowed_connections.remove(&(n, t));
                                        inverse_move_table.insert((node, t, n));

                                        perfect_pruning_table.insert(n, depth);
                                        imperfect_pruning_table.insert(n, (1..=depth).choose(g).unwrap());
                                        queue.push_back((n, depth + 1));
                                    }
                                    // Consume all remaining connections if
                                    // this is the final node explored, to
                                    // ensure that all nodes support all moves
                                    None => {
                                        for (n, t) in &allowed_connections {
                                            inverse_move_table.insert((node, *t, *n));
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                let mut move_table = BTreeMap::new();
                for (from, t, to) in inverse_move_table.into_iter() {
                    move_table.insert((to, t), from);
                }

                MockSolvable {
                    move_table,
                    perfect_pruning_table,
                    imperfect_pruning_table,
                    goal_states,
                }
            }
        }

        quickcheck! {
            fn solves_any_mock_puzzle(mp: MockSolvable, i: MockSolvableNodes) -> bool {
                let solver = IdaBasic::new(mp.clone());
                let solution = solver.solve_optimal(i);
                let mut solved = i;
                for t in &solution {
                    solved = *mp.move_table.get(&(solved, *t)).unwrap();
                }
                solution.len() == *mp.perfect_pruning_table.get(&i).unwrap() as usize
                    && mp.goal_states.contains(&solved)
            }
        }
    }
}
