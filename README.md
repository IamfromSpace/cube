## Cube Data Structures

There's a couple things we do to both limit calculation while also staying as compact as possible. 
This guide tries to avoid the theoretical and focus on the actual memory layout of the tricks we'll use.

To help us out here, we're going to consider an extremely simple twisty puzzle that still has multiple symmetries.

It's laid out like this:

```
0   3
  2
1   4
```

We can spin the left triangle clockwise:

```
1   3
  0
2   4
```

Or counter-clockwise:

```
2   3
  1
0   4
```

We can spin the right triangle clockwise:

```
0   2
  4
1   3
```

Or counter-clockwise:

```
0   4
  3
1   2
```

For symmetries, it's possible to mirror L/R:

```
3   0
  2
4   1
```

Or U/D:

```
1   4
  2
0   3
```

Or we can do both, which is equivalent to a 180deg rotation.

```
4   1
  2
3   0
```

We're going to now build three arrays or vectors to help us solve this puzzle extremely efficiently.
Vectors and arrays are helpful for a number of reasons.
First, we often use indexes as implicit information.
Not only can we look up something at index 5, but the fact that it is at index 5 also tells us something, even though the indexes took no space in memory.
Also, we are extremely compact in memory and don't need to jump around to find things by being contiguous, and we are very likely to take advantage of caching.

Also, arrays work nicely for us, because Permutations and Patterns can be perfectly indexed or perfectly hashed.
We just need to calculate the Lhemer code, which is a sort of mixed radix encoding.

The three arrays are a representative table, a move table, and a pruning table.
While there's a slightly more memory efficient approach for just pruning tables, the other tables have a lot of benefits in avoiding computation that make them worthwhile.

While we can perfectly index our puzzles permutations, we can perfectly index a single symmetry representative for each equivalence class.
Our representative table then just assigns arbitrary indexes to them that we'll use in all other tables.
It doesn't matter what they are, just that we commit to our choice.

We enumerate from 0 to the maximum possible states (`5!` or 120), decode it into a puzzle state, and find the smallest symmetry representative.
Symmetry representatives of X are discovered by getting the permutation SXS^-1.
Intuitively, if you consider some move set on a cube, you'll just get a different color if you first rotate the cube, apply the move set, and then undo the rotation.
It's not really unique, just the colors are moved around.

At this point we push the newly discovered smallest representative into a `BTreeSet`, so we can easily dedup the values we find.
For this particular puzzle, it's 18.
We need to discard half of all possibilities because they have odd parity, and then symmetry means we have about 1/4 of the 60 possible states.
We have slightly more representatives, because states like the identity don't find unique states through symmetry.

Now we can turn this into a sorted array and we have an index for every unique symmetry representative.
Our representative table is complete.
From now on, we'll either work with this index or try to get to the representative index as quickly as possible.

```
[ 0   // [0, 1, 2, 3, 4]
, 2   // [2, 0, 1, 3, 4]
, 4   // [4, 0, 1, 2, 3]
, 8   // [3, 1, 0, 2, 4]
, 10  // [0, 3, 1, 2, 4]
, 12  // [2, 3, 0, 1, 4]
, 14  // [4, 2, 0, 1, 3]
, 16  // [1, 4, 0, 2, 3]
, 18  // [3, 4, 0, 1, 2]
, 21  // [1, 0, 3, 2, 4]
, 23  // [3, 0, 2, 1, 4]
, 31  // [1, 3, 2, 0, 4]
, 33  // [3, 2, 1, 0, 4]
, 37  // [2, 4, 1, 0, 3]
, 39  // [4, 3, 1, 0, 2]
, 58  // [3, 4, 2, 0, 1]
, 61  // [1, 0, 2, 4, 3]
, 119 // [4, 3, 2, 1, 0]
]
```

The above table just stores the Lehmer code to be efficient, so we include the decoded permutation in a comment too.
For example, index 0, is also really the value 0 (the Lehmer code for [0, 1, 2, 3, 4]).
Notice how representative 5 doesn't need to encode that 5 anywhere, it just is a index 5.
Most of the time we won't need to, but when we need to get a position's representative index, we can do so with a binary search in O(log(n)) time, since the array is sorted.

To avoid constantly having to move out of this representative index into the puzzle state to apply permutations, we're going to the build a move table.
There are |representatives| * |moves| entries in this table.
Again, we can use perfect indexing to get a flat table that avoids keys taking up space in memory.
Representative `i` and move `j` are stored at index `i * |moves| + j`.
Since it's common that we'll want to consider all moves on some representative, this is also conveniently puts them all right next to each other in memory.

Each entry in the table is the sym-coordinate from applying move `j` to representative `i`.
A sym-coordinate is the index a representative and the symmetry that needs to be applied to get the actual coordinate the move would have given us.
Undoing all this symmetry is some work, but it's work we can defer until we've identified the solve we want.
During the searching processes, we don't need to undo symmetries.

```
[ (1, 1)
, (1, 255)
, (1, 0)
, (1, 2)
, (0, 255)
, (1, 1)
, (2, 0)
, (7, 0)
, (1, 0)
, (7, 255)
, (6, 0)
, (11, 1)
, (9, 255)
, (4, 255)
, (12, 0)
, (11, 0)
, (3, 255)
, (9, 255)
, (5, 2)
, (10, 2)
, (10, 255)
, (4, 2)
, (8, 2)
, (6, 1)
, (2, 0)
, (11, 2)
, (8, 0)
, (5, 1)
, (2, 255)
, (1, 0)
, (13, 2)
, (10, 1)
, (6, 0)
, (5, 2)
, (15, 255)
, (8, 1)
, (4, 255)
, (3, 255)
, (9, 1)
, (16, 255)
, (4, 2)
, (5, 255)
, (13, 0)
, (7, 1)
, (12, 255)
, (3, 0)
, (6, 2)
, (2, 1)
, (3, 0)
, (11, 255)
, (14, 0)
, (13, 1)
, (10, 0)
, (7, 2)
, (14, 2)
, (12, 1)
, (12, 0)
, (13, 2)
, (17, 255)
, (14, 1)
, (8, 0)
, (8, 2)
, (8, 1)
, (8, 255)
, (9, 2)
, (9, 0)
, (9, 255)
, (9, 1)
, (14, 0)
, (14, 2)
, (14, 1)
, (14, 255)
]
```

With the move table, we can very quickly apply many moves.
We don't need to decode their Lehmer code, apply permutations, or consider symmetries.
We simply start with a representative and continually look up the next representative from the next move.
This helps us both in solving and in building a pruning table.

For example, lets say we start with the identity, we already established that this has Lehmer code of 0, and that it's at index 0 of our rep table.
Our first four entries of our move table are what happens if we apply the four possible turns on the identity.
Note that all of them lead to index 1 of our rep table (which has Lehmer code 2, which is permutation [2, 0, 1, 3, 4]), but all of them are reduced by a different symmetry.
So interestingly enough, no matter the move we apply to the identity, we always end up in the same place!

If we now take a look at the move table for rep index 1, we see a slightly more interesting story.
Unsurprisingly, one of the moves takes us back to the identity.
Another turn keeps us in the same place: this is because we're applying the same turn, which is equivalent to having applied a single opposite turn, which is the same thing with symmetry taken into account.
Two of the moves take us to totally new places, rep table index 2 (Lehmer code 4 and permutation [4, 0, 1, 2, 3]) and rep table index 7 (Lehmer code 16 and permutation [1, 4, 0, 2, 3]).

We can continue applying moves like this for as long as we'd like.

The pruning table is also just a flat array indexed by representatives.
Each entry is just 2b, either an uninitialized value or the number of moves to get to the goal state mod 3.
The mod 3 trick works because if we know that it's 13 moves to get to the goal state, applying a single move must get us closer (12), further (14), or the same distance (13) away from the goal.
Since 13 mod 3 is 1, we need to apply a move that gets us to 12 mod 3 (0) and 14 mod 3 (2) would be a step backwards.

We fill the table by just doing a BFS from the goal state(s).
All the goal states are marked with 0 in the table, and tracked in a queue.
For each item in the queue, we apply all moves (via our move table!), and each representative we find is then set to 1 and added to the queue.
If we find a value we've seen before, we have finished that branch.
When the pruning table gets full, we can do a reverse search on the representatives that haven't been solved for.

```
[ 0  // 0
, 1  // 1
, 2  // 2
, 1  // 4
, 1  // 4
, 1  // 4
, 0  // 3
, 2  // 2
, 1  // 4
, 2  // 5
, 0  // 3
, 0  // 3
, 1  // 4
, 0  // 3
, 1  // 4
, 2  // 5
, 0  // 6
, 2  // 5
]
```

We see the pruning table, which actually stores the modulo 3 of the number of moves to solve the position.
So to make this a bit more readable, we also put a comment showing the actual number of moves required.
We can see that our worst position is the 16th index, requiring 6 moves.
But to actually see that it takes 6 moves, we need to trace the steps, as from our table, we only know that the number of moves required modulo 3 is zero.
So it could take 3, 6, 9, or any other multiple of three.

If we consider index 16 in our move table, we see that all moves take us to representative index 9.
This makes sense, as it has a move count modulo 3 of 2, exactly what we're looking for--applying a single move can only get us one step closer, one step further, or the same distance.
If we then look at the move table for index 9, we see that it can take us to indexes 4, 3, 9, 16.
Index 16 has a move count modulo 3 of 0, so it must be worse (that's where we came from).
Index 9 has a move count modulo 3 of 2, so it must be the same (that's where we are now).
Both indexes 4 and 3 have a move count modulo 3 of 1, so both of these states can be solved more easily.
It doesn't matter which one we choose, we just continue this process until we find the goal state.

Move tables can and should be decomposed to be efficient.
For example, we can make distinct tables for corner permutation vs corner orientation.
However, pruning tables can be the composite of multiple move tables to provide a more accurate lower bound when searching.

Move tables only work on permutations and patterns (partial piece tracking), but they don't work when we ignore the exact placement of pieces tracked.
However, even though we need the full position of the pieces in the move tables, it may still be worthwhile to construct move tables that ignore this information.
This works for composite tables when we're looking to make our table more accurate, but it would be infeasible to have the composite table encode the exact position of the pieces.

Let's look at composite MoveTables by making our very simple puzzle slightly more complicated.
Now, there are two layers, that don't exchange pieces but affected simultaneously by turns.
This additional layer is extremely simple, instead of being two triangles, we have two lines.
Any left side turn swaps the left two pieces and any right side turn swaps the right side pieces.

The solved puzzle then might look like this:

```
  a b c

 0     3
    2
 1     4
```

If we apply a left side clockwise turn we'd effect both the top layer and bottom layer, but no pieces would move between them:

```
  b a c

 1     3
    0
 2     4
```

While it may be overkill for such a small puzzle, we can demonstrate how composite move tables can be used to be more compact.
Our move table for our two triangles is unchanged, but we'll now build a move table for our two lines.
We won't build a rep table, because our secondary tables can't actually be reduced by symmetry.
This is because the first move table defines the symmetry used by all other move tables.
This makes sense because the two different parts of the puzzle cannot be reduced by _different_ symmetries, as this would just be an illegal move.

All told, that gives us 12 entries, six possible positions with two possible turns each.

```

```


## Wing Edge Solving

### First Phase

We're trying to go from a full cube to G1, `<U, Uw2, F2, Fw2, R2, Rw2, B2, Bw2, L2, Lw2, D2, Dw2>`.
This is going to divide the cube into four orbits, so our...

### Final Phase

At this point, the cube is in G1, `<U, Uw2, F2, Fw2, R2, Rw2, B2, Bw2, L2, Lw2, D2, Dw2>`.
With these generators, there are four orbits that do not interact: the eight even pieces on the up and down faces, the eight odd pieces on the up and down faces, the four even pieces in the middle UD slice, and the four odd pieces in the middle UD slice.
Each middle slice as a whole has even parity, and there's even parity between the two up and down faces.
This ultimately gives us 12x as many states as a 3x3x3 G1 cube, with a 1.6x branching factor.

The parity between up and down is hard to take advantage of, because considering them simultaneously is over 50M positions (considering a 16x reduction symmetry), which is much too large for a move table (`8!*8!/2/16*16*5`, which is over 4GB).
Also, move tables ideally subdivided small enough to fit into CPU caches, so moves can be applied with low latency.

We want our pruning tables to be as large as feasibly possible for these problems.
There's never any hope of fitting them in CPU caches, with the level of pruning power required to be performant.

Our simplest approach is very similar to G1 3x3x3 solving.
We use three permutations: even UD, odd UD, UD slice.
From here, we can make two composites move tables, and make pruning tables out of them: UD, and even UD+UD slice.
We can get an additional virtual table by creating a translated pruning table through mirroring on the even UD+UD slice table to get the odd UD+UD slice table.
We then make a final composite pruning table out of the three.

Our foundational move tables are quite small, with all raw indexes fitting in u16.
Each supports an ~8x reduction by symmetry, as they support UF2Symmetry.
The even and odd UD move tables are (`8!/8*16*3B`) ~250KB.
The slice table is slightly smaller, at ~125KB, since we can take advantage of the middle slice's parity.

Our larger pruning table then, UD, is about 50MB (`8!*8!/8/4`, remember we can't take advantage of parity).
Our smaller one, even UD+UD slice, is only about 400KB (`8!*4!*4!/2/8/4`).

Alternatively, we can also take a huge table approach.
We consider three permutations: even UD, odd UD, and even UD slice.
However, we'll only use 4x symmetry reduction here via U2F2Symmetry, since the even UD slice doesn't support UF2.
We'll make a pruning table out of a huge composite made from all three.
Then, we'll make a translated move table through mirroring, and make a final composite pruning table with it.
This mirroring ensures that we're pruning on the odd UD slice too, and getting very large overall pruning.

The foundational pruning table is huge, at over 2.4GB (`8!*8!*4!/4/4`), but just inside of possibility.

### Alternate Final Phases

#### H1-like

An H1 like cube has generators `<U, Uw2, F, Fw2, R2, Rw2, B, Bw2, L2, Lw2, D, Dw2>`.
This splits the edges into two orbits: evens and odds.
There is overall even parity, but each orbit can be odd.

The issue with this, is that it's still staggeringly huge.
At `12!*12!/2` states, its nearly 6Mx larger than 3x3x3 G1.

#### Different Domino?

Instead of our current G1, we could alternatively use `<U, Uw, F2, Fw2, R2, Rw2, B2, Bw2, L2, Lw2, D, Dw>`, allowing quarter wide turns on the U and D faces.
This fuses the two middle slice orbits into one, with odd parity.
So this has `8!*8!/2*8!` states.
This is 140x (`8!*8!/2*8!/(8!*8!/2*4!*4!/2)`) larger than our other G1-like position, and 1,680x (`8!*8!/2*8!/(8!*8!/2*4!)`) larger than the 3x3x3 G1.
The upside of this approach is that it shifts the balance between the two phases, making the second phase smaller than the first.
The other G1-like phase is ~11x `(24!/(8!*8!/2*4!*4!/2)^2)` larger than phase 1, and this may make it impractical, because phase 1 is likely already harder, given that its one gigantic orbit.
The primary downside, is that this may overdo it: phase 2 would be ~1730x `((8!*8!/2*8!)^2/24!)` harder than phase 1.
