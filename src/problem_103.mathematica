(* Project Euler Problem 103 — https://projecteuler.net/problem=103
 
   Special Subset Sums: Optimum.
 
   A set A = {a_1, ..., a_n} of n distinct positive integers is a "special sum set" if for every
   pair of non-empty disjoint subsets B, C of A two conditions hold:
     (i)  S(B) != S(C)   — no two disjoint subsets share the same sum;
     (ii) |B| > |C|  =>  S(B) > S(C)  — a larger subset always has a larger sum.
   An optimum special sum set for a given n minimises S(A) = a_1 + ... + a_n.  The task is to
   find the optimum special sum set for n = 7 and report its "set string", the concatenation of
   its sorted elements as a single decimal numeral.
 
   Mathematical analysis.
   Condition (ii) for a sorted set a_1 < a_2 < ... < a_n is equivalent to requiring, for each k
   in {1, ..., floor((n-1)/2)}, that  a_1 + ... + a_{k+1} > a_{n-k+1} + ... + a_n.  This is
   because the smallest possible sum of a (k+1)-element subset is the sum of the k+1 smallest
   elements, and the largest possible sum of a k-element subset is the sum of the k largest.
 
   Condition (i) is equivalent to all 2^n - 1 non-empty subset sums being pairwise distinct.
   (Proof sketch: S(B) = S(C) for disjoint B, C iff S(B \ C) = S(C \ B) for disjoint symmetric
   differences; conversely, any two subsets with equal sums yield a disjoint pair via symmetric
   difference.)
 
   Heuristic seed.  The problem notes a constructive rule: given an optimum set A for size n,
   a near-optimum for size n+1 is B = {b, a_1+b, ..., a_n+b} where b is the middle element of
   A (the element at index ceil(n/2)).  For n = 6 the known optimum is {11,18,19,20,22,25},
   giving b = 20 and near-optimum B = {20,31,38,39,40,42,45} with S(B) = 255.  This heuristic
   is not guaranteed to be optimal—for n = 6 it overestimates by 2—so an exhaustive local
   search around the seed is required.
 
   Search strategy.  We perturb each element of the near-optimum by an offset in {-3,...,+3},
   producing up to 7^7 = 823 543 candidate 7-tuples.  After filtering for strict monotonicity
   (roughly 10–20% survive), each candidate is tested: first condition (ii) in O(n) time, then
   condition (i) by enumerating all 127 non-empty subset sums and checking uniqueness.  Total
   work is bounded by about 10^8 lightweight integer comparisons, trivially feasible.
 
   Parallelisation.  The candidate-testing phase is embarrassingly parallel: candidates are
   distributed across all CPU cores via ParallelSelect, each core independently evaluates the
   specialQ predicate, and the results are aggregated.  DistributeDefinitions propagates the
   predicate to all subkernels.  The minimum-sum valid set is then selected sequentially.
 
   Implementation sketch.  We store the near-optimum as a flat integer list, generate offset
   tuples via Tuples, translate to candidate sets, filter with Less@@#, and apply the two-phase
   predicate.  The set string is assembled via FromDigits on the flattened IntegerDigits of the
   winning set—returning a single integer for Mathematica's top-level output. *)

nCores = $ProcessorCount;
LaunchKernels[];

specialQ[s_List] := Module[{n = Length[s], sorted = Sort[s], sums},
  If[!And @@ Table[
    Total[sorted[[1 ;; k + 1]]] > Total[sorted[[n - k + 1 ;; n]]],
    {k, 1, Floor[(n - 1) / 2]}
  ], Return[False]];
  sums = Total /@ Rest[Subsets[sorted]];
  Length[Union[sums]] === Length[sums]
];

DistributeDefinitions[specialQ];

solve[] := Module[{nearOpt, candidates, valid, best},
  nearOpt = {20, 31, 38, 39, 40, 42, 45};
  candidates = Select[
    Map[nearOpt + # &, Tuples[Range[-3, 3], 7]],
    (Min[#] > 0 && Less @@ #) &
  ];
  valid = ParallelSelect[candidates, specialQ];
  best = First[SortBy[valid, Total]];
  FromDigits[Flatten[IntegerDigits /@ best]]
];

solve[]
