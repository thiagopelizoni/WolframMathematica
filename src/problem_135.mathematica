(* Project Euler Problem 135 - https://projecteuler.net/problem=135

   Let x > y > z > 0 be consecutive terms of an arithmetic progression and define n = x^2 - y^2 - z^2.
   The problem asks for how many n < 10^6 the Diophantine constraint has exactly ten distinct triples.

   Set y = a and common difference d > 0, so x = a + d and z = a - d. Then
   n = (a + d)^2 - a^2 - (a - d)^2 = a(4 d - a).
   Hence every solution corresponds to positive integers a and t = 4 d - a with n = a t, together with
   the arithmetic-progression integrality condition d = (a + t)/4 in Z and positivity z = a - d > 0.
   These become congruence and inequality constraints:
   a + t ≡ 0 (mod 4), t < 3 a, a >= 1, t >= 1.

   Therefore, for each n, the number of valid triples equals the number of ordered factor pairs (a, t) of n
   satisfying those two filters. Instead of factoring each n independently, we enumerate all admissible pairs
   globally. For each a, valid t are exactly one residue class modulo 4 up to
   tMax = Min(Floor[(10^6 - 1)/a], 3 a - 1). Each accepted pair contributes once to n = a t.

   The total work is proportional to Sum_{a<=N} min(N/a, 3 a), with N = 10^6 - 1, i.e. O(N log N).
   This is far below quadratic search and easily feasible in compiled kernel arithmetic.
   Memory is linear in N for a single frequency vector of solution counts.

   Parallelization is embarrassingly parallel over disjoint sets of a values. We split a by residue classes
   modulo the number of worker kernels, so each kernel receives a balanced mix of small and large a.
   Every kernel builds a local histogram for n via BinCounts; local histograms are reduced with Total,
   an associative deterministic aggregation with no shared mutable state.

   The Wolfram Language code uses exact integer operations only: Quotient, Mod, Range with step 4,
   and BinCounts for fast counting. Kernels are launched up to $ProcessorCount, helper definitions are
   distributed once, and solve[] returns Count[countVector, 10] as the required exact integer answer.
*)

nCores = $ProcessorCount;
If[Length[Kernels[]] == 0, LaunchKernels[]];
If[Length[Kernels[]] < nCores, LaunchKernels[nCores - Length[Kernels[]]]];
workerCount = Max[1, Length[Kernels[]]];

chunkCounts[uList_List, maxN_Integer] := Module[{indices},
  indices = Flatten[
    Table[
      Module[{vMax, v0},
        vMax = Min[Quotient[maxN, u], 3 u - 1];
        v0 = Mod[-u, 4];
        If[v0 == 0, v0 = 4];
        If[v0 > vMax, {}, u Range[v0, vMax, 4]]
      ],
      {u, uList}
    ],
    1
  ];
  BinCounts[indices, {1, maxN + 1, 1}]
]

DistributeDefinitions[chunkCounts];

solve[] := Module[{limit, maxN, slices, partials, counts},
  limit = 10^6;
  maxN = limit - 1;
  slices = Table[Range[k, maxN, workerCount], {k, 1, workerCount}];
  partials = ParallelMap[
    chunkCounts[#, maxN] &,
    slices,
    Method -> "CoarsestGrained"
  ];
  counts = Total[partials];
  Count[counts, 10]
]

solve[]
