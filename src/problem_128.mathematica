(* Project Euler Problem 128 - https://projecteuler.net/problem=128

   Tiles are arranged on a hexagonal spiral, numbered from the center. For tile n, define PD(n) as the number of
   absolute differences between n and its six neighboring tiles that are prime. It is known that PD(n) <= 3 for all n.
   The task is to find the 2000th tile for which PD(n) = 3.

   Let ring r >= 1 be the layer at hex distance r from the center. Ring r contains 6 r tiles, from
   3 r(r - 1) + 2 to 3 r(r + 1) + 1. A standard neighbor-difference analysis on axial coordinates shows that, for
   r > 1, only two boundary positions can satisfy PD = 3: the first tile of ring r,
   n1(r) = 3 r(r - 1) + 2, and the last tile of ring r, n2(r) = 3 r(r + 1) + 1.

   For n1(r), the three relevant prime differences reduce to 6 r - 1, 6 r + 1, and 12 r + 5; thus PD(n1(r)) = 3 iff
   all three are prime. For n2(r) with r > 1, the criterion is primality of 6 r - 1, 6 r + 5, and 12 r - 7. Therefore
   the search is reduced from a two-dimensional geometric walk to one-dimensional testing over ring index r.

   The algorithm enumerates r and emits n1(r), n2(r) when their primality triples pass, together with the exceptional
   tiles 1 and 2. Prime tests are on O(r) sized integers (linear forms in r), while candidate tiles scale as O(r^2).
   To reach the 2000th hit, r is only around 7*10^4, so total complexity is near-linear in that bound with fast primality
   checks and negligible memory.

   Parallelization is done by splitting the r-range into disjoint chunks. Each kernel evaluates the same independent
   primality predicates on its chunk and returns local candidates. Partial lists are aggregated by Join, deduplicated by
   Union, sorted, and indexed. This decomposition is embarrassingly parallel and deterministic.

   In Wolfram Language, we use exact integer arithmetic, PrimeQ for primality, Reap/Sow for efficient local collection,
   ParallelMap for multicore evaluation, and an outer doubling loop in solve[] that guarantees enough candidates before
   selecting the 2000th element.
*)

nCores = $ProcessorCount;
LaunchKernels[];

candidateChunk[rRange_List] := Module[{r, p, bags},
  bags = Reap[
    For[r = rRange[[1]], r <= rRange[[2]], r++,
      p = PrimeQ[6 r - 1];
      If[p && PrimeQ[6 r + 1] && PrimeQ[12 r + 5],
        Sow[3 r (r - 1) + 2]
      ];
      If[r > 1 && p && PrimeQ[6 r + 5] && PrimeQ[12 r - 7],
        Sow[3 r (r + 1) + 1]
      ]
    ]
  ][[2]];
  If[bags === {}, {}, First[bags]]
]

DistributeDefinitions[candidateChunk];

solve[] := Module[{target, rMax, chunkSize, ranges, partials, values},
  target = 2000;
  rMax = 4096;
  While[True,
    chunkSize = Max[256, Ceiling[rMax/(8 nCores)]];
    ranges = Table[{i, Min[i + chunkSize - 1, rMax]}, {i, 1, rMax, chunkSize}];
    partials = ParallelMap[candidateChunk, ranges];
    values = Sort[Union@Join[{1, 2}, Flatten[partials]]];
    If[Length[values] >= target,
      Return[values[[target]]]
    ];
    rMax *= 2
  ]
]

solve[]
