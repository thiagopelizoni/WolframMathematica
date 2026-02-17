(* Project Euler Problem 129 - https://projecteuler.net/problem=129

   Let R(k) = (10^k - 1)/9, the repunit with k decimal ones. For integers n coprime to 10, define A(n) as the least
   k such that R(k) is divisible by n. The task is to find the least n for which A(n) exceeds 10^6.

   Divisibility by repunits is equivalent to a modular recurrence. Setting r_1 = 1 mod n and
   r_{k+1} = (10 r_k + 1) mod n, we have r_k congruent to R(k) mod n. Hence A(n) is the first index where r_k = 0.
   This avoids large integer construction and uses only modular arithmetic.

   A pigeonhole argument on residues of R(1), ..., R(n) modulo n implies A(n) <= n when gcd(n, 10) = 1, so any solution
   of A(n) > 10^6 must satisfy n > 10^6. Therefore the search can start at 10^6 + 1 and move upward.

   For each candidate n, we only need to decide whether A(n) > L with L = 10^6. We iterate the recurrence up to L steps
   and stop early if residue 0 appears; surviving past step L proves A(n) > L. This turns each test into O(min(A(n), L))
   modular updates and keeps memory O(1).

   Parallelization is over disjoint contiguous blocks of candidate n values. Inside a block, each n is independent, so
   ParallelMap evaluates threshold tests concurrently on all kernels. The boolean results preserve candidate order; the
   first True in the block yields the smallest valid n in that block. If absent, search advances to the next block.

   The Wolfram Language implementation uses exact integer arithmetic, deterministic block generation, a pure modular loop
   for the threshold predicate, and associative/ordered aggregation of parallel results in solve[].
*)

nCores = $ProcessorCount;
LaunchKernels[];

exceedsAThreshold[n_Integer?Positive, limit_Integer?Positive] := Module[{r, k},
  If[Mod[n, 2] == 0 || Mod[n, 5] == 0, Return[False]];
  r = Mod[1, n];
  k = 1;
  While[r != 0 && k <= limit,
    r = Mod[10 r + 1, n];
    k++
  ];
  k > limit
]

candidateBlock[start_Integer?Positive, blockSize_Integer?Positive] := Select[
  Range[start, start + blockSize - 1],
  Mod[#, 2] == 1 && Mod[#, 5] != 0 &
]

DistributeDefinitions[exceedsAThreshold, candidateBlock];

solve[] := Module[{limit, start, blockSize, block, flags, pos},
  limit = 10^6;
  start = limit + 1;
  blockSize = 50 nCores;
  While[True,
    block = candidateBlock[start, blockSize];
    flags = ParallelMap[exceedsAThreshold[#, limit] &, block];
    pos = FirstPosition[flags, True, Missing["NotFound"]];
    If[pos =!= Missing["NotFound"],
      Return[block[[pos[[1]]]]]
    ];
    start += blockSize
  ]
]

solve[]
