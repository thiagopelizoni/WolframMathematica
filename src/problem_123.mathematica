(* Project Euler Problem 123 - https://projecteuler.net/problem=123

   Let p_n denote the n-th prime, and define r_n as the remainder of
   (p_n - 1)^n + (p_n + 1)^n upon division by p_n^2. The task is to find the least index n for which r_n exceeds
   10^10.

   Write p = p_n. By the binomial theorem, modulo p^2 only the constant and linear terms in p can survive. For even n,
   the linear contributions cancel and the constant terms add, giving r_n = 2. For odd n, constants cancel and the
   linear terms add, giving r_n congruent to 2 n p modulo p^2. Therefore only odd n can cross a large threshold, and
   the exact test is r_n = Mod[2 n p_n, p_n^2].

   This converts the problem to scanning odd indices n in increasing order until the inequality holds. If one checks
   all odd n up to N, the arithmetic part is O(N), while prime access up to p_N is handled by the kernel's prime
   infrastructure; asymptotically this is dominated by prime generation and lookup up to roughly p_N ~ N log N. For
   the Euler bound, the first hit is near n about 2*10^4, so the full workload is tiny.

   Parallelization is embarrassingly parallel within each search block. Each odd index in a block is independent, so we
   distribute the boolean threshold test across all kernels with ParallelMap. Every core works on disjoint n values,
   then we aggregate with FirstPosition on the boolean vector and map the local position back to the global index. If a
   block has no hit, the search continues with the next disjoint block.

   In Wolfram Language, we detect nCores via $ProcessorCount, launch kernels, and define a small exact-integer helper
   for odd-index remainders. The solve[] driver performs deterministic blockwise parallel search and returns the first
   valid n as an exact integer.
*)

nCores = $ProcessorCount;
LaunchKernels[];

oddRemainder[n_Integer?Positive] := Module[{p},
  p = Prime[n];
  Mod[2 n p, p^2]
]

DistributeDefinitions[oddRemainder];

solve[] := Module[{threshold, blockSize, start, ns, hits, pos},
  threshold = 10^10;
  blockSize = 4096 nCores;
  start = 1;
  While[True,
    ns = Range[start, start + 2 (blockSize - 1), 2];
    hits = ParallelMap[oddRemainder[#] > threshold &, ns];
    pos = FirstPosition[hits, True, Missing["NotFound"]];
    If[pos =!= Missing["NotFound"],
      Return[ns[[pos[[1]]]]]
    ];
    start += 2 blockSize
  ]
]

solve[]
