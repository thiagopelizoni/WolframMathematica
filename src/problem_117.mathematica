(* Project Euler Problem 117 — https://projecteuler.net/problem=117
   
   Red, Green, and Blue Tiles
   
   A row of n = 50 unit cells is to be tiled using grey unit squares
   (length 1), red oblongs (length 2), green oblongs (length 3), and
   blue oblongs (length 4). Colours may be freely mixed. Determine
   the total number of distinct tilings.
   
   Mathematical analysis:
   A tiling is uniquely specified by an ordered sequence of tiles
   whose lengths sum to n. Let a, b, c, d be the respective counts
   of grey, red, green, and blue tiles, so a + 2b + 3c + 4d = n.
   The number of distinct orderings for a given (a,b,c,d) is the
   multinomial coefficient (a+b+c+d)! / (a! b! c! d!). Setting
   a = n - 2b - 3c - 4d, the total count is
   
     f(n) = Sum_{b,c,d >= 0, 2b+3c+4d <= n}
              (n - b - 2c - 3d)! / ((n-2b-3c-4d)! b! c! d!).
   
   Equivalently, f satisfies the linear recurrence f(n) = f(n-1) +
   f(n-2) + f(n-3) + f(n-4) with f(0)=1, f(k)=0 for k<0, which
   corresponds to the generating function 1/(1-x-x^2-x^3-x^4).
   The test value f(5) = 15 is easily verified.
   
   The multinomial sum has O(n^3 / 48) terms for n = 50 — a few
   thousand at most — each requiring one multinomial evaluation.
   This is trivially feasible in O(n^3) small-integer operations.
   
   Parallelization strategy:
   The outer index d ranges over 0..floor(n/4) = 12. Each value
   of d defines a completely independent sub-sum over (b, c). We
   distribute the multinomial function to all kernels and compute
   the 13 partial sums via ParallelTable, one per value of d,
   then aggregate with Total. No shared state is needed.
   
   Implementation plan:
   Detect cores, launch kernels. Define a helper that computes the
   inner double sum for a given d using exact Multinomial
   coefficients. Distribute definitions, run ParallelTable over d,
   and return Total as the answer.
*)

nCores = $ProcessorCount;
LaunchKernels[];

innerSum[n_, d_] := Module[{s = 0, cMax, bMax, a, t},
  cMax = Floor[(n - 4 d)/3];
  Do[
    bMax = Floor[(n - 3 c - 4 d)/2];
    Do[
      a = n - 2 b - 3 c - 4 d;
      t = a + b + c + d;
      s += Multinomial[a, b, c, d],
      {b, 0, bMax}
    ],
    {c, 0, cMax}
  ];
  s
]

DistributeDefinitions[innerSum];

solve[] := Module[{n = 50},
  Total[ParallelTable[innerSum[n, d], {d, 0, Floor[n/4]}]]
]

solve[]
