(* Project Euler Problem 137 - https://projecteuler.net/problem=137

   Define A_F(x) = Sum_{k>=1} F_k x^k, where F_k are Fibonacci numbers with F_1 = F_2 = 1. A positive integer n is a
   golden nugget when there exists rational x such that A_F(x) = n. The goal is to determine the 15th such nugget.

   The generating function is A_F(x) = x/(1 - x - x^2). Setting x/(1 - x - x^2) = n gives
   n x^2 + (n + 1) x - n = 0. Rational x requires square discriminant:
   m^2 = (n + 1)^2 + 4 n^2 = 5 n^2 + 2 n + 1.
   Completing the square transforms this to the Pell-type conic
   (5 n + 1)^2 - 5 m^2 = -4.
   Thus nuggets correspond to positive integer points on u^2 - 5 v^2 = -4 with u == 1 (mod 5), via u = 5 n + 1.

   The identity L_t^2 - 5 F_t^2 = 4 (-1)^t links Pell solutions to Lucas and Fibonacci numbers. Restricting to the
   branch with u == 1 (mod 5) yields u_k = L_{4 k + 1}, and then n_k = (u_k - 1)/5. Standard addition identities reduce
   this to the compact closed form n_k = F_{2 k} F_{2 k + 1}. Hence the 15th nugget is exactly F_30 F_31.

   Algorithmically, once the reduction is established, computing the first K nuggets is O(K log K) bit operations using
   fast doubling for Fibonacci evaluation. For K = 15 this is negligible, and exact integer arithmetic keeps the result
   mathematically exact without numerical stability concerns.

   Parallelization is by disjoint index blocks in k. Each worker evaluates n_k on an independent contiguous subrange,
   producing a local list. Global aggregation is concatenation in block order, then direct indexing of the K-th element.
   This decomposition is race-free and deterministic because each block is independent and Join preserves ordered blocks.

   The Wolfram Language implementation uses Fibonacci for exact sequence values, ParallelMap over block descriptors for
   dynamic multicore usage, and a single solve[] entry point returning the exact integer answer.
*)

nCores = $ProcessorCount;
If[Length[Kernels[]] == 0, LaunchKernels[]];
If[Length[Kernels[]] < nCores, LaunchKernels[nCores - Length[Kernels[]]]];
workerCount = Max[1, Length[Kernels[]]];

goldenNugget[k_Integer?Positive] := Fibonacci[2 k] Fibonacci[2 k + 1]

DistributeDefinitions[goldenNugget];

solve[] := Module[{target, chunkSize, blocks, partials, nuggets},
  target = 15;
  chunkSize = Ceiling[target/workerCount];
  blocks = Select[
    Table[
      {1 + (i - 1) chunkSize, Min[target, i chunkSize]},
      {i, 1, workerCount}
    ],
    #[[1]] <= #[[2]] &
  ];
  partials = ParallelMap[
    Table[goldenNugget[k], {k, #[[1]], #[[2]]}] &,
    blocks,
    Method -> "CoarsestGrained"
  ];
  nuggets = Join @@ partials;
  nuggets[[target]]
]

solve[]
