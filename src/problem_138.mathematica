(* Project Euler Problem 138 - https://projecteuler.net/problem=138

   Let an isosceles triangle have equal sides L, even base b, and integer altitude h from the apex. We seek triangles
   for which h = b + 1 or h = b - 1, and must compute the sum of L over the first twelve nondegenerate solutions.

   Write b = 2 a with a >= 1. Then h = 2 a +- 1 and the right-triangle split gives
   L^2 = a^2 + h^2 = a^2 + (2 a +- 1)^2 = 5 a^2 +- 4 a + 1.
   Rearranging by completing the square:
   (5 a + 2)^2 - 5 L^2 = -1 for h = 2 a + 1, and
   (5 a - 2)^2 - 5 L^2 = -1 for h = 2 a - 1.
   Hence both branches lie on the negative Pell curve x^2 - 5 y^2 = -1 with y = L and x == +-2 (mod 5).

   Positive solutions of x^2 - 5 y^2 = -1 form the orbit
   x + y Sqrt[5] = (2 + Sqrt[5]) (9 + 4 Sqrt[5])^k, k >= 0.
   Their y-components are 1, 17, 305, 5473, ...; y = 1 corresponds to the degenerate base b = 0 and is discarded.
   For k >= 1 one gets valid triangles alternating between h = b - 1 and h = b + 1.
   Using standard Lucas-Fibonacci identities on Pell powers yields the closed form
   L_k = F_{6 k + 3}/2 for k >= 1, where F_n is the Fibonacci sequence with F_1 = F_2 = 1.

   Therefore the required value is Sum_{k=1}^{12} F_{6 k + 3}/2. This is exact integer arithmetic throughout.
   Complexity is O(K log K) bit operations via fast-doubling Fibonacci evaluation, with K = 12, so runtime is trivial.

   Parallelization is performed over disjoint index blocks of k. Each kernel computes a local partial sum of L_k on its
   assigned block, and global aggregation is Total over partial sums. The reduction is associative, race-free, and fully
   deterministic.

   The Wolfram Language implementation uses Fibonacci, Quotient for exact halving, Table/Range partitioning, and
   ParallelMap for dynamic multicore execution sized from $ProcessorCount, wrapped in a single solve[] entry point.
*)

nCores = $ProcessorCount;
If[Length[Kernels[]] == 0, LaunchKernels[]];
If[Length[Kernels[]] < nCores, LaunchKernels[nCores - Length[Kernels[]]]];
workerCount = Max[1, Length[Kernels[]]];

lValue[k_Integer?Positive] := Quotient[Fibonacci[6 k + 3], 2]

DistributeDefinitions[lValue];

solve[] := Module[{target, indices, chunkSize, chunks, partialSums},
  target = 12;
  indices = Range[target];
  chunkSize = Ceiling[target/workerCount];
  chunks = Partition[indices, UpTo[chunkSize]];
  partialSums = ParallelMap[
    Total[lValue /@ #] &,
    chunks,
    Method -> "CoarsestGrained"
  ];
  Total[partialSums]
]

solve[]
