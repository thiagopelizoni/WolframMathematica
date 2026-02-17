(* Project Euler Problem 124 - https://projecteuler.net/problem=124

   Define rad(n) as the product of the distinct prime divisors of n, with rad(1) = 1. For 1 <= n <= 100000, form the
   ordered sequence obtained by sorting integers by increasing rad(n), breaking ties by increasing n itself. If E(k)
   denotes the k-th term in this ordering, the problem asks for E(10000).

   The ordering is lexicographic on pairs (rad(n), n). Thus the task is reduced to computing rad(n) for the full range
   and selecting the 10000-th element after sorting these pairs. The arithmetic core is the squarefree kernel map
   n -> prod_{p|n} p, a multiplicative function evaluated independently at each n. Since each value depends only on n,
   there is no coupling between subproblems.

   For each n, rad(n) can be recovered from prime factorization by multiplying the distinct prime bases. With modern
   integer factorization routines on inputs up to 10^5, this is fast, and the global cost is near-linear in practice
   with a modest polylogarithmic factor. Sorting 10^5 pairs adds O(N log N) comparisons, which is entirely feasible for
   Project Euler bounds.

   Parallelization is naturally embarrassingly parallel over disjoint integer subranges. We partition 1..N into balanced
   chunks, evaluate all (rad(n), n) pairs inside each chunk on separate kernels with ParallelMap, and aggregate partial
   lists via Join. Because each chunk is independent and the final merge uses associative list concatenation followed by
   deterministic global sorting, there are no race conditions and reproducibility is preserved.

   The Wolfram Language implementation uses exact integer arithmetic throughout, FactorInteger to extract prime support,
   Table for local chunk evaluation, ParallelMap for multicore distribution, SortBy with lexicographic keys for ranking,
   and a single solve[] entry point returning the required integer.
*)

nCores = $ProcessorCount;
LaunchKernels[];

radValue[n_Integer?Positive] := Times @@ (First /@ FactorInteger[n])

chunkBounds[limit_Integer?Positive, chunkSize_Integer?Positive] := Table[
  {i, Min[i + chunkSize - 1, limit]},
  {i, 1, limit, chunkSize}
]

DistributeDefinitions[radValue];

solve[] := Module[{limit, target, chunkSize, bounds, pairs, sorted},
  limit = 100000;
  target = 10000;
  chunkSize = Max[1000, Ceiling[limit/(8 nCores)]];
  bounds = chunkBounds[limit, chunkSize];
  pairs = Join @@ ParallelMap[Table[{radValue[n], n}, {n, #[[1]], #[[2]]}] &, bounds];
  sorted = SortBy[pairs, {First, Last}];
  sorted[[target, 2]]
]

solve[]
