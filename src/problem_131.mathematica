(* Project Euler Problem 131 - https://projecteuler.net/problem=131

   We seek how many primes p < 10^6 admit a positive integer n such that n^3 + n^2 p is a perfect cube.

   Write n^3 + n^2 p = m^3 with m > n. Rearranging gives m^3 - n^3 = n^2 p. Let g = gcd(m, n), m = g v, n = g u,
   gcd(u, v) = 1. Then g(v^3 - u^3) = u^2 p. Because gcd(u, v^3 - u^3) = 1, we must have u^2 dividing g; set
   g = u^2 t. Substitution yields t(v^3 - u^3) = p. Since p is prime and t, v^3 - u^3 are positive integers, t = 1
   and v^3 - u^3 = p. Therefore n = u^3 and p is a difference of two coprime cubes.

   Factorization gives p = (v - u)(v^2 + uv + u^2). Primality forces v - u = 1, hence v = u + 1 and
   p = (u + 1)^3 - u^3 = 3 u^2 + 3 u + 1. Conversely, every prime of this form works by choosing n = u^3, so the
   problem is exactly to count u >= 1 with 3 u^2 + 3 u + 1 < 10^6 prime.

   The bound on u follows from a quadratic inequality, so only O(sqrt(10^6)) candidates exist (576 values). Candidate
   generation is linear in that bound, primality tests dominate, and memory usage is O(1) besides small vectors. The
   workload is trivial for modern hardware.

   Parallelization is embarrassingly parallel over disjoint u-ranges. We partition 1..uMax into contiguous chunks, map a
   primality-count function across chunks with ParallelMap, and aggregate with Total. This is deterministic because each
   chunk is independent and addition is associative.

   The Wolfram Language implementation uses exact integer arithmetic throughout, PrimeQ for primality, closed-form
   candidate construction, and solve[] returning the final count as a single integer output.
*)

nCores = $ProcessorCount;
LaunchKernels[];

primeForm[u_Integer?Positive] := 3 u u + 3 u + 1

countPrimeChunk[uChunk_List] := Count[PrimeQ /@ (primeForm /@ uChunk), True]

DistributeDefinitions[primeForm, countPrimeChunk];

solve[] := Module[{limit, uMax, chunkSize, chunks, partials},
  limit = 10^6;
  uMax = Floor[(Sqrt[12 limit - 3] - 3)/6];
  chunkSize = Max[16, Ceiling[uMax/nCores]];
  chunks = Table[Range[i, Min[i + chunkSize - 1, uMax]], {i, 1, uMax, chunkSize}];
  partials = ParallelMap[countPrimeChunk, chunks];
  Total[partials]
]

solve[]
