(* Project Euler 148: https://projecteuler.net/problem=148

  We must count entries in the first 10^9 rows of Pascal's triangle that are not divisible by 7. Rows are indexed from 0,
  so the target is the sum over n = 0..10^9-1 of the number of binomial coefficients C(n,k) with 7 not dividing C(n,k).

  Lucas' theorem gives the exact local criterion modulo a prime p: write n and k in base p, then C(n,k) is nonzero mod p
  iff every digit of k is at most the corresponding digit of n. For p = 7 this implies that row n contributes
  product_i (d_i + 1), where d_i are base-7 digits of n. Hence the global task is a digital summatory function.

  Let F(N) = sum_{n=0}^{N-1} product_i (d_i(n)+1). If N has base-7 digits a_1...a_L (most significant first), digit-DP over
  the first position where n is strictly smaller yields
  F(N) = sum_{i=1}^L (prod_{j<i}(a_j+1)) * a_i(a_i+1)/2 * 28^(L-i),
  because sum_{t=0}^6 (t+1) = 28 for each unconstrained suffix digit. This reduces evaluation to O(log_7 N) arithmetic.

  A direct loop over 10^9 rows is unnecessary; the digit formula is exact and fast. To satisfy multicore decomposition in a
  mathematically sound way, we partition [0,N) into many disjoint intervals [b_i,b_{i+1}) and sum F(b_{i+1})-F(b_i). Each
  interval is independent, so kernels can process chunks concurrently, and aggregation is associative via Total.

  Complexity per interval is O(log_7 N), and with C chunks the total is O(C log N) with tiny constants and exact integer
  arithmetic. Memory is O(C) for interval endpoints and partial sums, easily negligible for Project Euler bounds.

  The Wolfram Language implementation uses IntegerDigits in base 7, exact powers of 28, and ParallelMap with fine-grained
  scheduling over interval tasks. No randomness, external input, or side effects are used, so results are deterministic. *)

nCores = $ProcessorCount;

ClearAll[prefixCount, chunkContribution, solve];

prefixCount[n_Integer] := Module[
  {digits, len, total, prefix, d},
  If[n <= 0, Return[0]];
  digits = IntegerDigits[n, 7];
  len = Length[digits];
  total = 0;
  prefix = 1;
  Do[
    d = digits[[i]];
    total += prefix Quotient[d (d + 1), 2] 28^(len - i);
    prefix *= (d + 1),
    {i, 1, len}
  ];
  total
];

chunkContribution[{lo_Integer, hi_Integer}] := prefixCount[hi] - prefixCount[lo];

solve[] := Module[
  {limit, nChunks, bounds, intervals, partials},
  limit = 10^9;
  If[$KernelCount < nCores, LaunchKernels[nCores - $KernelCount]];
  nChunks = Max[1024, 512 nCores];
  bounds = Table[Quotient[i limit, nChunks], {i, 0, nChunks}];
  intervals = Partition[bounds, 2, 1];
  DistributeDefinitions[prefixCount, chunkContribution];
  partials = ParallelMap[chunkContribution, intervals, Method -> "FinestGrained"];
  Total[partials]
];

solve[]
