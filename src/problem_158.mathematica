(* Project Euler 158: https://projecteuler.net/problem=158

Let p(n) denote the number of length-n strings built from distinct letters of the 26-letter
alphabet for which exactly one adjacent comparison is increasing, meaning exactly one index i
with 2<=i<=n satisfies s_i > s_{i-1}. The task is to compute max_{1<=n<=26} p(n).

For fixed n, first choose the underlying letter set, then count valid orderings of that set.
After rank-normalization, only permutations of {1,...,n} matter, so p(n)=C(26,n)*A(n,1),
where A(n,1) is the Eulerian number for exactly one ascent. A direct combinatorial derivation
of A(n,1) is immediate: if there is at most one ascent, the permutation is decreasing on each
side of a cut. Selecting a nonempty proper subset S for the left block determines a unique
candidate (both blocks forced to be decreasing). Exactly n-1 choices, namely suffix intervals
of largest elements, produce zero ascents at the cut; all other choices produce one ascent.
Hence A(n,1)=(2^n-2)-(n-1)=2^n-n-1.

Therefore p(n)=C(26,n)*(2^n-n-1), and we only need a scan over n=1..26. The arithmetic cost
is linear in alphabet size, O(26), with exact integer operations and negligible memory usage.
This is comfortably below any practical Project Euler limit.

Parallelization is performed over disjoint n-subranges. The range 1..26 is partitioned into
near-equal chunks based on $ProcessorCount; each kernel computes a local maximum over its
chunk, and the master kernel aggregates with Max. Because Max is associative and commutative,
reduction is deterministic and race-free.

The Wolfram Language implementation uses exact integers (Binomial and powers of 2), compact
helper functions for A(n,1) and p(n), explicit kernel provisioning from $ProcessorCount, and
ParallelMap over chunked workloads. The final expression evaluates solve[] and returns the
exact maximal value with no side output. *)

nCores = $ProcessorCount;

ClearAll[
  ascentOnePermutations,
  pValue,
  solve
];

ascentOnePermutations[n_Integer?Positive] := 2^n - n - 1;

pValue[n_Integer?Positive] := Binomial[26, n]*ascentOnePermutations[n];

solve[] := Module[
  {nValues = Range[1, 26], workers, chunkSize, chunks, partialMaxima},
  If[$KernelCount < nCores, LaunchKernels[nCores - $KernelCount]];
  workers = Max[1, Min[nCores, Length[nValues]]];
  chunkSize = Ceiling[Length[nValues]/workers];
  chunks = Partition[nValues, UpTo[chunkSize]];
  DistributeDefinitions[pValue];
  partialMaxima = ParallelMap[
    Max[pValue /@ #] &,
    chunks,
    Method -> "CoarsestGrained"
  ];
  Max[partialMaxima]
];

solve[]
