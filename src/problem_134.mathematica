(* Project Euler Problem 134 - https://projecteuler.net/problem=134

   For each pair of consecutive primes p1 < p2 with 5 <= p1 <= 10^6, define S as the least positive integer that ends
   with the decimal digits of p1 and is divisible by p2. The task is to compute the exact sum of these minimal S values.

   Writing d = floor(log10(p1)) + 1 and B = 10^d, every integer ending in p1 has form N = x B + p1 with x >= 0. We need
   the minimal x such that x B + p1 == 0 (mod p2). Since p2 is a prime greater than 5, gcd(B, p2) = 1, so B is invertible
   modulo p2 and the linear congruence has the unique residue class x == -p1 B^(-1) (mod p2). Taking its least
   nonnegative representative gives the unique minimal candidate N in this class, hence the required S.

   Therefore each prime pair contributes S(p1, p2) = p1 + B * Mod[-p1 * B^(-1), p2], with all operations exact over Z.
   No search over decimal concatenations is needed; each term is obtained by one inverse modulo p2 and constant-time
   arithmetic on machine-size integers (here d <= 6). The dominant cost is prime generation up to the first prime beyond
   10^6 and processing O(pi(10^6)) pairs. Using a sieve-backed prime table, complexity is near-linear for generation and
   O(pi(10^6) log p2) for modular inverses, which is easily feasible at this scale.

   The workload is embarrassingly parallel: each consecutive pair is independent. We split the full pair list into
   contiguous chunks sized from $ProcessorCount, dispatch each chunk to a kernel, compute local chunk totals, and combine
   by Total. This reduction is associative and deterministic, so parallel execution preserves exact reproducibility.

   In Wolfram Language, primes are obtained via PrimePi and Prime, pairing is done by aligned slices, and each term uses
   IntegerLength, PowerMod with exponent -1 for modular inversion, and Mod for canonical residues. Exact integers are
   used throughout; no floating arithmetic, randomness, I/O, or external state is involved.
*)

nCores = $ProcessorCount;
If[Length[Kernels[]] == 0, LaunchKernels[]];
If[Length[Kernels[]] < nCores, LaunchKernels[nCores - Length[Kernels[]]]];

pairValue[{p1_Integer, p2_Integer}] := Module[{b, inv, x},
  b = 10^IntegerLength[p1];
  inv = PowerMod[b, -1, p2];
  x = Mod[-p1 inv, p2];
  x b + p1
]

DistributeDefinitions[pairValue];

solve[] := Module[{limit, count, primes, pairs, chunkSize, chunks},
  limit = 10^6;
  count = PrimePi[limit];
  primes = Prime[Range[count + 1]];
  pairs = Transpose[{primes[[3 ;; count]], primes[[4 ;; count + 1]]}];
  chunkSize = Ceiling[Length[pairs]/nCores];
  chunks = Partition[pairs, UpTo[chunkSize]];
  Total[
    ParallelMap[
      Total[pairValue /@ #] &,
      chunks,
      Method -> "CoarsestGrained"
    ]
  ]
]

solve[]
