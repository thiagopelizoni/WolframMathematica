(*
Project Euler 111 — https://projecteuler.net/problem=111

For each digit d in {0,...,9}, define M(10,d) as the largest multiplicity of d among 10-digit primes, then
N(10,d) as the count of such primes and S(10,d) as their sum. The objective is Sum_d S(10,d).

Fix d and let k be the number of positions not equal to d. Then candidate numbers with exactly 10-k copies of
d are obtained by selecting k positions and assigning digits from {0,...,9}\{d} to those positions. This is
an exact parametrization: every 10-digit integer with exactly 10-k copies of d appears once, because its set of
non-d coordinates and corresponding digits are unique. Therefore M(10,d) is found by scanning k upward from 0
until at least one prime exists; then M(10,d)=10-k and S(10,d) is the sum over prime candidates at that first k.

The search is finite and small. For fixed k, candidate count is Binomial(10,k)*9^k. In practice one reaches a
prime at very small k, so work is dominated by low-order terms. Even if one pessimistically reached k=3 for all
d, the total primality checks remain around 10*Binomial(10,3)*9^3, easily feasible.

Parallelization is naturally embarrassingly parallel across d, since each digit class has independent candidate
generation and primality testing. We map the per-digit computation across all kernels, each producing one exact
integer S(10,d). Aggregation is a single Total reduction, associative and race-free, with no shared mutable
state.

The implementation uses exact integer arithmetic only. Digits are represented as fixed-length vectors, converted
to integers by a positional dot product with powers of ten. Candidate construction uses Subsets and Tuples;
primality is tested by PrimeQ; control flow uses deterministic selection of the first successful k.
*)

ClearAll[solve];

solve[] := Module[
  {
    nCores,
    n = 10,
    digits,
    powers,
    baseWeight,
    lowerBound,
    sumForDigit,
    digitSums
  },
  nCores = $ProcessorCount;
  digits = Range[0, 9];
  powers = 10^Reverse[Range[0, n - 1]];
  baseWeight = Total[powers];
  lowerBound = 10^(n - 1);
  sumForDigit = Function[d,
    Module[{choices, k, positions, assignments, pos, vals, base, v, total = 0, found = False},
      choices = DeleteCases[digits, d];
      For[k = 0, k <= n, k++,
        positions = Subsets[Range[n], {k}];
        assignments = Tuples[choices, k];
        Do[
          pos = positions[[i]];
          base = d*baseWeight - d*Total[powers[[pos]]];
          Do[
            vals = assignments[[j]];
            v = base + vals.powers[[pos]];
            If[
              v >= lowerBound && v > 5 && OddQ[v] && Mod[v, 5] != 0 && PrimeQ[v],
              total += v;
              found = True
            ],
            {j, Length[assignments]}
          ],
          {i, Length[positions]}
        ];
        If[found, Break[]];
      ];
      total
    ]
  ];
  DistributeDefinitions[n, digits, powers, baseWeight, lowerBound, sumForDigit];
  digitSums = ParallelMap[sumForDigit, digits, Method -> "CoarsestGrained"];
  Total[digitSums]
]

solve[]
