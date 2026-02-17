(* Project Euler Problem 122 — https://projecteuler.net/problem=122

   For each integer k, let m(k) be the minimum number of multiplications needed to compute n^k from n, where every
   multiplication may combine any two previously computed powers. Equivalently, m(k) is the length of a shortest
   addition chain for k. The task is to evaluate Sum_{k=1}^{200} m(k).

   Exponentiation by repeated products maps exactly to addition chains: starting from exponent 1, each multiplication
   n^a n^b = n^(a+b) appends a+b to the chain, and every chain yields a valid multiplication schedule. Therefore the
   optimization problem is purely combinatorial on exponent sets. We must compute exact shortest lengths, not merely
   star-chain lengths, so transitions include all pair sums of prior exponents.

   The solver uses iterative deepening depth-first search per target k. For fixed depth d, we test existence of an
   increasing chain 1 = a0 < a1 < ... < at = k with t <= d and ai = aj + al for some j,l < i. Two admissible bounds
   guarantee correctness while pruning aggressively: if current last exponent L satisfies L*2^r < k with r steps left,
   k is unreachable even under maximal doubling; similarly a candidate c is discarded when c*2^(r-1) < k. Since the
   depth loop starts at IntegerLength[k-1, 2] (exactly Ceiling(Log[2, k])) and stops at a constructive binary upper
   bound IntegerLength[k, 2] + popcount(k) - 2, the first feasible depth is exactly m(k).

   Worst-case addition-chain search is exponential in depth, but here k <= 200 and optimal depths are small
   (max m(k) = 11), so bounded iterative deepening with admissible pruning is easily feasible. The total workload is
   the sum of 200 independent exact searches, each over a heavily reduced state tree; integer arithmetic is exact and
   small, so memory pressure stays low.

   Parallelization is embarrassingly parallel over targets k. We detect nCores = $ProcessorCount, split 1..200 into
   disjoint chunks, evaluate chunk subtotals in parallel kernels, and combine by Total. Because addition is associative
   and there is no shared mutable state, aggregation is deterministic and race-free.

   In Wolfram Language, helper functions implement the binary upper bound, fixed-depth existence test, and minimal
   depth search. Parallel definitions are distributed once, chunk subtotals are computed with ParallelMap, and solve[]
   returns the exact Euler value as a single integer output.
*)

nCores = $ProcessorCount;
LaunchKernels[];

binaryUpperBound[k_Integer?Positive] := Module[{bits},
  bits = IntegerDigits[k, 2];
  IntegerLength[k, 2] + Total[bits] - 2
]

chainExistsAtDepth[target_Integer?Positive, depth_Integer?NonNegative] := Module[{dfs},
  dfs[path_List, remaining_Integer?NonNegative] := Module[{last, len, raw, candidates, found, c},
    last = path[[-1]];
    If[last == target,
      True,
      If[remaining == 0,
        False,
        If[last*2^remaining < target,
          False,
          len = Length[path];
          raw = Flatten[Table[path[[i]] + path[[j]], {i, len, 1, -1}, {j, i, 1, -1}]];
          candidates = Select[DeleteDuplicates[raw], last < # <= target && #*2^(remaining - 1) >= target &];
          candidates = Sort[candidates, Greater];
          found = False;
          Do[
            c = candidates[[idx]];
            If[TrueQ[dfs[Append[path, c], remaining - 1]],
              found = True;
              Break[]
            ],
            {idx, 1, Length[candidates]}
          ];
          found
        ]
      ]
    ]
  ];
  TrueQ[dfs[{1}, depth]]
]

minChainLength[target_Integer?Positive] := Module[{lower, upper, depth},
  If[target == 1,
    0,
    lower = IntegerLength[target - 1, 2];
    upper = binaryUpperBound[target];
    depth = lower;
    While[depth <= upper && !TrueQ[chainExistsAtDepth[target, depth]],
      depth++
    ];
    depth
  ]
]

DistributeDefinitions[binaryUpperBound, chainExistsAtDepth, minChainLength];

solve[] := Module[{limit = 200, chunkSize, chunks, partialSums},
  chunkSize = Ceiling[limit/nCores];
  chunks = Table[Range[i, Min[i + chunkSize - 1, limit]], {i, 1, limit, chunkSize}];
  partialSums = ParallelMap[Total[minChainLength /@ #] &, chunks];
  Total[partialSums]
]

solve[]
