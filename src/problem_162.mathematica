(* Project Euler 162: https://projecteuler.net/problem=162

We count hexadecimal integers with at most sixteen digits, no leading zero, and with digits 0, 1, and A each occurring at least once.
The output must be the count written in hexadecimal notation.

For a fixed length n, let U be the set of 16 hex symbols and R={0,1,A}. We need the number of words of length n over U with first symbol
in U\{0} and with every element of R present. Inclusion-exclusion on missing required symbols is exact and minimal here. If S is a subset
of R interpreted as the set of required symbols that are forbidden, then the usable alphabet has size 16-|S|, while the first position has
(16-|S|)-1 choices when 0 is still usable, and 16-|S| choices when 0 is forbidden already. Hence
N(n)=Sum_{S subseteq R} (-1)^|S| * firstChoices(S) * (16-|S|)^(n-1).

The target quantity is Sum_{n=1}^{16} N(n). Complexity is O(L*2^|R|) with L=16 and |R|=3, so only 128 elementary integer terms are
needed. This is asymptotically constant for the Project Euler bound and therefore easily feasible with exact arithmetic.

Parallelism is over lengths n: each N(n) is independent, so work is embarrassingly parallel and can be split across kernels with no shared
state. Partial counts are aggregated by Total, an associative deterministic reduction, guaranteeing reproducibility and race-free behavior.
If parallel evaluation is unavailable at runtime, the same deterministic computation falls back to serial evaluation.

The Wolfram Language implementation uses Subsets for inclusion-exclusion classes, exact integer powers for counting terms, and a single
solve[] function that returns the uppercase hexadecimal string via IntegerString[...,16]. All parameters are internal and fixed. *)

nCores = $ProcessorCount;

ClearAll[
  countForLength,
  solve
];

countForLength[n_Integer?Positive, requiredSubsets_List] := Module[
  {subset, excluded, alphabetSize, firstChoices},
  Total[
    Map[
      (
        subset = #;
        excluded = Length[subset];
        alphabetSize = 16 - excluded;
        firstChoices = alphabetSize - If[MemberQ[subset, 0], 0, 1];
        (-1)^excluded*firstChoices*alphabetSize^(n - 1)
      ) &,
      requiredSubsets
    ]
  ]
];

solve[] := Module[
  {
    maxLen = 16,
    requiredDigits = {0, 1, 10},
    requiredSubsets,
    workers,
    lengthCounts,
    answer
  },
  requiredSubsets = Subsets[requiredDigits];
  workers = Max[1, nCores - 1];
  lengthCounts = If[
    workers > 1,
    Block[
      {$Messages = {}},
      Check[
        ParallelTable[
          countForLength[n, requiredSubsets],
          {n, 1, maxLen},
          Method -> "CoarsestGrained"
        ],
        Table[
          countForLength[n, requiredSubsets],
          {n, 1, maxLen}
        ]
      ]
    ],
    Table[
      countForLength[n, requiredSubsets],
      {n, 1, maxLen}
    ]
  ];
  answer = Total[lengthCounts];
  ToUpperCase[IntegerString[answer, 16]]
];

solve[]
