(* Project Euler 150: https://projecteuler.net/problem=150

  A triangular array with 1000 rows is filled by a deterministic congruential sequence derived modulo 2^20, then shifted
  to signed integers. The task is to find the minimum sum among all downward contiguous sub-triangles: choose an apex
  (r,c), choose a height h, and sum entries on rows r..r+h-1 with segment widths growing by one each row.

  Write a_{r,c} for row r, column c, and define row prefixes P_{r,0}=0 and P_{r,j}=Sum_{m=1..j} a_{r,m}. Any row segment
  from c to c+d has exact weight P_{r,c+d}-P_{r,c-1}. Hence for fixed apex (r,c), extending depth d updates the triangle
  sum by one O(1) segment query on row r+d. Enumerating all apexes and all depths is therefore an exact Theta(n^3)
  procedure once prefixes are available.

  The operation count is Sum_{r=1..n} r(n-r+1)=n(n+1)(n+2)/6. For n=1000 this is 167,167,000 incremental updates, which is
  computationally feasible with packed machine integers and compiled inner loops. Memory is Theta(n^2): 500,500 generated
  values plus a padded prefix matrix of dimensions n by n+1.

  Parallelization is mathematically clean after sequence construction. For each start row r, all triangles with apex row r
  are independent from other start rows; each kernel computes one local minimum over its assigned rows. Work is scheduled
  dynamically with fine granularity to balance the nonuniform row costs, and partial results are aggregated by Min, an
  associative and commutative reduction, so determinism and race freedom are preserved.

  The Wolfram Language implementation uses exact integer modular arithmetic, TakeList to build triangular rows, Accumulate
  with zero-prepended padded prefixes for O(1) segment extraction, a compiled row-kernel for the cubic scan, and ParallelMap
  over start rows based on $ProcessorCount to exploit all available CPU cores. *)

nCores = $ProcessorCount;

ClearAll[
  generateSequence,
  buildPrefixMatrix,
  minForStart,
  solve
];

generateSequence[count_Integer] := Module[
  {values, t},
  values = ConstantArray[0, count];
  t = 0;
  Do[
    t = Mod[615949 t + 797807, 1048576];
    values[[k]] = t - 524288,
    {k, 1, count}
  ];
  Developer`ToPackedArray[values]
];

buildPrefixMatrix[values_List, nRows_Integer] := Module[
  {rows},
  rows = TakeList[values, Range[nRows]];
  Developer`ToPackedArray[
    PadRight[Prepend[Accumulate[#], 0], nRows + 1, 0] & /@ rows
  ]
];

minForStart = Compile[
  {{prefixMatrix, _Integer, 2}, {start, _Integer}, {nRows, _Integer}},
  Module[
    {best, c, d, sum, rr, seg},
    best = 3000000000000;
    For[c = 1, c <= start, c++,
      sum = 0;
      For[d = 0, d <= nRows - start, d++,
        rr = start + d;
        seg = prefixMatrix[[rr, c + d + 1]] - prefixMatrix[[rr, c]];
        sum = sum + seg;
        If[sum < best, best = sum];
      ];
    ];
    best
  ],
  RuntimeOptions -> "Speed"
];

solve[] := Module[
  {nRows, count, values, prefixMatrix, starts, minima},
  nRows = 1000;
  count = Quotient[nRows (nRows + 1), 2];
  If[$KernelCount < nCores, LaunchKernels[nCores - $KernelCount]];
  values = generateSequence[count];
  prefixMatrix = buildPrefixMatrix[values, nRows];
  starts = Range[nRows];
  DistributeDefinitions[minForStart, prefixMatrix, nRows];
  minima = ParallelMap[
    minForStart[prefixMatrix, #, nRows] &,
    starts,
    Method -> "FinestGrained"
  ];
  Min[minima]
];

solve[]
