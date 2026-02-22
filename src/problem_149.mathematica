(* Project Euler 149: https://projecteuler.net/problem=149

  The instance defines a deterministic pseudo-random sequence s_k, fills a 2000 by 2000 grid row-wise, and asks for the
  largest sum obtainable from a contiguous run of entries along one of four lattice directions: horizontal, vertical,
  main diagonal, or anti-diagonal. The run length is unconstrained except for contiguity and boundary limits.

  Let A be the grid. For any fixed line L in the allowed directions, the objective on L is the classical maximum
  contiguous subsequence sum. Writing prefix sums p_0 = 0 and p_j = Sum[L_i, {i, 1, j}], the best segment ending at j is
  p_j - min_{0 <= t < j} p_t, so each line is solvable in linear time by Kadane's recurrence, equivalent to this prefix
  minimum formulation. Therefore the global optimum is simply the maximum over all admissible lines.

  The combinatorial reduction is exact: every feasible segment belongs to exactly one extracted line in one direction,
  and every contiguous segment of each extracted line is feasible in the grid. No geometric case is missed because the
  four directions are represented by rows, columns, and diagonals of A and of row-reversed A.

  Sequence generation costs Theta(N) with N = 2000^2 terms, since the lagged recurrence depends on previous values and is
  inherently sequential. The search phase scans each grid entry a constant number of times across all direction families,
  also Theta(N). Memory remains Theta(N) for the packed integer grid. With N = 4*10^6, this is computationally practical.

  Parallelism is introduced where independence is exact: once A is built, line evaluations are embarrassingly parallel.
  We partition rows, columns, and diagonal-offset sets into disjoint chunks sized from $ProcessorCount, assign chunks to
  kernels with ParallelMap, compute local maxima, and combine via Max, an associative and commutative reduction.

  In Wolfram Language, the generator is implemented with exact integer modular arithmetic and packed arrays. The solver
  defines a linear-time max-subarray kernel, chunking utilities, parallel evaluators for line families, and a final
  aggregator returning one exact integer answer with deterministic behavior and no external state dependencies. *)

nCores = $ProcessorCount;

ClearAll[
  generateSequence,
  maxSubarray,
  splitList,
  chunkLineMax,
  parallelLineMax,
  chunkDiagonalMax,
  parallelDiagonalMax,
  solve
];

generateSequence[n_Integer] := Module[
  {t},
  t = ConstantArray[0, n];
  Do[
    t[[k]] = Mod[100003 - 200003 k + 300007 k^3, 1000000],
    {k, 1, 55}
  ];
  Do[
    t[[k]] = Mod[t[[k - 24]] + t[[k - 55]], 1000000],
    {k, 56, n}
  ];
  Developer`ToPackedArray[t - 500000]
];

maxSubarray[v_List] := Module[
  {best, cur, x},
  best = v[[1]];
  cur = best;
  Do[
    x = v[[i]];
    cur = Max[x, cur + x];
    best = Max[best, cur],
    {i, 2, Length[v]}
  ];
  best
];

splitList[list_List, parts_Integer] := Module[
  {chunkSize},
  chunkSize = Max[1, Ceiling[Length[list]/parts]];
  Table[
    list[[i ;; Min[i + chunkSize - 1, Length[list]]]],
    {i, 1, Length[list], chunkSize}
  ]
];

chunkLineMax[lines_List] := Max[Map[maxSubarray, lines]];

parallelLineMax[lines_List, cores_Integer] := Module[
  {chunks, localMaxima},
  chunks = splitList[lines, cores];
  localMaxima = ParallelMap[chunkLineMax, chunks, Method -> "CoarsestGrained"];
  Max[localMaxima]
];

chunkDiagonalMax[matrix_List, offsets_List] := Module[
  {best},
  best = -Infinity;
  Do[
    best = Max[best, maxSubarray[Diagonal[matrix, off]]],
    {off, offsets}
  ];
  best
];

parallelDiagonalMax[matrix_List, offsets_List, cores_Integer] := Module[
  {chunks, localMaxima},
  chunks = splitList[offsets, cores];
  localMaxima = ParallelMap[
    chunkDiagonalMax[matrix, #] &,
    chunks,
    Method -> "CoarsestGrained"
  ];
  Max[localMaxima]
];

solve[] := Module[
  {side, total, matrix, columns, reversed, offsets, maxRows, maxCols, maxDiagMain, maxDiagAnti},
  side = 2000;
  total = side^2;
  If[$KernelCount < nCores, LaunchKernels[nCores - $KernelCount]];
  matrix = Partition[generateSequence[total], side];
  columns = Transpose[matrix];
  reversed = Reverse[matrix, 2];
  offsets = Range[-(side - 1), side - 1];
  DistributeDefinitions[
    maxSubarray,
    splitList,
    chunkLineMax,
    parallelLineMax,
    chunkDiagonalMax,
    parallelDiagonalMax
  ];
  maxRows = parallelLineMax[matrix, nCores];
  maxCols = parallelLineMax[columns, nCores];
  maxDiagMain = parallelDiagonalMax[matrix, offsets, nCores];
  maxDiagAnti = parallelDiagonalMax[reversed, offsets, nCores];
  Max[maxRows, maxCols, maxDiagMain, maxDiagAnti]
];

solve[]
