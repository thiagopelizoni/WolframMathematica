(*
  Project Euler Problem 96: Su Doku
  URL: https://projecteuler.net/problem=096

  Problem Statement:
  We are provided with a text file containing 50 distinct 9x9 Sudoku puzzles. The objective is to solve all 50 puzzles.
  Once solved, we must identify the 3-digit number formed by the digits in the top-left corner (row 1, columns 1, 2,
  and 3) of each grid. The final answer is the sum of these 50 numbers.

  Mathematical Analysis:
  A 9x9 Sudoku grid is a constraint satisfaction problem. While the general n^2 x n^2 case is NP-complete, the standard
  9x9 size is small enough to be solved virtually instantly using a Depth-First Search (DFS) backtracking algorithm
  optimized with heuristics.
  The most effective heuristic for Sudoku is the "Minimum Remaining Values" (MRV) strategy. At each step, rather than
  picking the first empty cell, we select the cell with the fewest valid candidate digits. This dramatically prunes the
  search tree by forcing early contradictions.
  Algorithm Steps:
  1. Find the cell with the minimum number of valid candidates.
  2. If the cell has 0 candidates, backtrack (dead end).
  3. If no empty cells remain, the puzzle is solved.
  4. Otherwise, tentatively assign each candidate and recurse.
  
  Since the input size is fixed (50 puzzles) and 9x9 Sudoku is bounded, the complexity is well within limits for exact
  solving.

  Parallelization Strategy:
  The 50 puzzles are independent instances. We decompose the problem by parsing the input file into a list of 50
  matrices. We explicitly launch parallel kernels and distribute the solver definitions to ensure stability in the
  script environment. We then use `ParallelMap` to apply the solver to each matrix concurrently. This prevents
  timeouts associated with auto-launching kernels during the computation phase.

  Wolfram Language Implementation:
  - Define core logic (`getCandidates`, `findBestCell`, `backtrack`) in the global scope to facilitate distribution.
  - Use `Import` to fetch the data.
  - Use `LaunchKernels` and `DistributeDefinitions` explicitly to stabilize parallel execution and avoid link errors.
  - Use `ParallelMap` with `Method -> "CoarsestGrained"` for efficient load balancing.
  - Extract the specific digits using `Part` and sum them using `Total`.
*)

(* --- Global Logic Definitions --- *)

getCandidates[board_, r_, c_] := Module[{row, col, blockRow, blockCol, block, used},
  row = board[[r]];
  col = board[[All, c]];
  blockRow = 3 * Quotient[r - 1, 3] + 1;
  blockCol = 3 * Quotient[c - 1, 3] + 1;
  block = Flatten[board[[blockRow ;; blockRow + 2, blockCol ;; blockCol + 2]]];
  used = Union[row, col, block];
  Complement[{1, 2, 3, 4, 5, 6, 7, 8, 9}, used]
];

findBestCell[board_] := Module[{emptyPos, bestPos = Null, minLen = 10, cands},
  emptyPos = Position[board, 0, {2}];
  
  If[Length[emptyPos] == 0, Return[Null]];

  Do[
    cands = getCandidates[board, pos[[1]], pos[[2]]];
    If[Length[cands] < minLen,
      minLen = Length[cands];
      bestPos = pos;
      If[minLen <= 1, Break[]]; 
    ];
  , {pos, emptyPos}];

  If[minLen == 0, Return[$Failed]];
  
  {bestPos, getCandidates[board, bestPos[[1]], bestPos[[2]]]}
];

backtrack[board_] := Module[{target, pos, cands, r, c},
  target = findBestCell[board];

  If[target === Null, Throw[board]];
  
  If[target === $Failed, Return[$Failed]];

  {pos, cands} = target;
  {r, c} = pos;

  Do[
    backtrack[ReplacePart[board, {r, c} -> val]];
  , {val, cands}];
  
  $Failed
];

solveOne[grid_] := Catch[backtrack[grid]];

solve[] := Module[{rawData, puzzles, solvedGrids},
  
  LaunchKernels[];
  
  DistributeDefinitions[getCandidates, findBestCell, backtrack, solveOne];

  rawData = Import["https://projecteuler.net/project/resources/p096_sudoku.txt", "Lines"];
  puzzles = Map[
    Function[chunk, Map[ToExpression[Characters[#]] &, Rest[chunk]]],
    Partition[rawData, 10]
  ];

  solvedGrids = ParallelMap[solveOne, puzzles, Method -> "CoarsestGrained"];

  Total[
    Map[
      FromDigits[#[[1, 1 ;; 3]]] &, 
      solvedGrids
    ]
  ]
];

solve[]