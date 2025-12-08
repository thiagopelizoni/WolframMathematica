(*
  Project Euler Problem 85: Counting Rectangles
  URL: https://projecteuler.net/problem=085

  Problem Statement:
  The number of rectangles contained within a grid of size n x m is given by R(n, m) = T_n * T_m, where T_k = k(k+1)/2
  is the k-th triangular number. We are looking for the grid dimensions (n, m) such that R(n, m) is as close as
  possible to 2,000,000. The output should be the area (n * m) of this optimal grid.

  Mathematical Analysis:
  The number of rectangles in an n x m grid is the number of ways to choose 2 horizontal lines from n+1 lines and 2
  vertical lines from m+1 lines. Thus, Count(n, m) = Binomial[n+1, 2] * Binomial[m+1, 2] = (n(n+1)/2) * (m(m+1)/2).
  We want to minimize |Count(n, m) - 2,000,000|.
  Due to symmetry, we can assume without loss of generality that n >= m, or simply iterate n and find the optimal m
  for that n. The function Count(n, m) is monotonically increasing with respect to both n and m.
  Upper Bound: For m = 1, Count(n, 1) = n(n+1)/2. Setting this approx 2,000,000 implies n^2 approx 4,000,000, or
  n approx 2,000. This gives a finite search space for n: [1, 2000].
  Optimization: For a fixed n, we can solve for the optimal m algebraically. Let Target_m = 2,000,000 / T_n. We need
  T_m approx Target_m. Solving m(m+1)/2 = Target_m for m gives m approx (Sqrt(1 + 8*Target_m) - 1) / 2. We test the
  integer floor and ceiling of this value to find the m that minimizes the error.

  Computational Complexity:
  The algorithm iterates n from 1 to 2000. Inside the loop, operations are O(1) (arithmetic and simple comparisons).
  Total complexity is O(N_max), where N_max = 2000. This is negligible (~microseconds).

  Parallelization Strategy:
  The search over n is embarrassingly parallel. The range [1, 2000] is distributed across available cores using
  ParallelMap. Each task computes the best {error, area} for a given n. The main process aggregates these results
  by selecting the pair with the minimal error.

  Wolfram Language Implementation:
  - Define a worker function `solveForN` that computes the best m and corresponding error for a given n.
  - Use `ParallelMap` to apply this function over Range[2000].
  - Use `MinimalBy` to find the solution with the smallest absolute difference from the target.
  - Extract the area from the best result.
*)

solve[] := Module[{nCores, target, limit, solveForN, results, bestResult},
  nCores = $ProcessorCount;
  target = 2000000;
  (* Limit derived from n(n+1)/2 ~= 2,000,000 -> n ~= 2000 *)
  limit = 2000;

  (* Worker function: finds best m for a fixed n and returns {error, area} *)
  solveForN = Function[{n},
    Module[{Tn, targetTm, mApprox, mCandidates, bestM, rectCount, error},
      Tn = n * (n + 1) / 2;
      
      (* We want Tn * Tm ~= target => Tm ~= target / Tn *)
      targetTm = target / Tn;
      
      (* Inverse triangular number: m^2 + m - 2*targetTm = 0 => m = (Sqrt[1 + 8*targetTm] - 1)/2 *)
      mApprox = (Sqrt[1 + 8 * targetTm] - 1) / 2;
      
      (* Check the integer neighbors of the approximate solution *)
      mCandidates = DeleteDuplicates[{Floor[mApprox], Ceiling[mApprox]}];
      (* Dimensions must be positive integers *)
      mCandidates = Select[mCandidates, # >= 1 &];
      
      (* Find the m that minimizes the absolute difference *)
      bestM = First[MinimalBy[mCandidates, 
        Function[{m}, Abs[Tn * (m * (m + 1) / 2) - target]]
      ]];
      
      rectCount = Tn * (bestM * (bestM + 1) / 2);
      error = Abs[rectCount - target];
      
      {error, n * bestM}
    ]
  ];

  (* Execute search in parallel *)
  results = ParallelMap[solveForN, Range[limit], Method -> "CoarsestGrained"];

  (* Find the result with the minimum error *)
  bestResult = First[MinimalBy[results, First]];

  (* Return the area of the optimal grid *)
  Last[bestResult]
]

solve[]