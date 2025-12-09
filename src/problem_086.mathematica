(*
  Project Euler Problem 86: Cuboid Route
  URL: https://projecteuler.net/problem=086

  Problem Statement:
  A spider S sits in one corner of a cuboid room of dimensions M x N x P, and a fly F sits in the opposite corner.
  By "unfolding" the room onto a 2D plane, finding the shortest path from S to F becomes a straight line distance
  calculation. There are up to three such candidate paths corresponding to the three ways to unfold the net.
  We are interested in integer cuboids where the shortest path itself has an integer length.
  For a given maximum dimension M (where dimensions are 1 <= a <= b <= c <= M), let count(M) be the number of distinct
  cuboids (a,b,c) such that the shortest path is an integer. We seek the least value of M such that count(M) exceeds
  1,000,000.

  Mathematical Analysis:
  Let the dimensions of the cuboid be x, y, z such that 1 <= x <= y <= z = M.
  The shortest path distance D on the surface is given by min(Sqrt[(x+y)^2 + z^2], Sqrt[(x+z)^2 + y^2],
  Sqrt[(y+z)^2 + x^2]).
  Since x <= y <= z, the term (x+y)^2 + z^2 is always the minimum. Let s = x + y. Then D = Sqrt[s^2 + z^2].
  We require D to be an integer, which implies that s^2 + z^2 must be a perfect square. Thus (s, z, D) form a
  Pythagorean triple.
  Here z is the fixed largest dimension M. The sum of the other two dimensions is s = x + y.
  Since 1 <= x <= y <= z, the possible range for s is:
    Minimum s: x=1, y=1 => s=2.
    Maximum s: x=z, y=z => s=2z.
    Constraint x <= y implies x = s - y <= y => s <= 2y => y >= s/2.
    Constraint y <= z implies s - x <= z => s - 1 <= z (since x>=1) is not tight. Actually y <= z implies y <= M.
  For a fixed z (which we will iterate as M), we need to find the number of integers s in [2, 2M] such that
  s^2 + M^2 is a perfect square.
  For each valid s, the number of pairs (x, y) such that x + y = s and 1 <= x <= y <= M is required.
  Given s, the number of pairs (x, y) is the number of integers y such that s/2 <= y <= min(s-1, M).
  Let this count be k.
    If s <= M, then y ranges from ceil(s/2) to s-1. Count = (s-1) - ceil(s/2) + 1 = floor((s-1)/2).
      Actually simpler: x <= y => x <= s-x => 2x <= s => x <= floor(s/2). Also x >= 1.
      So pairs are determined by x: 1 <= x <= floor(s/2).
      But we also need y <= M => s-x <= M => x >= s-M.
      So max(1, s-M) <= x <= floor(s/2).
      Count = floor(s/2) - max(1, s-M) + 1.
      Case 1: s <= M. max(1, s-M) = 1. Count = floor(s/2).
      Case 2: s > M. max(1, s-M) = s-M. Count = floor(s/2) - (s-M) + 1.
      Wait, simpler logic from literature:
      If s <= M, count is floor(s/2).
      If s > M, count is M - ceil(s/2) + 1 = M - floor((s+1)/2) + 1.

  Algorithm and Complexity:
  We are looking for the cumulative count sum_count(M) > 1,000,000.
  Since count(M) = count(M-1) + (number of solutions with z=M), we can simply iterate M starting from 1 and maintain
  a running total.
  For a fixed M, we iterate s from 2 to 2M. We check if M^2 + s^2 is a square. If so, we add the number of (x,y) pairs.
  Checking "is square" is O(1). M will go up to around 2000 (heuristic from problem difficulty).
  Complexity: Sum_{M=1 to K} M = O(K^2). With K ~ 2000-3000, operations ~ 10^7, which is trivial.

  Parallelization Strategy:
  While the search for M is inherently sequential (we stop as soon as total > 10^6), the calculation for a specific M
  can be parallelized. However, since the total count is cumulative, we can check ranges of M in chunks or just run
  sequentially because O(K^2) is very fast.
  Given the constraint to use parallelism: We can define a function `countForM[z]` that calculates the solutions added
  by setting the largest dimension to z. Then we can sum these in blocks or accumulate them.
  Since we don't know M precisely, we can iterate M in steps (e.g., checks of 100) or just run a parallel computation
  to build a table of counts up to an upper bound (e.g., M=3000) and then scan for the threshold.
  Heuristic check: M=100 -> count=100. M=1000 -> count=~70k? The target is 1M.
  We will calculate counts for M in [1, 3000] in parallel, accumulate them, and finding the first crossing.

  Wolfram Language Implementation:
  - Define `solutionsForZ[z]` which iterates s from 2 to 2z.
  - Use `IntegerQ[Sqrt[s^2 + z^2]]` to check for integer path.
  - Add contributions based on s <= z or s > z.
  - Use `ParallelTable` to compute this for z up to a safe upper bound (e.g., 2000).
  - Use `Accumulate` on the list.
  - Find the first index exceeding 1,000,000.
  - If 2000 isn't enough, we might need to go higher. Previous similar problems suggest M around 1800. Let's use 3000.
*)

solve[] := Module[{nCores, upperBound, countForZ, counts, cumulativeCounts, resultM},
  nCores = $ProcessorCount;
  upperBound = 2000; (* Heuristic bound; standard solution is around 1818 *)

  (* Function to calculate number of integer shortest paths where largest dimension is z *)
  countForZ = Function[{z},
    Module[{count = 0, root},
      (* Iterate s = x + y. Range is 2 to 2z. *)
      Do[
        root = Sqrt[z^2 + s^2];
        If[IntegerQ[root],
          (* If valid s, add number of pairs (x,y) *)
          If[s <= z,
            count += Floor[s/2],
            count += (z - Ceiling[s/2] + 1)
          ]
        ],
        {s, 2, 2 * z}
      ];
      count
    ]
  ];

  (* Compute counts for each z from 1 to upperBound in parallel *)
  counts = ParallelTable[
    countForZ[z],
    {z, 1, upperBound},
    Method -> "CoarsestGrained"
  ];

  (* Calculate cumulative sum *)
  cumulativeCounts = Accumulate[counts];

  (* Find the first M where the count exceeds 1,000,000 *)
  resultM = FirstPosition[cumulativeCounts, x_ /; x > 1000000];

  (* FirstPosition returns {index}, and since z starts at 1, index is exactly M *)
  If[MissingQ[resultM],
    "UpperBound insufficient",
    resultM[[1]]
  ]
]

solve[]