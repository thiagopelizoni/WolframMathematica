(*
  Project Euler Problem 76: Counting Summations
  URL: https://projecteuler.net/problem=076

  Problem Statement:
  Determine the number of different ways the integer 100 can be written as a sum of at least two positive integers.
  This is equivalent to finding p(100) - 1, where p(n) is the unrestricted partition function of n (subtracting the
  case where the sum consists of the single number 100).

  Mathematical Analysis:
  The partition function p(n) is the coefficient of x^n in the generating function P(x) = Product[(1 - x^k)^(-1)]
  for k = 1 to infinity. To count partitions of 100 into at least two parts, we restrict the parts to be strictly less
  than 100. Thus, we seek the coefficient of x^100 in the product G(x) = Product[(1 - x^k)^(-1)] for k = 1 to 99.
  This restriction naturally excludes the partition {100}, leaving exactly the sums with 2 or more terms.
  
  While Euler's pentagonal number recurrence provides an O(n^1.5) sequential algorithm, we implement a divide-and-
  conquer polynomial multiplication strategy. We treat the partial products as polynomials truncated modulo x^101.
  Multiplying polynomials is an associative operation, allowing us to use a binary tree reduction. This exposes O(n)
  independent multiplication tasks at the leaves, making the algorithm suitable for parallel execution, unlike the
  inherently serial recurrence relation. The complexity is roughly O(n^2 log n) or O(n^2) depending on multiplication
  depth, which is computationally trivial for n=100 but demonstrates the correct parallel architecture.

  Parallelization Strategy:
  The problem decomposes into the multiplication of 99 polynomials.
  1. We initialize a list of coefficient vectors for (1 - x^k)^(-1) mod x^101.
  2. We employ an iterative reduction: in each step, the list of polynomials is partitioned into pairs.
  3. These pairs are multiplied concurrently using `ParallelMap` distributed across all available cores.
  4. The process repeats until a single polynomial remains. The answer is the coefficient of the x^100 term.

  Wolfram Language Implementation:
  - We use `ListConvolve` with padding for polynomial multiplication.
  - We use `ParallelMap` with "CoarsestGrained" method to minimize overhead during the reduction steps.
  - Data structures are flat lists of integers representing coefficients.
*)

solve[] := Module[{nCores, target, polys, polyMul, pairs, leftovers, finalPoly},
  nCores = $ProcessorCount;
  target = 100;

  polyMul = Function[{p1, p2},
    Take[ListConvolve[p1, p2, {1, 1}, 0], UpTo[target + 1]]
  ];

  polys = Table[
    Table[If[Divisible[i, k], 1, 0], {i, 0, target}],
    {k, 1, target - 1}
  ];

  While[Length[polys] > 1,
    pairs = Partition[polys, 2];
    
    leftovers = If[OddQ[Length[polys]], {Last[polys]}, {}];
    
    polys = Join[
      ParallelMap[polyMul @@ # &, pairs, Method -> "CoarsestGrained"],
      leftovers
    ];
  ];

  finalPoly = First[polys];

  finalPoly[[target + 1]]
]

solve[]