(* Project Euler Problem 119 — https://projecteuler.net/problem=119
   
   Digit Power Sum
   
   Define the sequence {a_n} of integers >= 10 that equal the sum
   of their own digits raised to some positive integer power. The
   sequence is sorted in ascending order. Given a_2 = 512 (= 8^3)
   and a_10 = 614656 (= 28^4), find a_30.
   
   Mathematical analysis:
   Rather than iterating over all integers and testing whether n
   equals (digitSum(n))^k for some k, we invert the problem:
   enumerate pairs (b, k) with b >= 2, k >= 2, compute n = b^k,
   and check whether digitSum(n) = b. This is efficient because
   the digit sum of an m-digit number is at most 9m, so for a
   given b, only exponents k with b^k having digit sum <= 9 *
   ceil(k * log10(b) + 1) matter, and the search space is sparse.
   
   An upper bound on the 30th term can be estimated empirically
   to lie well below 10^18. For b up to 150 (exceeding the
   maximum possible digit sum for numbers with up to 18 digits)
   and k up to floor(60 / log2(b)), the total number of candidate
   pairs is a few thousand. Each requires one exact power and one
   digit-sum computation, both trivial.
   
   Parallelization strategy:
   The candidate generation for distinct bases b is embarrassingly
   parallel: for each b in 2..bMax, we independently compute all
   qualifying powers b^k and filter those whose digit sum equals b.
   These independent per-base computations are distributed across
   cores via ParallelMap. The resulting lists are joined, sorted,
   and the 30th element is extracted.
   
   Implementation plan:
   Detect cores and launch kernels. For each base b from 2 to an
   upper bound, generate all powers b^k (k >= 2) below a ceiling
   10^20, filter those with digit sum equal to b and value >= 10.
   Distribute the per-base function and ParallelMap over the base
   range. Join, Union (to deduplicate), Sort, and return the 30th
   element.
*)

nCores = $ProcessorCount;
LaunchKernels[];

candidatesForBase[b_, ceil_] := Module[{results = {}, k = 2, n},
  n = b^2;
  While[n <= ceil,
    If[n >= 10 && Total[IntegerDigits[n]] == b,
      AppendTo[results, n]
    ];
    k++;
    n = b^k
  ];
  results
]

DistributeDefinitions[candidatesForBase];

solve[] := Module[{bMax = 200, ceil = 10^20, all},
  all = Join @@ ParallelMap[candidatesForBase[#, ceil] &, Range[2, bMax]];
  all = Union[all];
  all[[30]]
]

solve[]
