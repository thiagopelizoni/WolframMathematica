(*
Project Euler Problem 63: Powerful Digit Counts
URL: https://projecteuler.net/problem=063

Problem Statement:
Determine the total number of n-digit positive integers that are also nth powers. Formally, we seek the cardinality of
the set of pairs (b, n) of positive integers such that the decimal representation of b^n consists of exactly n digits.

Mathematical Analysis:
An integer x = b^n has exactly n decimal digits if and only if it satisfies the inequality 10^(n-1) <= b^n < 10^n.
Taking the n-th root of the inequality, we obtain 10^(1 - 1/n) <= b < 10. The strict upper bound b < 10 implies that
the base b must be an integer in the set {1, 2, ..., 9}. For any such fixed base b, the lower bound 10^(n-1) <= b^n
dictates the range of valid exponents. Taking logarithms base 10, we get n - 1 <= n * log10(b), which rearranges to
n * (1 - log10(b)) <= 1, or n <= 1 / (1 - log10(b)). Since log10(b) < 1 for all b in {1, ..., 9}, the denominator is
positive, establishing a finite upper bound for n for each base. The problem reduces to finding the number of integer
solutions for n for each base b and summing these counts. Since b^n < 10^n is trivially true for b < 10, we strictly
need to verify b^n >= 10^(n-1), which is equivalent to checking if IntegerLength[b^n] == n.

Feasibility and Complexity:
The search space is extremely small. The maximum exponent n occurs for b=9, where n <= 1/(1-log10(9)) approx 21.85.
Thus, the total number of pairs to check is less than 9 * 22 = 198. The algorithm runs in effectively O(1) time
relative to modern computing power. The space complexity is minimal. We use exact integer arithmetic to avoid any
potential floating-point precision issues near the boundary conditions.

Parallelization Strategy:
The problem decomposes naturally into independent tasks for each base b. We distribute the set of bases {1, ..., 9}
across all available processor cores using `ParallelMap`. Each core independently computes the count of valid exponents
for its assigned bases. The partial counts are then aggregated using `Total`. This approach ensures full utilization of
the available hardware, satisfying the requirement for dynamic parallelism.

Implementation Strategy:
A helper function `countValidExponents[b]` is defined to iterate n starting from 1, checking the condition
`IntegerLength[b^n] == n`. The loop terminates as soon as the condition fails (since n grows faster than the digits of
b^n for b < 10). The main `solve[]` function orchestrates the parallel mapping of this helper over `Range[9]` and
returns the sum of the results.
*)

nCores = $ProcessorCount;

countValidExponents[b_Integer] := Module[{n = 1},
  While[IntegerLength[b^n] == n,
    n++
  ];
  n - 1
];

solve[] := Total[ParallelMap[countValidExponents, Range[9]]];

solve[]