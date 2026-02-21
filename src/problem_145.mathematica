(* Project Euler 145: https://projecteuler.net/problem=145

  We must count positive integers n < 10^9 such that n does not end in zero and every decimal digit of
  n + reverse(n) is odd. Reversibility is therefore a carry-constrained digit-pair problem, not a brute-force scan.

  Write n with d digits as sum_{i=0}^{d-1} a_i 10^i, with a_0 != 0 and a_{d-1} != 0. In the addition
  n + reverse(n), the i-th output digit depends on a_i + a_{d-1-i} and carry c_i. Because mirrored positions share
  the same pair sum, carries are forced into a finite-state pattern. The resulting automaton yields closed counts:
  for even d = 2k, the count is 20 * 30^(k - 1); for odd d, lengths d congruent to 1 mod 4 contribute zero, while
  lengths d congruent to 3 mod 4 contribute 100 * 500^((d - 3)/4). These identities are standard for this problem
  and follow from parity and carry compatibility at mirrored and central positions.

  Hence the total below 10^9 is the sum over d = 1..9 of a closed formula per length. Complexity is O(log_10 N):
  only nine lengths are evaluated, each in constant time, with exact integer arithmetic and O(1) memory.

  Parallelization is naturally done by digit length. Each length d is an independent subproblem, mapped to kernels
  with ParallelMap under dynamic scheduling. Aggregation is a pure associative Total over exact integers, so there is
  no shared mutable state and no race condition. Determinism follows immediately from pure functions and exact math.

  The Wolfram Language script therefore implements one length-count function, distributes it to all kernels, computes
  counts in parallel for d = 1..maxDigits, and returns their exact sum. *)

nCores = $ProcessorCount;

ClearAll[countByDigits, solve];

countByDigits[d_Integer] := Which[
  d == 1, 0,
  EvenQ[d], 20 30^(Quotient[d, 2] - 1),
  Mod[d, 4] == 1, 0,
  True, 100 500^Quotient[d - 3, 4]
];

solve[] := Module[
  {limit, maxDigits, digits, counts},
  limit = 10^9;
  If[$KernelCount < nCores, LaunchKernels[nCores - $KernelCount]];
  maxDigits = IntegerLength[limit - 1];
  digits = Range[maxDigits];
  DistributeDefinitions[countByDigits];
  counts = ParallelMap[countByDigits, digits, Method -> "FinestGrained"];
  Total[counts]
];

solve[]
