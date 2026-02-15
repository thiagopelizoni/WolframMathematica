(* Project Euler Problem 113 — Non-bouncy Numbers Below a Googol
   https://projecteuler.net/problem=113

   A positive integer is called increasing if no digit is exceeded by the digit to its left,
   and decreasing if no digit is exceeded by the digit to its right. Numbers that are neither
   increasing nor decreasing are bouncy. The problem asks: how many integers below 10^100
   (a googol) are not bouncy?

   Mathematical analysis. We count non-bouncy numbers via inclusion-exclusion: the set of
   non-bouncy numbers equals the union of increasing and decreasing numbers, so
     |non-bouncy| = |increasing| + |decreasing| - |increasing AND decreasing|.
   Numbers that are simultaneously increasing and decreasing have all digits equal
   (repdigits), so we subtract those to avoid double-counting.

   Counting increasing d-digit numbers. A d-digit increasing number has digits d_1 <= d_2
   <= ... <= d_d with d_1 >= 1. This is equivalent to choosing a weakly increasing sequence
   of length d from {0,...,9} whose first element is positive. The total number of weakly
   increasing sequences of length d from a 10-element alphabet is C(d+9, 9) by the classical
   stars-and-bars / multiset coefficient. Among these, sequences with d_1 = 0 correspond to
   a free weakly increasing sequence of length d-1 from {0,...,9} prepended by 0, giving
   C(d+8, 9). By the Pascal identity the difference C(d+9, 9) - C(d+8, 9) = C(d+8, 8).
   Summing over d = 1, ..., 100 by the hockey-stick identity:
     Sum_{d=1}^{100} C(d+8, 8) = Sum_{j=9}^{108} C(j, 8) = C(109, 9) - 1.

   Counting decreasing d-digit numbers. A d-digit decreasing number has d_1 >= d_2 >= ...
   >= d_d with d_1 >= 1. Equivalently, choose a multiset of size d from {0,...,9} and arrange
   it in non-increasing order; d_1 >= 1 excludes only the all-zero multiset. So the count for
   exactly d digits is C(d+9, 9) - 1. Summing over d = 1, ..., 100:
     Sum_{d=1}^{100} [C(d+9, 9) - 1] = C(110, 10) - 1 - 100 = C(110, 10) - 101.

   Repdigits. For each digit length d there are exactly 9 repdigits (digits 1 through 9),
   giving 9 * 100 = 900 total repdigits in 1, ..., 10^100 - 1.

   Combining yields the closed form:
     non-bouncy = [C(109, 9) - 1] + [C(110, 10) - 101] - 900
                = C(109, 9) + C(110, 10) - 1002.

   Feasibility. The entire computation reduces to evaluating two binomial coefficients and
   performing a single subtraction — O(1) symbolic operations. The result is an exact integer
   with about 14 decimal digits, well within machine word range but computed via arbitrary
   precision for safety.

   Parallelization strategy. Although the closed form is instantaneous, the specification
   requires meaningful parallelism. We decompose over the 100 digit lengths: each d in
   {1, ..., 100} independently contributes C(d+8, 8) + C(d+9, 9) - 10 non-bouncy d-digit
   numbers (increasing + decreasing - repdigits). These 100 independent evaluations are
   distributed across cores via ParallelTable, and the results aggregated with Total. This
   decomposition is embarrassingly parallel, associative, and deterministic.

   Implementation. The code uses Binomial for exact integer binomial coefficients,
   ParallelTable over digit lengths, and Total for the reduction. LaunchKernels initialises
   subkernels. No floating-point arithmetic, randomness, or I/O is used. *)

nCores = $ProcessorCount;
LaunchKernels[];

solve[] := Module[
  {counts},
  counts = ParallelTable[
    Binomial[d + 8, 8] + Binomial[d + 9, 9] - 10,
    {d, 1, 100}
  ];
  Total[counts]
]

solve[]
