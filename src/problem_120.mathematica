(* Project Euler Problem 120 — https://projecteuler.net/problem=120
   
   Square Remainders
   
   Let r be the remainder when (a-1)^n + (a+1)^n is divided by a^2.
   For each integer a with 3 <= a <= 1000, let r_max be the maximum
   of r over all positive integers n. Find the sum of all r_max.
   
   Mathematical analysis:
   Expand both powers by the binomial theorem and add. Terms with
   even powers of a survive; modulo a^2 only the constant and a^1
   terms remain. For even n, every surviving k is even, and the
   only contribution mod a^2 is the k=0 term, giving r = 2. For
   odd n, the surviving k are odd, and the sole contribution mod
   a^2 is the k=1 term, giving r = 2na mod a^2 = a(2n mod a).
   
   Maximising 2n mod a over odd positive n:
   
   If a is odd, gcd(4,a) = 1, so as n ranges over odd integers,
   2n (mod a) achieves every residue class including a-1. Thus
   r_max = a(a - 1).
   
   If a is even, write a = 2m. Then 2n mod 2m = 2(n mod m). Since
   n is odd, n mod m attains the value m-1 (which has the same
   parity as m-1; one verifies that both parities of m work because
   the full odd arithmetic progression mod m covers all residues
   when m is odd, and covers all odd residues — including m-1 —
   when m is even). Hence r_max = a(a - 2).
   
   In closed form: r_max(a) = a(a - 1) for odd a, a(a - 2) for
   even a. This is verifiable against the given example a = 7:
   7 * 6 = 42.
   
   The answer is the direct sum of these 998 values, requiring
   O(N) integer multiplications — trivially feasible for N = 1000.
   
   Parallelization strategy:
   The 998 independent per-a evaluations of r_max are distributed
   across all available cores via ParallelSum. Each core handles a
   disjoint subrange of a values; the partial sums are aggregated
   with Plus. No shared state is needed.
   
   Implementation plan:
   Detect cores, launch kernels. Define rMax[a] using the parity-
   based closed form. Compute the answer via ParallelSum over
   a = 3..1000 and return.
*)

nCores = $ProcessorCount;
LaunchKernels[];

rMax[a_] := If[OddQ[a], a (a - 1), a (a - 2)]

DistributeDefinitions[rMax];

solve[] := ParallelSum[rMax[a], {a, 3, 1000}]

solve[]
