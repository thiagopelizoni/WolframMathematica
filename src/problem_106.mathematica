(* Project Euler Problem 106 — https://projecteuler.net/problem=106
 
   For n = 12, count how many pairs of disjoint non-empty subsets must be explicitly tested for equal sums when
   verifying the special sum set property. By condition (ii), only pairs of equal size k can be ambiguous; for
   different sizes the ordering of sums is forced by size, so no equality test is needed.
 
   Fix k and choose 2k elements. Any unordered pair of disjoint k-subsets corresponds to a binary word of length 2k
   with k A's and k B's, modulo swapping A and B. There are (1/2) * Binomial(2k, k) such pairs. If the elements of
   one subset are strictly less than the corresponding elements of the other when both are sorted, then sums are
   ordered and no test is required. This is equivalent to the ballot condition that every prefix has at least as many
   A's as B's, counted by the Catalan number C_k = Binomial(2k, k)/(k+1). Therefore the number of pairs that still
   require testing for a fixed 2k-set is (1/2) * Binomial(2k, k) - C_k. Summing over all choices of 2k elements yields
   total = Sum_{k=2..floor(n/2)} Binomial(n, 2k) * ((1/2) * Binomial(2k, k) - C_k).
 
   The computation is O(n) with exact integer arithmetic; for n=12 it is trivial. Parallelization is applied across
   the independent k values via ParallelTable, and the associative Total aggregates the results without shared state.
   Implementation uses Binomial for exact combinatorics and a Catalan function, returning the integer answer.
 *)

nCores = $ProcessorCount;
LaunchKernels[];

solve[] := Module[{n, catalan, terms},
  n = 12;
  catalan[k_Integer] := Binomial[2 k, k]/(k + 1);
  terms = ParallelTable[
    Binomial[n, 2 k]*(Binomial[2 k, k]/2 - catalan[k]),
    {k, 2, Floor[n/2]}
  ];
  Total[terms]
];

solve[]