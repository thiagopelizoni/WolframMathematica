(* Project Euler Problem 46: Goldbach's Other Conjecture

   Problem Description:
   Christian Goldbach proposed that every odd composite number can be written as the sum of a 
   prime and twice a square: $n = p + 2k^2$.
   Examples: $9 = 7 + 2(1^2)$, $15 = 7 + 2(2^2)$, $21 = 3 + 2(3^2)$.
   The objective is to find the smallest odd composite integer that disproves this conjecture.

   Mathematical Solution:
   1. Domain Definition:
      Let $S$ be the set of odd composite integers: $S = \{ n \in \mathbb{Z}^+ \mid n \text{ is odd} \land n \text{ is composite} \}$.
   
   2. Conjecture Verification (Predicate):
      For a given $n \in S$, the conjecture holds if:
      $\exists p \in \mathbb{P}$ (where $p < n$) such that $\frac{n - p}{2}$ is a perfect square.
      
   3. Algorithm:
      We iterate through odd integers $n$ starting from 3.
      If $n$ is prime, it is skipped (not in domain $S$).
      If $n$ is composite, we verify the predicate against the set of primes less than $n$.
      The first $n$ for which the predicate yields False is the counterexample.
*)

ConjectureHoldsQ[n_Integer] := AnyTrue[
   Prime[Range[PrimePi[n]]], 
   IntegerQ[Sqrt[(n - #)/2]] &
]

n = 3;

While[
   PrimeQ[n] || ConjectureHoldsQ[n], 
   n += 2
];

n