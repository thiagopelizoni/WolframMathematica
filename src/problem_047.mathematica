(* Project Euler Problem 47: Distinct Primes Factors

   Problem Description:
   The problem asks for the first four consecutive integers to have four distinct prime factors each.
   For example, 14 = 2 * 7 and 15 = 3 * 5 are two consecutive numbers with two distinct prime factors.

   Mathematical Solution:
   1. Definition:
      Let $\omega(n)$ be the number of distinct prime factors of a positive integer $n$.
      This function is known as the little omega function (PrimeNu in Wolfram Language).
      We are looking for the smallest integer $n$ such that:
      $\omega(n) = \omega(n+1) = \omega(n+2) = \omega(n+3) = 4$.

   2. Search Algorithm:
      We iterate through integers $k$ starting from the lower bound (computationally, 2 is fine, 
      though 2*3*5*7 = 210 is the theoretical minimum for a single number).
      We maintain a counter for the streak of consecutive integers satisfying $\omega(k) = 4$.
      If $\omega(k) \neq 4$, the counter resets to 0.
      When the counter reaches 4, the solution is the starting integer of the current streak, 
      calculated as $k - 3$.
*)

targetStreak = 4;
consecutive = 0;
n = 2;

Catch[
  While[True,
    If[PrimeNu[n] == targetStreak,
       consecutive++;
       If[consecutive == targetStreak, Throw[n - targetStreak + 1]],
       consecutive = 0
    ];
    n++
  ]
]