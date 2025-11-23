(* Project Euler Problem 50: Consecutive Prime Sum

   Problem Description:
   The problem asks for the prime number below 1,000,000 that can be written as the sum of the 
   most consecutive primes. For example, 41 = 2 + 3 + 5 + 7 + 11 + 13 is a sum of 6 consecutive primes.

   Mathematical Solution:
   1. Data Structure (Prefix Sums):
      Let $P = \{p_1, p_2, \dots, p_k\}$ be the ordered sequence of primes.
      We construct the cumulative sum sequence $S$ where $S_n = \sum_{m=1}^{n} p_m$ and $S_0 = 0$.
      The sum of any consecutive subsequence of primes from index $i$ to $j$ ($p_i + \dots + p_j$) 
      can be computed in $O(1)$ time as $\Delta S = S_j - S_{i-1}$.

   2. Search Space Optimization:
      The brute-force approach involves $O(k^2)$ checks. However, we are looking for the maximum length $L = j - (i-1)$.
      We iterate through the starting index $i$. For the ending index $j$, we only need to consider 
      intervals where the length is greater than the currently found maximum ($j > i + \text{current\_max}$).
      
   3. Bounds:
      Since $p_n$ is strictly increasing, the partial sums grow quadratically.
      If $S_j - S_{i-1} \ge \text{limit}$, we can terminate the inner loop early, as extending 
      the sequence further will only increase the sum.
*)

limit = 1000000;
primes = Prime[Range[PrimePi[limit]]];
cumulative = Prepend[Accumulate[primes], 0];
count = Length[primes];

maxLength = 0;
maxPrime = 0;

Do[
  Do[
    currentSum = cumulative[[j]] - cumulative[[i]];
    
    If[currentSum >= limit, Break[]];
    
    If[PrimeQ[currentSum],
       maxLength = j - i - 1;
       maxPrime = currentSum
    ],
    {j, i + maxLength + 1, count + 1}
  ],
  {i, 1, count}
];

maxPrime