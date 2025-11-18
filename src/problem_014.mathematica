(* Problem: https://projecteuler.net/problem=14 *)
(* Longest Collatz sequence starting number below one million. *)

(*
   Problem statement (Project Euler 14, paraphrased):
   
   The following iterative sequence is defined for the set of positive integers:
   
     n → n/2    (n is even)
     n → 3n + 1 (n is odd)
   
   Using the rule above and starting with 13, we generate the following sequence:
     13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
   
   It can be seen that this sequence (starting at 13 and finishing at 1) 
   contains 10 terms. Although it has not been proved yet (Collatz conjecture),
   it is thought that all starting numbers finish at 1.
   
   Which starting number, under one million, produces the longest chain?
   
   NOTE: Once the chain starts the terms are allowed to go above one million.
   
   The known correct answer is:
     837799 (which produces a chain of length 525)

   ---------------------------------------------------------------------------
   Mathematical background: The Collatz Conjecture
   ---------------------------------------------------------------------------

   1) Definition of the Collatz function:
      For any positive integer n, define:
        C(n) = n/2      if n is even
        C(n) = 3n + 1   if n is odd
      
      The Collatz sequence starting from n₀ is:
        n₀, C(n₀), C(C(n₀)), C(C(C(n₀))), ...
      
      We continue applying C until we reach 1.

   2) The Collatz Conjecture (unproven):
      For every positive integer n, the Collatz sequence eventually reaches 1.
      This has been verified computationally for extremely large numbers but
      has never been proven mathematically.

   3) Sequence length:
      The "length" of a Collatz sequence starting from n is the number of 
      terms in the sequence, including both the starting number and the 
      final 1.
      
      Example: Starting from 13:
        13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
      This sequence has length 10.

   4) Problem objective:
      Among all starting numbers 1 ≤ n < 1,000,000, find the one that
      produces the longest Collatz sequence.

   5) Optimization strategy: Memoization
      Computing Collatz sequences can be expensive because:
      - Some sequences are very long (hundreds of steps)
      - Many sequences overlap (e.g., if we compute the sequence for 40,
        we've already computed part of the sequence for 13)
      
      Solution: Use memoization (dynamic programming) to cache the length
      of sequences we've already computed. When computing the length for n:
        - If we reach a number m whose length we've already cached, we can
          immediately compute: length(n) = steps_taken + length(m)
        - This avoids redundant computation and dramatically speeds up the
          algorithm.

   6) Implementation details:
      - We use Wolfram Language's built-in memoization pattern with
        downvalues: collatzLength[n_] := collatzLength[n] = ...
      - This automatically caches results as they are computed
      - We iterate through all starting numbers from 1 to 999,999
      - We track the maximum length found and the corresponding starting number
      - MaximalBy provides an elegant functional approach to find the maximum

   ---------------------------------------------------------------------------
   Wolfram Language implementation
   ---------------------------------------------------------------------------
*)

(* Clear any previous definitions of collatzLength to ensure clean memoization *)
ClearAll[collatzLength]

(* Base case: the sequence starting at 1 has length 1 (just the number 1 itself) *)
collatzLength[1] = 1;

(* Recursive case with memoization:
   The pattern collatzLength[n_] := collatzLength[n] = ... creates a cached
   definition that stores the result after the first computation. *)
collatzLength[n_Integer?Positive] := collatzLength[n] = 
  1 + If[EvenQ[n],
    (* If n is even: next term is n/2 *)
    collatzLength[n/2],
    (* If n is odd: next term is 3n+1 *)
    collatzLength[3*n + 1]
  ]

longestCollatzStartingNumber[limit_Integer?Positive] := Module[
  {
    (* We'll find the starting number with maximum sequence length *)
    startingNumbers, lengthsAndNumbers
  },
  
  (* Generate all starting numbers from 1 to limit-1 *)
  startingNumbers = Range[1, limit - 1];
  
  (* Compute (length, starting_number) pairs for all starting numbers.
     We pair each number with its sequence length. *)
  lengthsAndNumbers = {collatzLength[#], #}& /@ startingNumbers;
  
  (* Find the pair with maximum length (first element of pair).
     MaximalBy returns a list containing the element(s) with the largest value.
     First extracts the first (and only) maximal pair from the list.
     Last then extracts the second element (the starting number) from that pair. *)
  Last[First[MaximalBy[lengthsAndNumbers, First]]]
]

(* Find the starting number under 1,000,000 that produces the longest
   Collatz sequence. Expected answer: 837799 *)
longestCollatzStartingNumber[1000000]
