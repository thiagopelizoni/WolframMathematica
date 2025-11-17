(* Problem: https://projecteuler.net/problem=10 *)
(* Summation of all prime numbers strictly less than a given upper bound. *)

(*
   Problem statement (Project Euler 10, paraphrased):
   The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
   Find the sum of all the primes below two million. :contentReference[oaicite:2]{index=2}

   The known correct answer for n = 2,000,000 is:
     142913828922. :contentReference[oaicite:3]{index=3}


   ---------------------------------------------------------------------------
   Mathematical background: Sieve of Eratosthenes
   ---------------------------------------------------------------------------

   1) Goal:
      For a given integer n > 2, we wish to identify all primes p with 2 <= p < n
      and compute their sum.

   2) Prime definition:
      A prime number is an integer greater than 1 that has no positive divisors
      other than 1 and itself. :contentReference[oaicite:4]{index=4}

   3) Sieve of Eratosthenes idea:
      - We create a boolean array 'sieve' that represents the integers
        1, 2, ..., n.
      - Initially, we consider all entries from 2 to n to be "prime candidates".
      - Then, for each i from 2 up to floor(sqrt(n)):
          * If i is still marked as a prime candidate,
            we mark all multiples of i greater than or equal to i^2
            as composite (not prime).
      - At the end of this process, the remaining numbers marked as candidates
        are exactly the prime numbers up to n.

      Why start at i^2?
      - Any composite multiple of i less than i^2 can be written as i * k
        with k < i, and will have been marked when we processed the smaller
        factor k earlier.

   4) Restricting to primes below n:
      If our array indices run from 1 to n, then index i corresponds to the
      integer i. We want all primes p with 2 <= p < n, so we will sum indices
      i = 2, 3, ..., n - 1 that remain marked as prime.

   5) Complexity:
      The sieve runs in O(n log log n) time and uses O(n) memory, which is
      efficient enough for n = 2,000,000.

   We now implement this algorithm in Wolfram Language.
*)

sumOfPrimesBelow[n_Integer?Positive] := Module[
  {
    sieve, (* boolean array marking prime candidates *)
    limit, i, j, total = 0
  },

  (* Create an array of length n where sieve[[i]] corresponds to integer i.
     Initially, mark all entries as True (candidate primes). *)
  sieve = ConstantArray[True, n];

  (* 1 is not a prime, so mark it False.
     (The array does not represent 0; integer 1 corresponds to index 1.) *)
  sieve[[1]] = False;

  (* We only need to consider potential factors i up to floor(sqrt(n)). *)
  limit = Floor[Sqrt[n]];

  (* Sieve of Eratosthenes core loop. *)
  For[i = 2, i <= limit, i++,
    If[sieve[[i]],
      (* i is still considered prime; mark all multiples of i starting at i^2
         as composite (not prime). *)
      For[j = i*i, j <= n, j += i,
        sieve[[j]] = False;
      ];
    ];
  ];

  (* Now, sieve[[i]] is True exactly when i is prime.
     We want the sum of all primes p with 2 <= p < n,
     which correspond to indices i = 2, 3, ..., n - 1. *)
  Do[
    If[sieve[[i]], total += i],
    {i, 2, n - 1}
  ];

  total
]

(* Example: sum of all primes below 2,000,000.
   The expected Project Euler problem 10 answer is 142913828922. *)
sumOfPrimesBelow[2000000]
