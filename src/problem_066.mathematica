(*
Project Euler Problem 66: Diophantine Equation
URL: https://projecteuler.net/problem=066

Problem Statement:
We consider the Diophantine equation x^2 - D y^2 = 1, where D is a positive non-square integer. For a given D, there
are infinitely many integer solutions (x, y). We are interested in the fundamental solution, which is the solution with
the smallest positive x. The task is to find the value of D <= 1000 for which this minimal x is maximized.

Mathematical Analysis:
The equation x^2 - D y^2 = 1 is a Pell's equation. The fundamental solution (x, y) can be found using the continued
fraction expansion of Sqrt[D]. If Sqrt[D] = [a0; a1, a2, ..., an, ...], the convergents p_k/q_k approximate Sqrt[D].
The fundamental solution (x, y) corresponds to one of these convergents. Specifically, let r be the period length of
the continued fraction of Sqrt[D].
1. If r is even, the fundamental solution is (x, y) = (p_(r-1), q_(r-1)).
2. If r is odd, the fundamental solution is (x, y) = (p_(2r-1), q_(2r-1)).
Alternatively, we can simply generate convergents p_k/q_k until the condition p_k^2 - D q_k^2 = 1 is satisfied. Since
the sequence of x values grows exponentially, we need to handle very large integers. Mathematica handles arbitrary-
precision integers automatically, so overflow is not a concern, provided we have enough memory. The constraints
D <= 1000 are small enough that the period lengths and solution sizes are manageable (though x can be very large,
e.g., for D=61, x has 10 digits; for larger D, it can exceed 100 digits).

Asymptotic Analysis:
The period length of Sqrt[D] is roughly O(Sqrt(D) log(D)). Generating the continued fraction and convergents is
efficient. For D <= 1000, the computation is trivial for a modern computer. The complexity is dominated by the
arithmetic operations on large integers. The number of D values is 1000, and for each, we perform a continued fraction
expansion. This fits comfortably within a fraction of a second to a few seconds.

Parallelization Strategy:
The calculation for each D is independent. We can distribute the values of D (excluding perfect squares) across
available cores. Each core computes the minimal x for its assigned subset of D values. We then aggregate the results
by finding the pair {D, x} with the maximum x.
We will use `ParallelMap` to compute {D, minimalX} for each candidate D.

Implementation Details:
- Identify D values: Range[1000], excluding perfect squares.
- Function `minimalSolutionX[D]`:
  - Compute continued fraction of Sqrt[D] using `ContinuedFraction` or a manual step-by-step approach to get
    convergents on the fly.
  - Using `Convergents[Sqrt[D], k]` might require guessing k. A robust way is to use `ContinuedFraction` to get the
    period, or use the recurrence relation for Pell equation convergents directly until the condition is met.
  - Given the potentially large size of x, we must return exact integers.
- Aggregation: `MaximalBy` or sorting to find the D associated with the largest x.
*)

nCores = $ProcessorCount;

solve[] := Module[{candidates, getMinimalX, results, best},
  (* Filter out perfect squares from 1 to 1000 *)
  candidates = Select[Range[1000], ! IntegerQ[Sqrt[#]] &];

  (* Function to find the minimal x for x^2 - D y^2 = 1 *)
  getMinimalX = Function[d,
    Module[{cf, convs, sol},
      (* We need the convergents of Sqrt[d]. 
         The fundamental solution corresponds to p/q where p^2 - d*q^2 == 1.
         The period length determines which convergent it is.
         If period length L is even, solution is at index L-1 (0-indexed).
         If period length L is odd, solution is at index 2L-1.
         Mathematica's ContinuedFraction gives the periodic part.
      *)
      cf = ContinuedFraction[Sqrt[d]];
      
      (* ContinuedFraction returns {integer_part, {period_list}} *)
      (* The sequence of partial quotients is integer_part followed by period_list repeating. *)
      
      (* Calculate the specific convergent index needed. *)
      (* Let L be the length of the period. *)
      (* Index needed k = L - 1 (if L even) or 2L - 1 (if L odd). *)
      (* Note: FromContinuedFraction takes a list of partial quotients. *)
      
      Module[{a0, period, L, k, partialQuotients, p, q},
        a0 = First[cf];
        period = Last[cf];
        L = Length[period];
        
        If[EvenQ[L],
          k = L - 1,
          k = 2 * L - 1
        ];
        
        (* Construct the list of partial quotients up to index k *)
        (* Sequence: a0, p1, p2, ..., pL, p1, p2, ... *)
        (* We need k+1 terms total (indices 0 to k). *)
        (* The period part repeats. *)
        
        partialQuotients = Prepend[
          PadRight[period, k + 1, period], (* generates enough periodic terms *)
          a0
        ];
        (* PadRight with periodic padding generates cyclic repetitions. 
           However, PadRight[list, n, padding] pads with 'padding' on the right. 
           If 'padding' is the list itself, it acts as a cyclic filler. 
           Wait, PadRight[period, k, period] might not align perfectly if k is large, 
           but here k is at most 2L-1.
           Let's use a safer construction.
        *)
        partialQuotients = Join[{a0}, Flatten[Table[period, {Ceiling[k/L] + 1}]]];
        partialQuotients = Take[partialQuotients, k + 1];
        
        (* Compute the convergent p/q *)
        {p, q} = {Numerator[#], Denominator[#]} & @ FromContinuedFraction[partialQuotients];
        
        (* Return the result pair *)
        {d, p}
      ]
    ]
  ];

  (* Distribute definitions for parallel execution *)
  DistributeDefinitions[getMinimalX];

  (* Compute {D, x} for all non-square D in parallel *)
  results = ParallelMap[getMinimalX, candidates];

  (* Find the pair with the maximum x *)
  best = First @ TakeLargestBy[results, Last, 1];

  (* Return D *)
  First[best]
];

solve[]