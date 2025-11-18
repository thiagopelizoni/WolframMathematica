(* Project Euler Problem 12: Highly divisible triangular number
   
   PROBLEM STATEMENT:
   Find the first triangular number to have over five hundred divisors.
   
   MATHEMATICAL BACKGROUND:
   - Triangular numbers T_n are defined as T_n = 1 + 2 + ... + n = n(n+1)/2
   - These represent the number of objects that can form an equilateral triangle
   - The divisor function τ(n) counts the number of positive divisors of n
   
   SOLUTION APPROACH:
   We employ an efficient divisor counting algorithm based on the property
   that divisors come in pairs (d, n/d). By iterating only up to √n, we
   count each pair once, adjusting for perfect squares where d = √n.
*)

(* Function: CountDivisors
   Purpose: Efficiently compute the number of divisors of n
   
   Algorithm explanation:
   For any divisor d of n where d ≤ √n, there exists a complementary
   divisor n/d where n/d ≥ √n. By iterating from 1 to √n, we count
   both divisors in each pair, giving us 2k divisors where k is the
   number of divisor pairs.
   
   Special case: If n is a perfect square, then √n is counted twice
   (once as d and once as n/d), so we subtract 1 to correct the count.
   
   Time complexity: O(√n), significantly better than O(n) naive approach
*)
CountDivisors[n_Integer] := Module[{divisors, sqrtN, i},
  divisors = 0;
  sqrtN = Floor[Sqrt[n]];
  
  (* Iterate through potential divisors up to √n *)
  For[i = 1, i <= sqrtN, i++,
    If[Mod[n, i] == 0,
      (* Found a divisor pair: (i, n/i) *)
      divisors += 2
    ]
  ];
  
  (* Correction for perfect squares: √n was counted twice *)
  If[sqrtN * sqrtN == n,
    divisors -= 1
  ];
  
  divisors
];

(* Function: FindTriangularWithDivisors
   Purpose: Find the first triangular number with more than minDivisors divisors
   
   Mathematical reasoning:
   We generate triangular numbers sequentially using the recurrence relation:
   T_n = T_{n-1} + n
   
   Starting with T_1 = 1, we iteratively add the next integer to build
   successive triangular numbers. For each T_n generated, we compute τ(T_n)
   using our efficient divisor counting function.
   
   The algorithm terminates when we find T_n such that τ(T_n) > minDivisors.
*)
FindTriangularWithDivisors[minDivisors_Integer] := Module[{num, triangle},
  num = 1;        (* Current index n in the sequence *)
  triangle = 0;   (* Current triangular number T_n *)
  
  While[True,
    (* Generate next triangular number: T_n = T_{n-1} + n *)
    triangle += num;
    
    (* Check if τ(T_n) exceeds our threshold *)
    If[CountDivisors[triangle] > minDivisors,
      Return[triangle]
    ];
    
    (* Move to next index *)
    num += 1;
  ];
];

(* MAIN COMPUTATION *)
(* Find the first triangular number with over 500 divisors *)
answer = FindTriangularWithDivisors[500];

(* Display result *)
Print["The first triangular number with over 500 divisors is: ", answer];
Print["Verification: This number has ", CountDivisors[answer], " divisors"];

(* Additional mathematical insight:
   The answer can be verified using Mathematica's built-in DivisorSigma[0, n]
   which computes τ(n) directly: *)
Print["Built-in verification: ", DivisorSigma[0, answer], " divisors"];

answer
