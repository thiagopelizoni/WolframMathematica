(* Project Euler Problem 45: Triangular, Pentagonal, and Hexagonal

   Problem Description:
   The objective is to find the next integer after 40,755 that belongs simultaneously to the sets 
   of Triangular ($T_n$), Pentagonal ($P_n$), and Hexagonal ($H_n$) numbers.
   
   Mathematical Solution:
   1. Generative Formulas:
      $T_n = \frac{n(n+1)}{2}$, $P_n = \frac{n(3n-1)}{2}$, $H_n = n(2n-1)$.
   
   2. Subset Property (Theoretical Insight):
      Note that $H_n = n(2n-1) = \frac{(2n-1)(2n)}{2} = T_{2n-1}$.
      This implies $H \subset T$ (Every hexagonal number is a triangular number).
      While the algorithm below iterates through $T_n$ and checks both $P$ and $H$ (fidelity to the source),
      mathematically one only needs to find $x \in H$ such that $x \in P$.

   3. Inverse Mapping (Membership Test):
      To verify if $x \in P$, solve $3n^2 - n - 2x = 0$ for $n$.
      The positive root is $n = \frac{1 + \sqrt{1 + 24x}}{6}$. Thus, $x \in P \iff \sqrt{1 + 24x} \equiv 5 \pmod 6$.
      
      To verify if $x \in H$, solve $2n^2 - n - x = 0$ for $n$.
      The positive root is $n = \frac{1 + \sqrt{1 + 8x}}{4}$.
*)

PentagonalQ[x_Integer] := IntegerQ[(1 + Sqrt[1 + 24 x]) / 6]

HexagonalQ[x_Integer] := IntegerQ[(1 + Sqrt[1 + 8 x]) / 4]

NextSpecialNumber[] := Module[{i = 286, triangle},
  While[True,
    triangle = i (i + 1) / 2;
    If[PentagonalQ[triangle] && HexagonalQ[triangle],
       Return[triangle]
    ];
    i++
  ]
]

NextSpecialNumber[]