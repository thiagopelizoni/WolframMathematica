(* Project Euler Problem 39: Integer Right Triangles

   Problem Description:
   The objective is to find the value of the perimeter p <= 1000 that maximizes the number of 
   integer right triangles {a, b, c} such that a + b + c = p.
   
   Mathematical Solution:
   1. System of Equations:
      We have the Pythagorean theorem a^2 + b^2 = c^2 and the linear constraint c = p - a - b.
   
   2. Variable Reduction:
      Substituting c into the quadratic equation:
      a^2 + b^2 = (p - a - b)^2
      a^2 + b^2 = p^2 + a^2 + b^2 - 2pa - 2pb + 2ab
      0 = p^2 - 2pa - 2b(p - a)
      
      Solving for b yields the constraint function:
      b = (p^2 - 2pa) / (2(p - a))
      
   3. Search Optimization:
      Instead of iterating through both a and b (O(p^2)), we iterate only through a in the 
      interval [1, p/3]. A valid solution exists if and only if the derived b is an integer.
      
   4. Parity Constraint:
      For integer right triangles, the perimeter p is always even. 
      (Proof: sum of two odd squares is even (but not divisible by 4), sum of even/odd squares is odd. 
      This constrains c and consequently the sum a+b+c to be even).
*)

SolutionCount[p_Integer] := Count[
   Table[(p^2 - 2 p a)/(2 (p - a)), {a, 1, Floor[p/3]}], 
   _Integer
]

domain = Range[2, 1000, 2];

First @ MaximalBy[domain, SolutionCount]