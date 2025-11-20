(* Project Euler Problem 36: Double-base Palindromes

   Problem Description:
   The objective is to calculate the sum of all positive integers n strictly less than 1,000,000 
   such that n is palindromic in both the decimal base (base 10) and the binary base (base 2).
   (e.g., 585 in base 10 is 585, and in base 2 is 1001001001).

   Mathematical Solution:
   1. Definition: Let D_b(n) be the sequence of digits of n in base b.
      A number satisfies the condition if D_10(n) is a palindrome AND D_2(n) is a palindrome.
   2. Search Space: The domain is the set of integers Z = {1, 2, ..., 10^6 - 1}.
   3. Parity Constraint (Optimization Note): Since binary numbers cannot have leading zeros, 
      a binary palindrome must start with 1, and therefore must end with 1. Thus, n must be odd. 
      (While the code below checks the full range for completeness matching the Python logic, 
      mathematically only odd numbers need to be tested).
   4. Aggregation: Compute the sum of all n in Z satisfying the dual-palindrome property.
*)

DoubleBasePalindromeQ[n_Integer] := PalindromeQ[n] && PalindromeQ[IntegerDigits[n, 2]]

limit = 1000000;

Total[Select[Range[1, limit - 1], DoubleBasePalindromeQ]]