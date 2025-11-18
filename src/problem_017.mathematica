(* Problem: https://projecteuler.net/problem=17 *)
(* Counting letters in written numbers. *)

(*
   Problem statement (Project Euler 17, paraphrased):
   
   If the numbers 1 to 5 are written out in words: one, two, three, four, five,
   then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
   
   If all the numbers from 1 to 1000 (one thousand) inclusive were written out
   in words, how many letters would be used?
   
   NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
   forty-two) contains 23 letters and 115 (one hundred and fifteen) contains
   20 letters. The use of "and" when writing out numbers is in compliance with
   British usage.

   ---------------------------------------------------------------------------
   Mathematical background: Linguistic representation of numbers
   ---------------------------------------------------------------------------

   1) Problem setup:
      We need to convert each integer from 1 to 1000 into its English word
      representation following British conventions, then count the total number
      of letters (excluding spaces and hyphens).

   2) British number naming conventions:
      - Numbers 1-9: one, two, three, ..., nine
      - Numbers 10-19: ten, eleven, twelve, ..., nineteen (irregular forms)
      - Multiples of 10: twenty, thirty, forty, ..., ninety
      - Compound numbers (21-99): combine tens and ones without spaces
        Example: 42 = "fortytwo" (when counting letters)
      - Hundreds: use "hundred" after the digit
        Example: 300 = "threehundred"
      - British rule: Insert "and" between hundreds and remainder
        Example: 342 = "threehundredandfortytwo"
        Example: 115 = "onehundredandfifteen"
      - Special case: 1000 = "onethousand"

   3) Algorithm structure:
      We need a function that converts numbers to words, then counts letters.
      
      The conversion has three main cases:
      a) Numbers 1-19: Direct lookup from predefined lists
      b) Numbers 20-99: Combine tens place + ones place
      c) Numbers 100-999: Combine hundreds + "and" + remainder
      d) Special case: 1000

   4) Implementation strategy:
      Create a helper function convertUnder1000 that handles numbers 0-999:
      - If n < 10: Look up in ones array
      - If 10 <= n < 20: Look up in teens array (adjusted index)
      - If 20 <= n < 100: Combine tens place and ones place
      - If 100 <= n < 1000: Combine hundreds + "and" + recursively convert remainder
      
      Main function numberToWords handles special case of 1000.

   5) Counting letters:
      For each number 1 to 1000:
      - Convert to words
      - Count the length of the resulting string (which has no spaces/hyphens)
      - Sum all lengths

   6) Wolfram Language approach:
      We implement the same logic as Python, but can also leverage Wolfram's
      pattern matching and functional programming features for cleaner code.

   ---------------------------------------------------------------------------
   Wolfram Language implementation
   ---------------------------------------------------------------------------
*)

(* Define lookup tables for number words *)
ones = {"", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"};
teens = {"ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", 
         "sixteen", "seventeen", "eighteen", "nineteen"};
tens = {"", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"};

(* Helper function to convert numbers under 1000 to words *)
convertUnder1000[n_Integer] := Which[
  (* Case 1: Numbers 0-9 (use ones array, 1-indexed) *)
  n < 10,
    ones[[n + 1]],
  
  (* Case 2: Numbers 10-19 (use teens array, adjust index) *)
  n < 20,
    teens[[n - 10 + 1]],
  
  (* Case 3: Numbers 20-99 (combine tens and ones) *)
  n < 100,
    tens[[Quotient[n, 10] + 1]] <> 
    If[Mod[n, 10] != 0, ones[[Mod[n, 10] + 1]], ""],
  
  (* Case 4: Numbers 100-999 (hundreds + "and" + remainder) *)
  True,
    ones[[Quotient[n, 100] + 1]] <> "hundred" <>
    If[Mod[n, 100] != 0, 
      "and" <> convertUnder1000[Mod[n, 100]], 
      ""]
]

(* Main function to convert any number 1-1000 to words *)
numberToWords[n_Integer] := Which[
  (* Special case: 1000 *)
  n == 1000,
    "onethousand",
  
  (* Special case: 0 (not used in this problem, but for completeness) *)
  n == 0,
    "zero",
  
  (* General case: 1-999 *)
  True,
    convertUnder1000[n]
]

(* Calculate total letter count for numbers 1 to 1000 *)
letterCount1To1000[] := Module[
  {
    numbers, words, letterCounts
  },
  
  (* Generate all numbers from 1 to 1000 *)
  numbers = Range[1, 1000];
  
  (* Convert each number to words *)
  words = numberToWords /@ numbers;
  
  (* Count letters in each word (StringLength counts characters) *)
  letterCounts = StringLength /@ words;
  
  (* Sum all letter counts *)
  Total[letterCounts]
]

(* More concise functional version using composition *)
letterCount1To1000Functional[] :=
  Total[StringLength[numberToWords[#]]& /@ Range[1, 1000]]

(* Even more concise using Map and Composition *)
letterCount1To1000Composed[] :=
  Total@Map[StringLength@*numberToWords, Range[1, 1000]]

(* Calculate the answer *)
letterCount1To1000[]
