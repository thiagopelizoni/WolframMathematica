(* Problem: https://projecteuler.net/problem=22 *)
(* Names scores from a sorted list. *)

(*
   Problem statement (Project Euler 22, paraphrased):
   
   Using a text file containing over five-thousand first names, begin by sorting
   it into alphabetical order. Then working out the alphabetical value for each
   name, multiply this value by its alphabetical position in the list to obtain
   a name score.
   
   For example, when the list is sorted into alphabetical order, COLIN, which is
   worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN
   would obtain a score of 938 × 53 = 49714.
   
   What is the total of all the name scores in the file?

   ---------------------------------------------------------------------------
   Mathematical background: Alphabetical encoding and weighted sums
   ---------------------------------------------------------------------------

   1) Problem components:
      This problem involves three main operations:
      a) Reading and parsing data from a file
      b) Sorting names alphabetically
      c) Computing weighted scores based on character values and positions

   2) Alphabetical value:
      Each letter has a numerical value based on its position in the alphabet:
        A = 1, B = 2, C = 3, ..., Z = 26
      
      The alphabetical value of a name is the sum of its letter values.
      
      Example: COLIN
        C = 3, O = 15, L = 12, I = 9, N = 14
        Value = 3 + 15 + 12 + 9 + 14 = 53

   3) Computing letter values:
      In ASCII/Unicode, uppercase letters have consecutive character codes:
        'A' has code 65
        'B' has code 66
        ...
        'Z' has code 90
      
      To convert a letter to its alphabetical position:
        position = ord(letter) - ord('A') + 1
      
      Example: ord('C') - ord('A') + 1 = 67 - 65 + 1 = 3

   4) Name score calculation:
      Once names are sorted alphabetically, each name gets a position:
        Position 1: first name alphabetically
        Position 2: second name alphabetically
        ...
      
      Name score = (alphabetical position) × (alphabetical value)
      
      Example: COLIN at position 938
        Name score = 938 × 53 = 49,714

   5) Total score:
      Sum all individual name scores across the entire sorted list.

   6) Data source:
      The problem provides a text file at a specific URL containing names
      formatted as: "NAME1","NAME2","NAME3",...
      
      Parsing steps:
      - Download the file content
      - Remove quotation marks
      - Split by commas to get individual names

   7) Algorithm outline:
      Step 1: Download and parse the names file
      Step 2: Sort names alphabetically
      Step 3: For each name at position i:
              a) Compute alphabetical value (sum of letter positions)
              b) Multiply by position (i+1, since positions start at 1)
              c) Add to total score
      Step 4: Return total score

   8) Wolfram Language tools:
      - Import or URLRead for fetching data
      - StringSplit and StringDelete for parsing
      - Sort for alphabetical ordering
      - LetterNumber converts letters to positions directly
      - MapIndexed for position-aware operations
      - Total for summing

   ---------------------------------------------------------------------------
   Wolfram Language implementation
   ---------------------------------------------------------------------------
*)

getNames[] := Module[
  {
    url, content, names
  },
  
  (* URL for the names file from Project Euler *)
  url = "https://projecteuler.net/resources/documents/0022_names.txt";
  
  (* Download the file content as a string *)
  content = Import[url, "Text"];
  
  (* Parse the content: remove quotes and split by commas *)
  names = StringSplit[StringDelete[content, "\""], ","];
  
  names
]

calculateNameScore[names_List] := Module[
  {
    sortedNames, totalScore
  },
  
  (* Step 1: Sort names alphabetically *)
  sortedNames = Sort[names];
  
  (* Step 2: Calculate weighted scores for all names *)
  totalScore = Total[
    MapIndexed[
      Function[{name, pos},
        Module[{alphabeticalValue, position, nameScore},
          (* Get position in list (pos is {index}, so extract with First) *)
          position = First[pos];
          
          (* Calculate alphabetical value: sum of letter positions *)
          (* LetterNumber converts each letter to 1-26 *)
          alphabeticalValue = Total[LetterNumber[Characters[name]]];
          
          (* Name score = position × alphabetical value *)
          nameScore = position * alphabeticalValue;
          
          nameScore
        ]
      ],
      sortedNames
    ]
  ];
  
  totalScore
]

(* More concise functional version *)
calculateNameScoreFunctional[names_List] := Module[
  {
    sortedNames
  },
  
  sortedNames = Sort[names];
  
  (* Compute all scores in one expression *)
  Total[
    MapIndexed[
      #2[[1]] * Total[LetterNumber[Characters[#1]]]&,
      sortedNames
    ]
  ]
]

(* Main execution *)
namesScoresTotal[] := Module[
  {
    names
  },
  
  (* Fetch and parse names from the file *)
  names = getNames[];
  
  (* Calculate and return total score *)
  calculateNameScore[names]
]

(* Calculate the total of all name scores *)
namesScoresTotal[]
