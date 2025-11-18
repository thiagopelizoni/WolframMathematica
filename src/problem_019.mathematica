(* Problem: https://projecteuler.net/problem=19 *)
(* Counting Sundays on the first of the month. *)

(*
   Problem statement (Project Euler 19, paraphrased):
   
   You are given the following information, but you may prefer to do some
   research for yourself.
   
   - 1 Jan 1900 was a Monday.
   - Thirty days has September, April, June and November.
     All the rest have thirty-one,
     Saving February alone, which has twenty-eight, rain or shine.
     And on leap years, twenty-nine.
   - A leap year occurs on any year evenly divisible by 4, but not on a
     century unless it is divisible by 400.
   
   How many Sundays fell on the first of the month during the twentieth
   century (1 Jan 1901 to 31 Dec 2000)?

   ---------------------------------------------------------------------------
   Mathematical background: Calendar calculations and day-of-week arithmetic
   ---------------------------------------------------------------------------

   1) Problem setup:
      We need to count how many times the first day of a month falls on a
      Sunday during the 100-year period from 1 January 1901 to 31 December 2000.
      
      This requires:
      - Iterating through each month in the date range
      - Determining the day of the week for the 1st of each month
      - Counting how many of these are Sundays

   2) Calendar systems and day-of-week calculations:
      The Gregorian calendar (used since 1582 in Catholic countries, adopted
      by Britain in 1752) is a complex system with:
      - 7-day weeks that cycle continuously
      - Months of varying lengths (28-31 days)
      - Leap years that add an extra day to February
      
      Computing the day of the week for an arbitrary date is non-trivial and
      involves modular arithmetic. Fortunately, most programming languages
      provide built-in functions for this.

   3) Day-of-week representation:
      Different systems use different conventions:
      - Python's datetime.weekday(): Monday=0, Tuesday=1, ..., Sunday=6
      - Wolfram's DayName: Monday, Tuesday, ..., Sunday (symbolic)
      - ISO 8601 standard: Monday=1, Tuesday=2, ..., Sunday=7
      
      The Python code checks if weekday() == 6, which means Sunday.

   4) Leap year rules:
      A year is a leap year if:
      - It is divisible by 4, AND
      - If it's a century year (divisible by 100), it must also be divisible by 400
      
      Examples:
      - 1900: divisible by 4 and 100, but NOT by 400 → not a leap year
      - 2000: divisible by 4, 100, AND 400 → leap year
      - 1904: divisible by 4, not a century → leap year

   5) Algorithm strategy:
      Iterate through all months from January 1901 to December 2000:
      - For each month, create a date object for the 1st day
      - Check if that day is a Sunday
      - Count the total number of Sundays
      
      Total months to check: 100 years × 12 months = 1,200 months

   6) Wolfram Language approach:
      Wolfram Language has sophisticated built-in date handling:
      - DateObject creates date representations
      - DayName extracts the day of the week as a symbolic value (Sunday, Monday, etc.)
      - We can iterate through year/month combinations and count matches

   7) Alternative approaches:
      - Mathematical: Use Zeller's congruence or similar algorithms to compute
        day-of-week without library functions
      - Analytical: Count leap years and use modular arithmetic to track
        day-of-week progression
      
      However, using built-in date functions is simpler, more reliable, and
      sufficiently efficient for this problem.

   ---------------------------------------------------------------------------
   Wolfram Language implementation
   ---------------------------------------------------------------------------
*)

countingSundays[] := Module[
  {
    sundays = 0,
    year, month, firstDay
  },
  
  (* Iterate through all years in the twentieth century (1901-2000) *)
  For[year = 1901, year <= 2000, year++,
    (* For each year, iterate through all 12 months *)
    For[month = 1, month <= 12, month++,
      (* Create a DateObject for the 1st day of the current month.
         DateObject[{year, month, day}] creates a date representation. *)
      firstDay = DateObject[{year, month, 1}];
      
      (* Check if the first day of the month is a Sunday.
         DayName[date] returns the symbolic day name (Sunday, Monday, etc.). *)
      If[DayName[firstDay] === Sunday,
        sundays++
      ];
    ];
  ];
  
  (* Return the total count of Sundays *)
  sundays
]

(* More functional version using nested iteration and counting *)
countingSundaysFunctional[] := Module[
  {
    years = Range[1901, 2000],
    months = Range[1, 12],
    allFirstDays, sundayCount
  },
  
  (* Generate all first-day dates using nested Table *)
  allFirstDays = Flatten[
    Table[
      DateObject[{year, month, 1}],
      {year, years},
      {month, months}
    ]
  ];
  
  (* Count how many of these dates fall on Sunday *)
  Count[DayName /@ allFirstDays, Sunday]
]

(* Most concise version using functional composition *)
countingSundaysComposed[] :=
  Count[
    DayName[DateObject[{#1, #2, 1}]]& @@@ 
      Tuples[{Range[1901, 2000], Range[1, 12]}],
    Sunday
  ]

(* Calculate the number of Sundays on the 1st of the month from 1901-2000 *)
countingSundays[]
