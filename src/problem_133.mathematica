(* Project Euler Problem 133 - https://projecteuler.net/problem=133

   Let R(k) = (10^k - 1)/9. For primes p below 100000, determine which primes can never divide any repunit of the form
   R(10^n), and sum those primes.

   For p coprime to 10, divisibility p | R(m) is equivalent to 10^m congruent to 1 modulo 9 p; this uniform modulus
   handles the exceptional prime p = 3 without ad hoc treatment. Therefore p divides some R(10^n) iff there exists n
   with 10^(10^n) congruent to 1 modulo 9 p.

   Let ord_p denote the multiplicative order of 10 modulo 9 p. Then 10^(10^n) congruent to 1 modulo 9 p exactly when
   ord_p divides 10^n. Hence p appears as a factor of some R(10^n) iff every prime divisor of ord_p lies in {2, 5},
   equivalently ord_p = 2^a 5^b. Primes for which ord_p has any other prime factor are precisely the nonfactors sought.

   The search domain is finite (all primes below 100000), so the algorithm is direct: compute ord_p for each eligible
   prime, strip powers of 2 and 5, and classify by whether the residue equals 1. Prime generation is near-linear in the
   bound; each order computation is fast for this scale, so total runtime is easily feasible.

   Parallelization is embarrassingly parallel over primes. Each prime is tested independently, ParallelMap evaluates the
   predicate on all kernels, and results are aggregated with Pick and Total. This reduction is associative and preserves
   deterministic ordering.

   The Wolfram Language implementation uses exact integer arithmetic throughout, Prime/PrimePi for prime enumeration,
   MultiplicativeOrder for ord_p, and a compact normalization step removing factors 2 and 5 before final summation.
*)

nCores = $ProcessorCount;
LaunchKernels[];

removeTwosAndFives[n_Integer?Positive] := Module[{m},
  m = n;
  While[Mod[m, 2] == 0, m /= 2];
  While[Mod[m, 5] == 0, m /= 5];
  m
]

isRepunitPowerOfTenFactor[p_Integer?PrimeQ] := Module[{ord},
  If[p == 2 || p == 5, Return[False]];
  ord = MultiplicativeOrder[10, 9 p];
  removeTwosAndFives[ord] == 1
]

DistributeDefinitions[removeTwosAndFives, isRepunitPowerOfTenFactor];

solve[] := Module[{limit, primes, flags, nonfactors},
  limit = 100000;
  primes = Prime[Range[PrimePi[limit - 1]]];
  flags = ParallelMap[isRepunitPowerOfTenFactor, primes];
  nonfactors = Pick[primes, flags, False];
  Total[nonfactors]
]

solve[]
