
-- Lambda-Micro-Haskell examples TPJ 2015


-- Logical negation

neg b = if b then False else True ;


-- Arithmetic functions

fib n = if n == 0 then 0
        else if n == 1 then 1
        else fib (n - 1) + fib (n - 2) ;  -- Inefficient!

times m n = if 0 < n then m + times m (n - 1)
            else if n < 0 then times m (n + 1) - m 
            else 0 ;

divides m n = if n == 0 then True
        else if n < m then False 
        else divides m (n - m);


-- Taking a function as argument

twice f z = f (f z) ;


-- Representing infinite lists as functions

hd l = l 1;

tl l n = l(n+1) ;

cons n l m = if m == 1 then n else l(m - 1) ;

from m n = m + n - 1 ;


-- Filtering an infinite list

fltr p l n =
  if (p (hd l))
    then if n==1 then hd l
         else fltr p (tl l) (n - 1)
    else fltr p (tl l) n ;


-- Erastosthenes sieve

notdivides m n = neg (divides m n) ;

sieve l = cons (hd l) (sieve (fltr (notdivides (hd l)) (tl l))) ;

primes = sieve (from 2) ;

-- Warning, this is *very* slow!!!
-- One reason for this is it recomputes "head l" multiple times
-- An efficient implementation would avoid this repetition
