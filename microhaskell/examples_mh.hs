
-- Micro-Haskell examples TPJ 2015


-- Logical negation

neg :: Bool -> Bool ;
neg b = if b then False else True ;


-- Arithmetic functions

fib :: Integer -> Integer ;
fib n = if n == 0 then 0
        else if n == 1 then 1
        else fib (n - 1) + fib (n - 2) ;  -- Inefficient!

times :: Integer -> Integer -> Integer ;
times m n = if 0 < n then m + times m (n - 1)
            else if n < 0 then times m (n + 1) - m 
            else 0 ;

divides :: Integer -> Integer -> Bool ;
divides m n = if n == 0 then True
        else if n < m then False 
        else divides m (n - m);


-- Taking a function as argument

twice :: (Integer -> Integer) -> Integer -> Integer ;
twice f n = f (f n) ;


-- Representing infinite lists as functions

hd :: (Integer -> Integer) -> Integer ;
hd l = l 1;

tl :: (Integer -> Integer) -> (Integer -> Integer) ;
tl l n = l(n+1) ;

cons :: Integer -> (Integer -> Integer) -> (Integer -> Integer) ;
cons n l m = if m == 1 then n else l(m - 1) ;

from :: Integer -> (Integer -> Integer) ;
from m n = m + n - 1 ;


-- Filtering an infinite list

fltr :: (Integer -> Bool) -> (Integer -> Integer) -> (Integer -> Integer) ;
fltr p l n =
  if (p (hd l))
    then if n==1 then hd l
         else fltr p (tl l) (n - 1)
    else fltr p (tl l) n ;


-- Erastosthenes sieve

notdivides :: Integer -> Integer -> Bool ;
notdivides m n = neg (divides m n) ;

sieve :: (Integer -> Integer) -> (Integer -> Integer) ;
sieve l = cons (hd l) (sieve (fltr (notdivides (hd l)) (tl l))) ;

primes :: Integer -> Integer ;
primes = sieve (from 2) ;

-- Warning, this is *very* slow!!!
-- One reason for this is it recomputes "head l" multiple times
-- An efficient implementation would avoid this repetition
