
-- List-Lambda-Micro-Haskell examples TPJ 2015


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


-- Lists

hd l = case l of [] -> Nothing; x:xs -> Just x ;

tl l = case l of [] -> Nothing; x:xs -> Just xs ;

nth n l =
  case l of
    [] -> (0-1) ;
    x : xs -> if n == 1 then x else nth (n-1) xs ;

from m = m: (from (m+1)) ;

fltr p l =
  case l of
    [] -> [] ;
    x : xs -> if p x then x : (fltr p xs)
                     else fltr p xs ;

-- Erastosthenes sieve

notdivides m n = neg (divides m n) ;

sieve l =
  case l of
    [] -> [] ;
    x : xs -> x : (sieve (fltr (\n -> neg (divides x n)) xs)) ;

primes = sieve (from 2) ;
