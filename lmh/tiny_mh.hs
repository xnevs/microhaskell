
-- Tiny lambda-Micro-Haskell program TPJ 2015

z :: Integer ;
z = 10 ;

neg :: Bool -> Bool;
neg b = if b then False else True ;

mnsdbl :: Integer -> Integer -> Integer ;
mnsdbl m n = m - n - n ;

once :: (Integer -> Integer) -> Integer -> Integer ;
once f x = f x ;

twice :: (Integer -> Integer) -> Integer -> Integer ;
twice f x = f (f x) ;
