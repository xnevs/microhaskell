n :: Integer ;
n = 42 ;

plus :: Integer -> Integer -> Integer ;
plus x y = x + y ;

rpt :: Integer -> (Integer -> Integer) -> Integer -> Integer ;
rpt m f n = if m == 0 then n
            else rpt (m-1) f (f n) ;
