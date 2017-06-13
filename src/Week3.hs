module Week3 where

skips :: [a] -> [[a]]
-- skips a = skipsHelper a $ length a
skips a = map (every a) [1..(length a)]

every :: [a] -> int -> [a]
every a i = do
    let b = zipwith a [1..i]

everyHelper :: [(a,Int)] -> [a] -> int -> [a]
everyHelper (x,y) r i
    | x `mod` y == 0 = everyHelper r ++ [x]
