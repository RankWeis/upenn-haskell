module Week1 where

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverseList (toDigits n)

reverseList :: [Integer] -> [Integer]
reverseList [] = []
reverseList (x:[]) = [x]
reverseList (x:xs) = reverseList xs ++ [x]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n = reverseList . doubleLoop 1 $ reverseList n

doubleLoop :: Integer -> [Integer] -> [Integer]
doubleLoop n [] = []
doubleLoop n (x:xs)
    |  n `mod` 2 == 0 = (x*2) : (doubleLoop (n+1) xs)
    |  otherwise  = x : (doubleLoop (n+1) xs)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:[]) = sumArray (toDigits x)
sumDigits (x:xs) = sumArray (toDigits x) + (sumDigits xs)

sumArray :: [Integer] -> Integer
sumArray [] = 0
sumArray (x:[]) = x
sumArray (x:xs) = x + (sumArray xs)

validate :: Integer -> Bool
validate n = luhn n `mod` 10 == 0

luhn :: Integer -> Integer
luhn n = sumDigits (doubleEveryOther (toDigits n))

