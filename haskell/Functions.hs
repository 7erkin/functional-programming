module Functions where

    import Data.Char

    discount :: Double -> Double -> Double -> Double
    discount limit proc sum = if sum >= limit then sum * (100 - proc) / 100 else sum

    twoDigits2Int :: Char -> Char -> Int
    twoDigits2Int x y = if (isDigit x && isDigit y) then (10 * digitToInt x) + digitToInt y else 100

    dist :: (Double, Double) -> (Double, Double) -> Double
    dist point1 point2 =  sqrt $ (fst point1 - fst point2) ^ 2 + (snd point1 - snd point2) ^ 2

    factorial n = if n == 0 then 1 else n * factorial(n - 1)

    factorial' 0 = 1
    factorial' n = n * factorial' (n - 1)

    doubleFact :: Integer -> Integer
    doubleFact n = if n <= 0 then 1 else n * doubleFact(n - 2)

    -- pattern matching
    doubleFact' :: Integer -> Integer
    doubleFact' (-1) = 1
    doubleFact' 0 = 1
    doubleFact' n = n * doubleFact' (n - 1)

    -- guards
    fibonacci :: Integer -> Integer
    fibonacci n | n == 0 || n == 1 || n == -1 = abs(1)
                | n == -2 = -1
                | n > 0 = fibonacci(n - 1) + fibonacci(n - 2)
                | n < 0 = fibonacci(n + 2) - fibonacci(n + 1)
