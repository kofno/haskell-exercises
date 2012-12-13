-- FizzBuzz impl.

fizzBuzz :: (Integral a, Show a) => [a] -> [[Char]]
fizzBuzz xs = [ fizzOrBuzz x | x <- xs ]
              where fizzOrBuzz n
                               | n `mod` 15 == 0 = "FizzBuzz"
                               | n `mod`  3 == 0 = "Fizz"
                               | n `mod`  5 == 0 = "Buzz"
                               | otherwise = show n