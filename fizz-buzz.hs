-- FizzBuzz impl.

fizzBuzz :: (Integral a, Show a) => [a] -> [[Char]]
fizzBuzz xs = [ fizzOrBuzz x | x <- xs ]
              where fizzOrBuzz n
                               | n `mod` 15 == 0 = "FizzBuzz"
                               | n `mod`  3 == 0 = "Fizz"
                               | n `mod`  5 == 0 = "Buzz"
                               | otherwise = show n

-- Sexy. No modulo or guards. Also, no parameters
fizzBuzz' :: [String]
fizzBuzz' = zipWith3 buzzer threes fives [1..]
    where buzzer "" "" n = show n
          buzzer s1 s2 _ = s1 ++ s2
          threes = cycle ["", "", "Fizz"]
          fives  = cycle ["", "", "", "", "Buzz"]

-- Example usages of fizzBuzz'

-- FizzBuzz one through fifteen
-- putStrLn . unlines . take 15 $ fizzBuzz'

-- FizzBuzz 26 - 50
-- putStrLn . unlines . drop 25 . take 50 $ fizzBuzz'

-- or take the 15 numbers after 30
-- putStrLn . unlines . take 15 . drop 30 $ fizzBuzz' 

