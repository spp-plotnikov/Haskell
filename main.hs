module Fib where

import Data.Function

fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
            | n == 1 = 1
            | n > 1 = helper 1 0 n
            | otherwise = (-1)^(1 - n) * fibonacci (-n)

helper cur pr n | n == 1 = cur
                | otherwise = helper (pr + cur) cur (n - 1)


seqA :: Integer -> Integer
seqA n 
	| n == 0 = 1
	| n == 1 = 2
	| n == 2 = 3
	| n >= 3  = let
			helper cur pr prpr n 
				| n == 2 = cur
				| otherwise = helper (cur + pr - 2 * prpr) cur pr (n - 1)
		in 
		 	helper 3 2 1 n
	| otherwise = error "not positive"


sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x
	| x == 0 = (0, 1)
	| otherwise = helper (abs x) (0, 0)
		where
			helper x (y, z)
				| x == 0 = (y, z)
				| otherwise = helper (x `div` 10) (y + x `mod` 10, z + 1)



class Printable a where
	toString :: a -> [Char]

instance Printable Bool where
	toString a = if a then "true" else "false"

instance Printable () where
	toString a = "unit type"