module Fib where

fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
            | n == 1 = 1
            | n > 1 = helper 1 0 n
            | otherwise = (-1)^(1 - n) * fibonacci (-n)

helper cur pr n | n == 1 = cur
                | otherwise = helper (pr + cur) cur (n - 1)