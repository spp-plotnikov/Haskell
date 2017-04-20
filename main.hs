import TabledFunctions
import Control.Exception
import Data.List 

test t = assert t $ return ()

main = do
  _ <- test (sort (fromFun (+1) [])      == sort (fromFun (+10) []))
  _ <- test (sort (fromFun (+1) [1..10]) == sort (fromFun (+1)  [1..10]))
  _ <- test (sort (fromFun (+1) [1..10]) /= sort (fromFun (+1)  [1..5]))
  _ <- test (sort (fromFun (+1) [1..10]) /= sort (fromFun (+10) [1..10]))
  _ <- let f = fromFun (\x -> x*2 + 30) [1..10] in
       test (sort f == sort (fromFun (\ x -> eval f x) [1..10]))
  _ <- let f = fromFun (\x -> x*2 + 30) [1..10] in
       test (sort f /= sort (fromFun (\ x -> 1 + eval f x) [1..10]))
  _ <- let f = fromFun (\x -> x*x) [-10..10] in
       test (sort [-10..10] == sort (dom f))
  _ <- test (sort (fromFun id [1..10]) == sort (invert (fromFun id [1..10])))
  _ <- test (sort (fromFun (\x -> x * x) [1..10] .*. fromFun id [1..10]) == sort (fromFun (\x -> x*x) [1..10]))

  _ <- let f x = x * 10 + 18 in
       let g x = x * x + x in
       let g' = fromFun g [1..100] in
       let f' = fromFun f (image g' [1..100]) in
       test (sort (fromFun (f . g) [1..100]) == sort (f' .*. g'))

  _ <- test (isInjective (fromFun (\x -> x*x + 4*x + 3) [1..100]))  
  _ <- test $ not (isInjective (fromFun (\x -> x*x + 4*x + 3) [-100..100]))  

  _ <- let f = fromFun (\x -> x*x*x+5*x*x) [1..100] in
       let g = invert f in
       test (sort (preimage f (image f [1..100])) == sort (image g (image f [1..100])))

  -- _ <- test (areMutuallyInverse (fromFun (+1) [1..200]) (fromFun (subtract 1) [1..100]))

  -- _ <- test $ not (areMutuallyInverse (fromFun (+1) [1..200]) (fromFun (+1) [1..100]))

  _ <- test (image undefined [] == [])

  _ <- test ([] == image (fromFun (+1) [1..10]) [100])
 
  putStrLn "All tests passed."
 