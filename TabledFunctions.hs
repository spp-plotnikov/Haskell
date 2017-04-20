module TabledFunctions where

import Data.Tuple
import Data.List

-- Табличное представление функции типа Int -> Int --- это список пар 
-- (аргумент, значение) без повторений; гарантируется, что в этом списке нет 
-- нескольких пар с одинаковым первым элементом. 

-- Реализовать следующие функции:

-- fromFun f s возвращает табличное представление сужения f на s; предполагается,
-- что f определена в каждой точке s.
fromFun :: (Int -> Int) -> [Int] -> [(Int, Int)]
fromFun function newDom = [(arg, function arg) | arg <- newDom]

-- dom f по табличному представлению функции возвращает её область определения 
-- в виде списка без повторений.
dom :: [(Int, Int)] -> [Int]
dom function = [fst pair | pair <- function]

-- eval f x вычисляет значение таблично представленной функции для заданного
-- значения аргумента (если оно определено).
eval :: [(Int, Int)] -> Int -> Int
eval function arg = if arg `elem` dom function 
	                then head [snd pair | pair <- function, fst pair == arg] 
	                else error "not defined"

-- invert f по табличному представлению функции возвращает табличное представление
-- её обратного отображения (это отображение не обязано быть функцией)
invert :: [(Int, Int)] -> [(Int, Int)]
invert function = [swap pair | pair <- function]

-- f .*. g по табличным представлениям функций возвращает табличное представление
-- их суперпозиции
infixr 9 .*.
(.*.) :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
(.*.) function1 function2 = [(arg, eval function2 sndArg) | arg <- dom function1, sndArg <- dom (invert function1), sndArg == eval function1 arg]

-- image f s по табличному представлению функции и множеству целых чисел, представленному
-- в виде списка без повторений, возвращает образ этого множества (в виде списка
-- без повторений). Образ должен быть определен всегда.
image :: [(Int, Int)] -> [Int] -> [Int]
image function args = getSortedListWithOnlyOneOccurences [snd pair | pair <- function, fst pair `elem` args]

-- preimage f s вычисляет прообраз; агрументы и дополнительные условия --- как в
-- предыдущей задаче.
preimage :: [(Int, Int)] -> [Int] -> [Int]
preimage function args = [fst pair | pair <- function, snd pair `elem` args]

-- isInjective f возвращает True тогда и только тогда, когда таблично-представленная функция 
-- инъективна.
isInjective :: [(Int, Int)] -> Bool
isInjective = undefined

-- areMutuallyInverse f g возвращает True тогда и только тогда, когда таблично-представленные
-- функции f и g --- взаимно-обратны.
areMutuallyInverse :: [(Int, Int)] -> [(Int, Int)] -> Bool
areMutuallyInverse function1 function2 = sort function1 == sort (invert function2)


-- my function (for helping)
getSortedListWithOnlyOneOccurences list = [head item | item <- group (sort list)]