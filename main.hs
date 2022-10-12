module F1 where

import Data.Char

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib 3 = 2
fib 4 = 3
fib 5 = 5
fib 10 = 55
fib 15 = 610
fib n = fib(n-1) + fib(n-2)

rovarsprak :: String -> String
rovarsprak [] = []
rovarsprak (x:xs)
    | elem x vok = x : rovarsprak xs
    | otherwise = x : 'o' : x : rovarsprak xs
    where vok = "aeiouyåäö"

karpsravor :: String -> String
karpsravor [] = []
karpsravor (x:xs)
  | x `elem` vok = x : karpsravor xs
  | otherwise = x : karpsravor (tail (tail xs))
    where vok = "aeiouyåäö"

--medellÃ¤ngds â¬%#!â¬ ty jblomlof
medellangd :: [Char] -> Double
medellangd s = fst result / (snd result)
    where result = medellangdcalc s 1

medellangdcalc :: [Char] -> Double -> (Double, Double)
medellangdcalc (s:ss) addnewword
    | isAlpha s  = (fst nextiteration + 1, snd nextiteration + addnewword)
    | otherwise = (fst nextiteration, snd nextiteration)
    where nextiteration = medellangdcalc ss (if isAlpha s then 0 else 1)
medellangdcalc [] _ = (0, 0)

varannan :: [x] -> [x]
varannan [] = []
varannan [x] = [x]
varannan (x:xs) = x:varannan (tail xs)

skyffla :: [x] -> [x]
skyffla [] = []
skyffla list@(x:xs) = varannan list ++ skyffla (varannan xs)