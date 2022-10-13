module Medellangd (
    medellangd
) where
import Data.Char
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