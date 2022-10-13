module Karpsravor (
    karpsravor
 ) where

karpsravor :: String -> String
karpsravor [] = []
karpsravor (x:xs)
  | x `elem` vok = x : karpsravor xs
  | otherwise = x : karpsravor (drop 2 xs)
    where vok = "aeiouyåäö"