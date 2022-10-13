module Rovarspark (
    rovarsprak
) where

rovarsprak :: String -> String
rovarsprak [] = []
rovarsprak (x:xs)
    | elem x vok = x : rovarsprak xs
    | otherwise = x : 'o' : x : rovarsprak xs
    where vok = "aeiouyåäö"