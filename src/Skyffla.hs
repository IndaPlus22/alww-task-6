module Skyffla (
    skyffla
) where

everyother :: [x] -> [x]
everyother [] = []
everyother [x] = [x]
everyother (x:xs) = x:everyother (tail xs)

skyffla :: [x] -> [x]
skyffla [] = []
skyffla list@(x:xs) = everyother list ++ skyffla (everyother xs)