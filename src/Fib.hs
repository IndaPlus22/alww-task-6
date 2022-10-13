module Fib
    ( fib
    ) where

fib :: Int -> Int
fib 0 = 0
fib n = last $ take n $ fibl
fibl = 1 : 1 : [a+b | (a,b) <- zip fibl (tail fibl)]