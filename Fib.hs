module Fib (fib) where

fib :: Int
fib = rabbits 5 3

rabbits :: Int -> Int -> Int
rabbits 1 _ = 1
rabbits 2 _ = 1
rabbits n k  =
  rabbits (n - 2) k * k + rabbits (n - 1) k
