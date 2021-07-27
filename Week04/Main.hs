module Main where

main :: IO ()
main = bar

bar :: IO ()
bar = getLine >>= \s -> 
      getLine >>= \t ->
      putStrLn $  s ++ t