module Main (main) where

import First.Second (getFirstAndLastName)

main :: IO ()
main = 
  getFirstAndLastName >>= \tup -> putStrLn (show tup)
