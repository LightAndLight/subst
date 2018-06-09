module Main where

import Term
import Term.Subst

main :: IO ()
main = do
  print (abstract "x" $ FreeVar "x")
