module Main where

import Term
import Term.Subst

main :: IO ()
main = do
  let identity = abstract "x" $ FreeVar "x"
  print (abstract "x" $ FreeVar "x")
  print $ instantiate identity (Int 1)

  let partConstant = abstract "y" $ FreeVar "x"
  print partConstant

  let constant = abstract "x" partConstant
  print constant
  let Just constant1 = instantiate constant (Int 1)
  print constant1
  print $ instantiate constant1 (Int 2)
