module Main (main) where

import PropFormula (tautology, Prop (Prop), interpretations)
import Formula (Formula(..), atoms)

main :: IO ()
main = do
  let formula = Atom (Prop "P") :& Atom (Prop "Q")
  print $ length $ interpretations (atoms formula)
  print $ tautology formula
