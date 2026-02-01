module DPLL
  ( unitPropagate,
    pureEliminate,
  )
where

import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import PropCNF (PropCNF (..))
import PropFormula (litNot)

unitPropagate :: (Eq v) => PropCNF v -> PropCNF v
unitPropagate (PropCNF clauses) =
  fromMaybe (PropCNF clauses) $ do
    [u] <- find isSingleton clauses
    let u' = litNot u
        clauses' = filter (notElem u) clauses
        clauses'' = fmap (filter (/= u')) clauses'
    return $ unitPropagate (PropCNF clauses'')
  where
    isSingleton :: [a] -> Bool
    isSingleton [_] = True
    isSingleton _ = False

pureEliminate :: (Eq v) => PropCNF v -> PropCNF v
pureEliminate (PropCNF clauses) =
  undefined
