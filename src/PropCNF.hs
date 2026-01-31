{-# LANGUAGE InstanceSigs #-}

module PropCNF
  ( PropCNF(..),
    tseitinCNF,
  )
where

import Formula (Formula (..), atoms)
import PropFormula (Prop (Prop), PropFormula, PropLiteral, lit, lit', litNot)

newtype PropCNF v = PropCNF [[PropLiteral v]]

instance (Show v) => Show (PropCNF v) where
  showsPrec :: Int -> PropCNF v -> ShowS
  showsPrec _ (PropCNF cnf) = showsCNF cnf
    where
      showsLit :: (Show v) => PropLiteral v -> ShowS
      showsLit (v, True) = shows v
      showsLit (v, False) = showString "!" . shows v

      showsCNF :: (Show v) => [[PropLiteral v]] -> ShowS
      showsCNF [] = showString "T"
      showsCNF [x] = showString "(" . showsClause x . showString ")"
      showsCNF (x : xs) = showString "(" . showsClause x . showString ") & " . showsCNF xs

      showsClause :: (Show v) => [PropLiteral v] -> ShowS
      showsClause [] = showString "_|_"
      showsClause [x] = showsLit x
      showsClause (x : xs) = showsLit x . showString " | " . showsClause xs

tseitinCNF :: (Ord v, Enum v) => PropFormula v -> PropCNF v
tseitinCNF formula =
  let max' = succ $ maximum $ (\(Prop x) -> x) <$> atoms formula
      (l, PropCNF delta, _) = cnf' (formula, PropCNF [], max')
   in PropCNF $ [l] : delta
  where
    cnf' ::
      (Ord v, Enum v) =>
      (PropFormula v, PropCNF v, v) ->
      (PropLiteral v, PropCNF v, v)
    cnf' (f, delta, new) = case f of
      Atom (Prop x) -> (lit x, delta, new)
      Not p -> (litNot l, delta', new')
        where
          (l, delta', new') = cnf' (p, delta, new)
      p :& q -> (lit new', PropCNF delta', succ new')
        where
          (l1, delta1, new1) = cnf' (p, delta, new)
          (l2, PropCNF delta2, new') = cnf' (q, delta1, new1)
          delta' =
            [lit' new', l1]
              : [lit' new', l2]
              : [litNot l1, litNot l2, lit new']
              : delta2
      p :| q -> (lit new', PropCNF delta', succ new')
        where
          (l1, delta1, new1) = cnf' (p, delta, new)
          (l2, PropCNF delta2, new') = cnf' (q, delta1, new1)
          delta' =
            [litNot l1, lit new']
              : [litNot l2, lit new']
              : [lit' new', l1, l2]
              : delta2
      p :-> q -> cnf' (Not p :| q, delta, new)
