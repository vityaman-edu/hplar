{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module PropFormula
  ( Prop (..),
    PropFormula,
    PropLiteral,
    p,
    lit,
    lit',
    litNot,
    valuation,
    interpretations,
    tautology,
    unsatisfiable,
    satisfiable,
    (|=>),
  )
where

import Data.Bifunctor (second)
import Formula (Formula (..), atoms, flatMap)

newtype Prop v = Prop v
  deriving (Eq)

type PropFormula v = Formula Prop v

type PropLiteral v = (v, Bool)

p :: a -> PropFormula a
p = Atom . Prop

lit :: v -> PropLiteral v
lit v = (v, True)

lit' :: v -> PropLiteral v
lit' = litNot . lit

litNot :: PropLiteral v -> PropLiteral v
litNot = second not

instance Show (Prop String) where
  showsPrec :: Int -> Prop String -> ShowS
  showsPrec _ (Prop v) = showString v

instance {-# OVERLAPPABLE #-} (Show v) => Show (Prop v) where
  showsPrec :: Int -> Prop v -> ShowS
  showsPrec d (Prop v) = showsPrec d v

instance Functor Prop where
  fmap :: (a -> b) -> Prop a -> Prop b
  fmap f (Prop v) = Prop (f v)

instance Foldable Prop where
  foldr :: (a -> b -> b) -> b -> Prop a -> b
  foldr f z (Prop v) = f v z

valuation :: (a -> Bool) -> PropFormula a -> Bool
valuation interpretation formula = case formula of
  Atom (Prop v) -> interpretation v
  Not a -> not (valuation interpretation a)
  a :& b -> valuation interpretation a && valuation interpretation b
  a :| b -> valuation interpretation a || valuation interpretation b
  a :-> b -> not (valuation interpretation a) || valuation interpretation b

interpretations :: (Eq a) => [a] -> [a -> Bool]
interpretations propositions = case propositions of
  [] -> []
  [x] -> [(== x), (/= x)]
  (x : xs) ->
    concat
      [ [ \y -> (y == x) || i y,
          \y -> (y /= x) && i y
        ]
        | i <- interpretations xs
      ]

tautology :: (Eq a) => PropFormula a -> Bool
tautology f = and [valuation (i . Prop) f | i <- interpretations $ atoms f]

unsatisfiable :: (Eq a) => PropFormula a -> Bool
unsatisfiable f = tautology $ Not f

satisfiable :: (Eq a) => PropFormula a -> Bool
satisfiable f = not $ unsatisfiable f

infix 6 |=>

(|=>) :: (Eq a) => a -> PropFormula a -> PropFormula a -> PropFormula a
(|=>) old new = flatMap (\x -> if x == Prop old then new else Atom x)
