{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module PropFormula
  ( Prop (..),
    PropFormula,
  )
where

import Formula (Formula)

newtype Prop v = Prop v
  deriving (Eq)

type PropFormula v = Formula Prop v

instance Show (Prop String) where
  showsPrec :: Int -> Prop String -> ShowS
  showsPrec _ (Prop v) = showString v

instance {-# OVERLAPPABLE #-} (Show v) => Show (Prop v) where
  showsPrec :: Int -> Prop v -> ShowS
  showsPrec d (Prop v) = showsPrec d v

instance Functor Prop where
  fmap :: (a -> b) -> Prop a -> Prop b
  fmap f (Prop v) = Prop (f v)
