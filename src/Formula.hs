{-# LANGUAGE InstanceSigs #-}

module Formula
  ( Formula (..),
  )
where

infixr 2 :&

infixr 3 :|

infixr 4 :->

infixr 5 :<->

data Formula a v
  = Const Bool
  | Atom a
  | Not (Formula a v)
  | (Formula a v) :& (Formula a v)
  | (Formula a v) :| (Formula a v)
  | (Formula a v) :-> (Formula a v)
  | (Formula a v) :<-> (Formula a v)

instance (Show a) => Show (Formula a v) where
  showsPrec :: Int -> Formula a v -> ShowS
  showsPrec d f = case f of
    Const False -> showString "F"
    Const True -> showString "T"
    Atom a -> showsPrec d a
    Not a -> showString "!" . showsPrec 11 a
    a :& b -> showParen (d > 6) $ showsPrec 6 a . showString " & " . showsPrec 7 b
    a :| b -> showParen (d > 5) $ showsPrec 5 a . showString " | " . showsPrec 6 b
    a :-> b -> showParen (d > 4) $ showsPrec 5 a . showString " -> " . showsPrec 4 b
    a :<-> b -> showParen (d > 3) $ showsPrec 4 a . showString " <-> " . showsPrec 4 b
