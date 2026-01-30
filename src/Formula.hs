{-# LANGUAGE InstanceSigs #-}

module Formula
  ( Formula (..),
    atoms,
  )
where

infixr 2 :&

infixr 3 :|

infixr 4 :->

infixr 5 :<->

data Formula a v
  = Const Bool
  | Atom (a v)
  | Not (Formula a v)
  | (Formula a v) :& (Formula a v)
  | (Formula a v) :| (Formula a v)
  | (Formula a v) :-> (Formula a v)
  | (Formula a v) :<-> (Formula a v)
  deriving (Eq)

instance (Show (a v)) => Show (Formula a v) where
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

instance (Functor f) => Functor (Formula f) where
  fmap :: (a -> b) -> Formula f a -> Formula f b
  fmap _ (Const b) = Const b
  fmap f (Atom a) = Atom (fmap f a)
  fmap f (Not a) = Not (fmap f a)
  fmap f (a :& b) = fmap f a :& fmap f b
  fmap f (a :| b) = fmap f a :| fmap f b
  fmap f (a :-> b) = fmap f a :-> fmap f b
  fmap f (a :<-> b) = fmap f a :<-> fmap f b

instance (Foldable f) => Foldable (Formula f) where
  foldr :: (a -> b -> b) -> b -> Formula f a -> b
  foldr _ z (Const _) = z
  foldr f z (Atom a) = foldr f z a
  foldr f z (Not a) = foldr f z a
  foldr f z (a :& b) = foldr f (foldr f z b) a
  foldr f z (a :| b) = foldr f (foldr f z b) a
  foldr f z (a :-> b) = foldr f (foldr f z b) a
  foldr f z (a :<-> b) = foldr f (foldr f z b) a

atoms :: Formula a v -> [a v]
atoms f = case f of
  (Const _) -> []
  (Atom a) -> [a]
  (Not a) -> atoms a
  (a :& b) -> atoms a ++ atoms b
  (a :| b) -> atoms a ++ atoms b
  (a :-> b) -> atoms a ++ atoms b
  (a :<-> b) -> atoms a ++ atoms b
