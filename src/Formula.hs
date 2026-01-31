{-# LANGUAGE InstanceSigs #-}

module Formula
  ( Formula (..),
    atoms,
    flatMap,
    simplify,
    nnf,
  )
where

infixr 2 :&

infixr 3 :|

infixr 4 :->

data Formula a v
  = Atom (a v)
  | Not (Formula a v)
  | (Formula a v) :& (Formula a v)
  | (Formula a v) :| (Formula a v)
  | (Formula a v) :-> (Formula a v)
  deriving (Eq)

instance (Show (a v)) => Show (Formula a v) where
  showsPrec :: Int -> Formula a v -> ShowS
  showsPrec d f = case f of
    Atom a -> showsPrec d a
    Not a -> showString "!" . showsPrec 11 a
    a :& b -> showParen (d > 6) $ showsPrec 6 a . showString " & " . showsPrec 7 b
    a :| b -> showParen (d > 5) $ showsPrec 5 a . showString " | " . showsPrec 6 b
    a :-> b -> showParen (d > 4) $ showsPrec 5 a . showString " -> " . showsPrec 4 b

instance (Functor f) => Functor (Formula f) where
  fmap :: (a -> b) -> Formula f a -> Formula f b
  fmap f (Atom a) = Atom (fmap f a)
  fmap f (Not a) = Not (fmap f a)
  fmap f (a :& b) = fmap f a :& fmap f b
  fmap f (a :| b) = fmap f a :| fmap f b
  fmap f (a :-> b) = fmap f a :-> fmap f b

instance (Foldable f) => Foldable (Formula f) where
  foldr :: (a -> b -> b) -> b -> Formula f a -> b
  foldr f z (Atom a) = foldr f z a
  foldr f z (Not a) = foldr f z a
  foldr f z (a :& b) = foldr f (foldr f z b) a
  foldr f z (a :| b) = foldr f (foldr f z b) a
  foldr f z (a :-> b) = foldr f (foldr f z b) a

atoms :: Formula a v -> [a v]
atoms f = case f of
  (Atom a) -> [a]
  (Not a) -> atoms a
  (a :& b) -> atoms a ++ atoms b
  (a :| b) -> atoms a ++ atoms b
  (a :-> b) -> atoms a ++ atoms b

flatMap :: (a b -> Formula a c) -> Formula a b -> Formula a c
flatMap transform formula = case formula of
  (Atom a) -> transform a
  (Not a) -> Not (fmt a)
  (a :& b) -> fmt a :& fmt b
  (a :| b) -> fmt a :| fmt b
  (a :-> b) -> fmt a :-> fmt b
  where
    fmt = flatMap transform

simplify :: Formula a v -> Formula a v
simplify f = case f of
  (Atom x) -> Atom x
  Not p -> simplify' (Not $ simplify p)
  (p :& q) -> simplify' (simplify p :& simplify q)
  (p :| q) -> simplify' (simplify p :| simplify q)
  (p :-> q) -> simplify' (simplify p :-> simplify q)
  where
    simplify' :: Formula a v -> Formula a v
    simplify' f' = case f' of
      (Not (Not p)) -> p
      x -> x

nnf :: Formula a v -> Formula a v
nnf = nnf' . simplify
  where
    nnf' :: Formula a v -> Formula a v
    nnf' f = case f of
      Atom x -> Atom x
      Not (Atom x) -> Not $ Atom x
      Not (Not x) -> nnf' x
      Not (x :& y) -> nnf' (Not x) :| nnf' (Not y)
      Not (x :| y) -> nnf' (Not x) :& nnf' (Not y)
      Not (x :-> y) -> nnf' x :& nnf' (Not y)
      x :& y -> nnf' x :& nnf' y
      x :| y -> nnf' x :| nnf' y
      x :-> y -> nnf' (Not x) :| nnf' y
