{-# LANGUAGE InstanceSigs #-}

module Formula
  ( Formula (..),
    atoms,
    flatMap,
    nnf,
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

flatMap :: (a b -> Formula a c) -> Formula a b -> Formula a c
flatMap transform formula = case formula of
  (Const a) -> Const a
  (Atom a) -> transform a
  (Not a) -> Not (fmt a)
  (a :& b) -> fmt a :& fmt b
  (a :| b) -> fmt a :| fmt b
  (a :-> b) -> fmt a :-> fmt b
  (a :<-> b) -> fmt a :<-> fmt b
  where
    fmt = flatMap transform

nnf :: Formula a v -> Formula a v
nnf = nnf' . simplify
  where
    simplify' :: Formula a v -> Formula a v
    simplify' f = case f of
      (Not (Const False)) -> Const True
      (Not (Const True)) -> Const False
      (Not (Not p)) -> p
      (_ :& Const False) -> Const False
      (Const False :& _) -> Const False
      (p :& Const True) -> p
      (Const True :& q) -> q
      (p :| Const False) -> p
      (Const False :| q) -> q
      (_ :| Const True) -> Const True
      (Const True :| _) -> Const True
      (Const False :-> _) -> Const True
      (Const True :-> p) -> p
      (_ :-> Const True) -> Const True
      (p :-> Const False) -> Not p
      (p :<-> Const True) -> p
      (Const True :<-> q) -> q
      (p :<-> Const False) -> Not p
      (Const False :<-> q) -> Not q
      x -> x

    simplify :: Formula a v -> Formula a v
    simplify f = case f of
      (Const x) -> Const x
      (Atom x) -> Atom x
      Not p -> simplify' (Not $ simplify p)
      (p :& q) -> simplify' (simplify p :& simplify q)
      (p :| q) -> simplify' (simplify p :| simplify q)
      (p :-> q) -> simplify' (simplify p :-> simplify q)
      (p :<-> q) -> simplify' (simplify p :<-> simplify q)

    nnf' :: Formula a v -> Formula a v
    nnf' f = case f of
      Const x -> Const x
      Atom x -> Atom x
      Not (Const x) -> Not $ Const x
      Not (Atom x) -> Not $ Atom x
      Not (Not x) -> nnf' x
      Not (x :& y) -> nnf' (Not x) :| nnf' (Not y)
      Not (x :| y) -> nnf' (Not x) :& nnf' (Not y)
      Not (x :-> y) -> nnf' x :& nnf' (Not y)
      Not (x :<-> y) -> (nnf' x :& nnf' (Not y)) :| (nnf' (Not x) :& nnf' y)
      x :& y -> nnf' x :& nnf' y
      x :| y -> nnf' x :| nnf' y
      x :-> y -> nnf' (Not x) :| nnf' y
      x :<-> y -> (nnf' x :& nnf' y) :| (nnf' (Not x) :& nnf' (Not y))
