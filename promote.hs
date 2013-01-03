{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}

data Nat = Ze | Su Nat

data Vec :: * -> Nat -> * where
  Nil  :: Vec a Ze
  Cons :: a -> Vec a n -> Vec a (Su n)

data Type t where
  RInt    :: Type Int
  RChar   :: Type Char

data Test :: Type Int -> * where
  A :: Test (RInt)
