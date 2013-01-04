{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

data Basica = 
    BInt
  | BChar

type family F (a :: Basica) :: * 
type instance F BInt  = Int
type instance F BChar = Char

-- data TestF :: F * -> * where -- doesn't work, type synonyms can't be promoted
data TestF :: * -> * where
  TInt  :: TestF (F BInt)
  TChar :: TestF (F BChar)

data Nat = Ze | Su Nat

data Vec :: * -> Nat -> * where
  Nil  :: Vec a Ze
  Cons :: a -> Vec a n -> Vec a (Su n)

data Type t where
  RInt    :: Type Int
  RChar   :: Type Char

data Test :: Type Int -> * where
  A :: Test (RInt)
  B :: Test (RInt)

data Adt =
    One
  | Two
  | Three 

type SInt = Int
{-- compile error
      `SInt' of kind `*' is not promotable

data TestAdt1 :: SInt -> * where 
  Ab :: TestAdt1 (SInt)

--}

data TestAdt :: Adt -> * where
  Aa :: TestAdt (One)
  Bb :: TestAdt (Two)
  Cc :: TestAdt (Three)
