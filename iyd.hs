{-# LANGUAGE GADTs, StandaloneDeriving, FlexibleInstances, KindSignatures, DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes, EmptyDataDecls #-}
{-# LANGUAGE PolyKinds #-}

--import GHC.TypeLits as TL 

data CameraMakes = -- < a value is ok, type is Make'
    Canon 
  | Nikon 
  | Panasonic 

data CameraModelsPanasonic = 
    Gf1
  | Gf2

data CameraModelsCanon = 
    EOS
  | Rebel
{--
data EOS
data Gf1
data Gf2
--}
type family CamFam (a :: CameraMakes) (b :: k)
type instance CamFam Canon Rebel = Int
type instance CamFam Panasonic (EOS :: CameraModelsCanon) = Char
type instance CamFam Panasonic (Gf1 :: CameraModelsPanasonic) = Int

data Camera :: CameraMakes -> * -> * where
  CPanasonic :: Camera Panasonic CameraModelsPanasonic --   :: CameraModelsPanasonic)
  CCanon     :: Camera Canon     CameraModelsCanon -- :: CameraModelsCanon)
{--
--}

data Property :: * where -- not a GADT
  PropSize   :: F BInt      -> Property 
--  PropCamera :: Camera a b  -> Property 
  PropModel  :: F BString   -> Property 
  
data Basic = 
    BInt
  | BChar
  | BString

type family F (a :: Basic) :: * 
type instance F BInt  = Int
type instance F BChar = Char
type instance F BString = String


data Dynamic = forall t. Dyn (Type t) t

data Type t where
  RInt    :: Type (F BInt)
  RChar   :: Type (F BChar)
  --RProp   :: Type Property
  RList   :: Type a -> Type [a]
  RDyn    :: Type Dynamic
