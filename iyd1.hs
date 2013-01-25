{-# LANGUAGE GADTs, StandaloneDeriving #-}
{-# LANGUAGE GADTs, ExistentialQuantification #-}
{-# LANGUAGE KindSignatures, DataKinds #-}
{-# LANGUAGE RankNTypes, EmptyDataDecls #-}

data CameraMake = 
    LeicaT
  | CanonT
  | NikonT
  | PanasonicT
    deriving Show

data NikonModel = 
    D90
    deriving Show

data PanasonicModel = 
    Gf1
  | Gf2
    deriving Show

data CanonModel = 
    EOS
  | Rebel
    deriving Show

data LeicaModel = 
    M3
  | M9
  | S2
    deriving Show

data Camera :: CameraMake -> * where
  Leica :: LeicaModel -> Camera LeicaT
  Canon :: CanonModel -> Camera CanonT
  Panasonic :: PanasonicModel -> Camera PanasonicT
  Nikon :: Int -> NikonModel -> Camera NikonT
deriving instance Show (Camera a)

showCamera (Leica m) = show m
showCamera (Canon m) = show m
showCamera (Nikon n m) = show m ++ show n

data Exif = Exif
    deriving Show

-- TODO: make it tagless
data Rep :: * -> * where
  RInt    :: Int -> Rep Int
  RChar   :: Int -> Rep Char
  RString :: [Char] -> Rep String
  ExifManufacturer :: String -> Rep Exif
  ExifModel        :: Rep String -> Rep Exif 
  ExifOrientation  :: Rep String -> Rep Exif
  ExifIsoSpeed     :: Rep Int -> Rep Exif
  ExifList         :: [Rep Exif] -> Rep [Exif]
deriving instance Show (Rep a)

showRep :: forall a. Rep a -> String
showRep (RInt i)    = show i
showRep (RChar c)   = show c
showRep (RString s) = show s
showRep (ExifManufacturer m) = showRep (RString m)
showRep (ExifModel m)        = showRep m
showRep (ExifOrientation m)  = showRep m
showRep (ExifIsoSpeed m)     = showRep m
showRep (ExifList (a:l))     = showRep a ++ showRep (ExifList l) 
showRep (ExifList [])        = ""

t = showRep (ExifList [ExifModel (RString "HI"), ExifIsoSpeed (RInt 3)])

