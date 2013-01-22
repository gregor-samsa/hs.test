{-# LANGUAGE GADTs, StandaloneDeriving #-}
{-# LANGUAGE KindSignatures, DataKinds #-}

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

data Exif = Exif
    deriving Show

data Rep :: * -> * where
  RInt  :: Int -> Rep Int
  RChar :: Int -> Rep Char
  ExifManufacturer :: String -> Rep Exif
  ExifModel        :: String -> Rep Exif 
  ExifOrientation  :: String -> Rep Exif
  ExifIsoSpeed     :: Int -> Rep Exif
  ExifList         :: [Rep Exif] -> Rep [Exif]
deriving instance Show (Rep a)


showCamera (Leica m) = show m
showCamera (Canon m) = show m
showCamera (Nikon n m) = show m ++ show n

