{-# LANGUAGE GADTs, StandaloneDeriving, KindSignatures #-}

data Field

data Exif :: * -> * where
  Manufacturer  :: Exif String
  Model         :: Exif String
  Orientation   :: Exif String
  IsoSpeed      :: Exif Int
  Field         :: (Show t) => Exif t -> t -> Exif Field 
deriving instance Show (Exif a)

-- ImageFile (an image file with Exif)
data ImageFile :: * -> * where
  ExifList  :: ImageFile [(Exif Field)] 
deriving instance Show (ImageFile a)

-- test
data Dynamic where 
  Dyn :: (Show t) => ImageFile t -> t -> Dynamic 
deriving instance Show Dynamic

f = Field Manufacturer "GF1"
flist = Dyn ExifList [f,f,f]


