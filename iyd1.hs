{-# LANGUAGE GADTs, StandaloneDeriving, KindSignatures #-}

data Exif1 :: * -> * where
  Manufacturer  :: Exif1 String
  Model         :: Exif1 String
  Orientation   :: Exif1 String
  IsoSpeed      :: Exif1 Int
  Field         :: Exif1 Dynamic

type ExifFieldList = [Exif1 Dynamic]

-- TRep - tagless representation type
data TRep :: * -> * where
  TExifList  :: TRep  ExifFieldList -- check
deriving instance Show (TRep a)

data Dynamic where 
  Dyn :: TRep t -> t -> Dynamic 
