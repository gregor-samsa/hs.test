{-# LANGUAGE GADTs, StandaloneDeriving, KindSignatures #-}

data Exif :: * -> * where
  Manufacturer  :: Exif String
  Model         :: Exif String
  Orientation   :: Exif String
  IsoSpeed      :: Exif Int
  Field         :: Exif Dynamic

type ExifFieldList = [Exif Dynamic]

-- TRep - tagless representation type
data TRep :: * -> * where
  TExifList  :: TRep  ExifFieldList -- check
deriving instance Show (TRep a)

data Dynamic where 
  Dyn :: TRep t -> t -> Dynamic 
