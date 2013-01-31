{-# LANGUAGE GADTs, StandaloneDeriving, KindSignatures #-}

data Field

data Exif :: * -> * where
  Manufacturer  :: Exif String
  Model         :: Exif String
  Orientation   :: Exif String
  IsoSpeed      :: Exif Int
  Field         :: (Show t) => Exif t -> t -> Exif Field 
deriving instance Show (Exif a)

type ExifProperties = [Exif Field]
type FileName = String
type FileDirectory = String

data SimpleFile where
  SimpleFileC :: FileName -> FileDirectory -> SimpleFile
  deriving Show 
 
data File :: * where -- don't seem to need a gadt/phantom
  ImageFile    :: SimpleFile -> ExifProperties -> File
  DefaultFile  :: SimpleFile -> File
  deriving Show 

{--
  type Files = [File]
  import :: Directory -> Files
  import d = 
    some pipe filter...

  query :: Files -> Query -> Files
  query fs q = fold fs q
--}

-- test
f = Field Manufacturer "GF1"
flist = [f,f,f]
simple = SimpleFileC "File" "Dir"
image = ImageFile simple flist
