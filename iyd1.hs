{-# LANGUAGE GADTs, StandaloneDeriving, KindSignatures #-}
import System.Environment
import System.Directory
import System.FilePath.Find

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
 
search pat dir = find always (fileName ~~? pat) dir
 
main = do [pat] <- getArgs
          dir   <- getCurrentDirectory
          files <- search pat dir
          mapM_ putStrLn files


-- test
f = Field Manufacturer "GF1"
flist = [f,f,f]
simple = SimpleFileC "File" "Dir"
image = ImageFile simple flist
