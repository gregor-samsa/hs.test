{-# LANGUAGE GADTs, StandaloneDeriving, KindSignatures #-}
import System.Environment
import System.Directory
import System.FilePath.Find
import System.FilePath.Posix as FilePath
import qualified Graphics.Exif as Ex

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
type FileExtension = String
type FileDirectory = String

data SimpleFile where
  SimpleFileC :: FileName -> FileDirectory -> FileExtension -> SimpleFile
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

fromExif :: (String, String) -> ExifProperties -> ExifProperties
fromExif ("Manufacturer", v) l = Field Manufacturer v : l
fromExif ("Model", v) l        = Field Model v : l
fromExif ("Orientation", v) l  = Field Orientation v : l
fromExif ("IsoSpeed", v) l     = Field Manufacturer v : l  -- FIXME
fromExif (_, v) l              = l


mkSimpleFile f = SimpleFileC name path ext
  where 
    (path, name) = FilePath.splitFileName f 
    ext = FilePath.takeExtension f 

mkFile ".jpg" f = do
  exif <- Ex.fromFile f
  exiftags <- Ex.allTags exif
  let fields = foldr fromExif [] exiftags
  let sf = mkSimpleFile f
  return $ ImageFile sf fields

mkFile ext f = do
  let (path, name) = FilePath.splitFileName f 
  return $ DefaultFile $ mkSimpleFile f

printFileMeta f = do
  defaultf <- mkFile (FilePath.takeExtension f) f
  putStrLn (show defaultf)
 
main = do [pat] <- getArgs
          dir   <- getCurrentDirectory
          files <- search pat dir
          mapM_ printFileMeta files

-- test
test = withArgs ["*"] main
