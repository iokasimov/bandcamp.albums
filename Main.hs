import "aeson" Data.Aeson (decode)
import "base" Control.Applicative (pure)
import "base" Control.Monad ((>>=))
import "base" Data.Bool (Bool (True))
import "base" Data.Functor ((<$>))
import "base" Data.Maybe (Maybe (Just, Nothing))
import "base" Data.Monoid ((<>))
import "base" System.IO (FilePath, IO, print)
import "bytestring" Data.ByteString.Lazy (readFile)
import "directory" System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import "filepath" System.FilePath.Posix ((</>))
import "transformers" Control.Monad.Trans.Reader (ReaderT (runReaderT))

import Data.Downloadable (download)
import Data.Bandcamp.Album (Album (..))
import Data.Bandcamp.Cover (Cover (..))
import Data.Bandcamp.Current (Current (..))
import Data.Bandcamp.Filename (Filename (..))
import Data.Bandcamp.Track (Track (..))

prepare_directory :: Album -> IO FilePath
prepare_directory a@(Album (Current album) ts artist aid') = do
	current_dir <- getCurrentDirectory
	let dir = current_dir </> "Temporary" </> artist </> album
	createDirectoryIfMissing True dir
	pure dir

main = decode @Album <$> readFile "scheme.json" >>= \case
	Nothing -> print "Error: album.json is invalid..."
	Just album -> prepare_directory album >>= runReaderT (download album)
