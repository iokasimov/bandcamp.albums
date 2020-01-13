import "aeson" Data.Aeson (decode)
import "async" Control.Concurrent.Async (runConcurrently)
import "base" Control.Applicative (pure)
import "base" Control.Monad ((>>=))
import "base" Data.Bool (Bool (True))
import "base" Data.Function ((.))
import "base" Data.Functor ((<$>))
import "base" Data.Maybe (Maybe (Just, Nothing))
import "base" Data.Monoid ((<>))
import "base" System.IO (FilePath, IO, print)
import "bytestring" Data.ByteString.Lazy (readFile)
import "directory" System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import "filepath" System.FilePath.Posix ((</>))
import "joint" Control.Joint.Abilities.Composition (run)
import "tagged" Data.Tagged (untag)

import Data.Downloadable (download)
import Data.Bandcamp.Album (Album (..))
import Data.Bandcamp.Cover (Cover (..))
import Data.Bandcamp.Filename (Filename (..))
import Data.Bandcamp.Track (Track (..))

prepare_directory :: Album -> IO FilePath
prepare_directory a@(Album title ts artist aid') = do
	current_dir <- getCurrentDirectory
	let dir = current_dir </> "Temporary" </> artist </> untag title
	createDirectoryIfMissing True dir
	pure dir

main = decode @Album <$> readFile "scheme.json" >>= \case
	Nothing -> print "Error: album.json is invalid..."
	Just album -> prepare_directory album >>= runConcurrently . run (download album)
