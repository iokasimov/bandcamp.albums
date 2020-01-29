import "aeson" Data.Aeson (decode)
import "directory" System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import "filepath" System.FilePath.Posix ((</>))
import "joint" Control.Joint.Abilities.Composition (run)
import "tagged" Data.Tagged (untag)

import qualified "bytestring" Data.ByteString.Lazy as Bytes (readFile)

import Data.Downloadable (download)
import Data.Bandcamp.Album (Album (Album))

prepare_directory :: Album -> IO FilePath
prepare_directory a@(Album title ts artist aid') = do
	current_dir <- getCurrentDirectory
	let dir = current_dir </> "Temporary" </> artist </> untag title
	createDirectoryIfMissing True dir
	pure dir

main = decode @Album <$> Bytes.readFile "scheme.json" >>= \case
	Nothing -> print "Error: album.json is invalid..."
	Just album -> prepare_directory album >>= run . run (download album) >>= either print print
