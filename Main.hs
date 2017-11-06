import qualified Entities.File as File
import qualified Entities.Track as Track
import qualified Entities.Current as Current
import qualified Entities.Album as Album

import Data.Aeson
import qualified Data.ByteString.Lazy as Lazybytes

type Album = Album.Album

main = Lazybytes.readFile "scheme.json" >>= 
	\scheme -> case (decode scheme :: Maybe Album) of
		Just album -> Album.download album
		Nothing -> print "Error: album.json is invalid..."