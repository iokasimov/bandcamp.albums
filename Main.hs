import Prelude hiding (readFile)

import Data.Aeson (decode)
import Data.ByteString.Lazy (readFile)

import Entities.Album (Album, download)

main = decode @Album <$> readFile "scheme.json" >>= maybe
	(print "Error: album.json is invalid...") download
