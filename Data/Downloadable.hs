module Data.Downloadable where

import "async" Control.Concurrent.Async (Concurrently)
import "base" Control.Exception (try)
import "base" Control.Monad ((>>=))
import "base" Data.Maybe (maybe)
import "base" Data.Monoid ((<>))
import "base" Data.String (String)
import "base" Data.Function ((.), ($), flip)
import "base" Data.Either (Either, either)
import "base" System.IO (IO, print)
import "bytestring" Data.ByteString.Lazy (ByteString, writeFile)
import "filepath" System.FilePath.Posix (FilePath)
import "http-client" Network.HTTP.Client (HttpException, Response)
import "joint" Control.Joint.Core (type (:=))
import "joint" Control.Joint.Transformer (type (:>), embed, build)
import "joint" Control.Joint.Base.Reader (Reader, ask)
import "lens" Control.Lens (preview)
import "wreq" Network.Wreq (get, responseBody)

class Downloadable a where
	download :: a -> Reader FilePath :> Concurrently := ()

load :: String -> (FilePath -> FilePath) -> String -> Reader FilePath :> IO := ()
load link place title = embed (request link) >>= either (embed . print)
	(maybe failed (save place) . preview responseBody) where

	save :: (FilePath -> FilePath) -> ByteString -> Reader FilePath :> IO := ()
	save path bytes = build ask >>= embed . flip writeFile bytes . path

	request :: String -> IO (Either HttpException (Response ByteString))
	request = try . get

	failed :: Reader FilePath :> IO := ()
	failed = embed . print $ "Failed downloading: " <> title
