module Data.Downloadable where

import "async" Control.Concurrent.Async (Concurrently)
import "base" Control.Exception (try)
import "base" Control.Monad (Monad ((>>=)))
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
import "joint" Control.Joint.Abilities.Transformer (type (:>), embed, build)
import "joint" Control.Joint.Effects.Reader (Configured, Reader, ask)
import "lens" Control.Lens (preview)
import "wreq" Network.Wreq (get, responseBody)

import "joint" Control.Joint.Abilities.Liftable (Liftable (lift))
import "joint" Control.Joint.Schemes ()

class Downloadable a where
	download :: a -> Reader FilePath :> Concurrently := ()

load :: (Monad t, Configured FilePath t, Liftable IO t) => String -> (FilePath -> FilePath) -> String -> t ()
load link place title = lift (request link) >>= either (lift . print)
	(maybe failed (save place) . preview responseBody) where

	save :: (Monad t, Liftable IO t, Configured FilePath t) => (FilePath -> FilePath) -> ByteString -> t ()
	save path bytes = lift ask >>= lift . flip writeFile bytes . path

	request :: String -> IO (Either HttpException (Response ByteString))
	request = try . get

	failed :: Liftable IO t => t ()
	failed = lift . print $ "Failed downloading: " <> title
