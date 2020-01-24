module Data.Downloadable where

import "base" Control.Exception (try)
import "base" Control.Monad (Monad ((>>=)))
import "base" Data.Maybe (maybe)
import "base" Data.Monoid ((<>))
import "base" Data.String (String)
import "base" Data.Functor (Functor (fmap), (<$>))
import "base" Data.Function ((.), ($), flip)
import "base" Data.Either (Either, either)
import "base" System.IO (IO, print)
import "bytestring" Data.ByteString.Lazy (ByteString, writeFile)
import "filepath" System.FilePath.Posix (FilePath)
import "http-client" Network.HTTP.Client (HttpException, Response)
import "joint" Control.Joint.Core (type (:=))
import "joint" Control.Joint.Abilities (Liftable (lift), (:>) (T))
import "joint" Control.Joint.Effects (Configured, Failable, Reader, get)
import "lens" Control.Lens (preview)

import qualified "wreq" Network.Wreq as HTTP (get, responseBody)

type Downloader = Reader FilePath :> IO

class Downloadable a where
	download :: a -> Downloader := ()

load :: (Monad t, Configured FilePath t, Liftable IO t) => String -> (FilePath -> FilePath) -> String -> t ()
load link place title = (request link) >>= either (lift . print) (maybe failed (save place) . preview HTTP.responseBody) where

	save :: (Monad t, Liftable IO t, Configured FilePath t) => (FilePath -> FilePath) -> ByteString -> t ()
	save path bytes = lift get >>= lift . flip writeFile bytes . path

	request :: Liftable IO t => String -> t (Either HttpException (Response ByteString))
	request = lift . try . HTTP.get

	failed :: Liftable IO t => t ()
	failed = lift . print $ "Failed downloading: " <> title
