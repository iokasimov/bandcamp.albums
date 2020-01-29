module Data.Downloadable where

import "base" Control.Exception (try)
import "bytestring" Data.ByteString.Lazy (ByteString, writeFile)
import "http-client" Network.HTTP.Client (HttpException, Response)
import "joint" Control.Joint.Core (type (:=))
import "joint" Control.Joint.Abilities (Liftable (lift), (:>) (T))
import "joint" Control.Joint.Effects (Configured, Failable, Reader, get)
import "lens" Control.Lens (preview)

import qualified "bytestring" Data.ByteString.Lazy as Bytes (writeFile)
import qualified "wreq" Network.Wreq as HTTP (get, responseBody)

type Downloader = Reader FilePath :> Either HttpException :> IO

class Downloadable a where
	download :: a -> Downloader := ()

load :: (Monad t, Configured FilePath t, Liftable IO t, Failable HttpException t) => String -> (FilePath -> FilePath) -> String -> t ()
load link place title = request link >>= maybe failed (save place) . preview HTTP.responseBody where

	save :: (Monad t, Liftable IO t, Configured FilePath t) => (FilePath -> FilePath) -> ByteString -> t ()
	save path bytes = lift get >>= lift . flip Bytes.writeFile bytes . path

	-- request :: Liftable IO t => String -> t (Either HttpException (Response ByteString))
	request :: (Monad t, Liftable IO t, Failable HttpException t) => String -> t (Response ByteString)
	request link = lift (try @HttpException $ HTTP.get link) >>= lift

	failed :: Liftable IO t => t ()
	failed = lift . print $ "Failed downloading: " <> title
