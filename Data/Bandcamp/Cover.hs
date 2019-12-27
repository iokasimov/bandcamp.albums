module Data.Bandcamp.Cover where

import "aeson" Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import "async" Control.Concurrent.Async (Concurrently (Concurrently))
import "base" Control.Applicative ((<*>))
import "base" Control.Monad ((>>=))
import "base" Control.Exception (try)
import "base" Data.Either (Either, either)
import "base" Data.Function ((.), ($), flip)
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int)
import "base" Data.Maybe (maybe)
import "base" Data.Monoid ((<>))
import "base" Data.String (String)
import "base" System.IO (IO, print)
import "base" Text.Show (show)
import "bytestring" Data.ByteString.Lazy (ByteString, readFile, writeFile)
import "filepath" System.FilePath.Posix (FilePath, (</>))
import "http-client" Network.HTTP.Client (HttpException, Response)
import "joint" Control.Joint.Core (type (:=))
import "joint" Control.Joint.Modulator ((-<$>-))
import "joint" Control.Joint.Transformer (type (:>), embed, build, unite)
import "joint" Control.Joint.Base.Reader (Reader, ask)
import "lens" Control.Lens (preview)
import "wreq" Network.Wreq (get, responseBody)

import Data.Downloadable (Downloadable (download))

newtype Cover = Cover Int

instance Downloadable Cover where
	download (Cover aid) = Concurrently -<$>- (embed request >>= either (embed . print)
		(maybe failed save . preview responseBody)) where

		request :: IO (Either HttpException (Response ByteString))
		request = try . get $ "http://f4.bcbits.com/img/a" <> show aid <> "_10.jpg"

		save :: ByteString -> Reader FilePath :> IO := ()
		save bytes = build ask >>= embed . flip writeFile bytes . place

		place :: FilePath -> FilePath
		place dir = dir </> "cover.jpg"

		failed :: Reader FilePath :> IO := ()
		failed = embed $ print "Failed: downloading cover"
