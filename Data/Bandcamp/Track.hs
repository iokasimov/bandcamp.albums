module Data.Bandcamp.Track where

import "aeson" Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import "async" Control.Concurrent.Async (Concurrently (Concurrently))
import "base" Control.Applicative ((<*>))
import "base" Control.Monad ((>>=))
import "base" Control.Exception (try)
import "base" Data.Either (Either, either)
import "base" Data.Function ((.), ($), flip)
import "base" Data.Functor ((<$>))
import "base" Data.Maybe (Maybe (Just, Nothing), maybe)
import "base" Data.Monoid ((<>))
import "base" Data.String (String)
import "base" System.IO (IO, print)
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
import Data.Bandcamp.Filename (Filename (Filename))

data Track = Track String Filename

instance FromJSON Track where
	parseJSON (Object o) = Track <$> o .: "title" <*> o .: "file"

instance Downloadable Track where
	download (Track title (Filename Nothing)) = embed . Concurrently . print $ "Track <" <> title <> "> not found..."
	download (Track title (Filename (Just link))) = Concurrently -<$>- (embed request >>= either (embed . print)
		(maybe failed save . preview responseBody)) where

		request :: IO (Either HttpException (Response ByteString))
		request = try . get $ link

		save :: ByteString -> Reader FilePath :> IO := ()
		save bytes = build ask >>= embed . flip writeFile bytes . place

		place :: FilePath -> FilePath
		place dir = dir </> title <> ".mp3"

		failed :: Reader FilePath :> IO := ()
		failed = embed . print $ "Failed downloading track: " <> title
