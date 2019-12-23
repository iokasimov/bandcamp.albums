module Data.Bandcamp.Track where

import "aeson" Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
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
import "lens" Control.Lens (preview)
import "transformers" Control.Monad.Trans.Class (lift)
import "transformers" Control.Monad.Trans.Reader (ReaderT, ask)
import "wreq" Network.Wreq (get, responseBody)

import Data.Downloadable (Downloadable (download))
import Data.Bandcamp.Filename (Filename (Filename))

data Track = Track String Filename

instance FromJSON Track where
	parseJSON (Object o) = Track <$> o .: "title" <*> o .: "file"

instance Downloadable Track where
	download (Track title (Filename Nothing)) = lift . print $ "Track <" <> title <> "> not found..."
	download (Track title (Filename (Just link))) = lift request >>= either (lift . print)
		(maybe failed save . preview responseBody) where

		request :: IO (Either HttpException (Response ByteString))
		request = try . get $ link

		save :: ByteString -> ReaderT FilePath IO ()
		save bytes = ask >>= lift . flip writeFile bytes . place

		place :: FilePath -> FilePath
		place dir = dir </> title <> ".mp3"

		failed :: ReaderT FilePath IO ()
		failed = lift . print $ "Failed downloading track: " <> title
