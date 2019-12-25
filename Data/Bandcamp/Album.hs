module Data.Bandcamp.Album where

import "aeson" Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import "async" Control.Concurrent.Async (Concurrently (Concurrently, runConcurrently))
import "base" Control.Applicative ((<*>), (*>))
import "base" Control.Monad (void)
import "base" Data.Function ((.), ($))
import "base" Data.Functor ((<$>))
import "base" Data.String (String)
import "base" Data.Traversable (traverse)
import "terminal-progress-bar" System.ProgressBar
	(Progress (Progress), defStyle, incProgress, newProgressBar)
import "transformers" Control.Monad.Trans.Class (lift)
import "transformers" Control.Monad.Trans.Reader (mapReaderT)

import Data.Downloadable (Downloadable (download))
import Data.Bandcamp.Cover (Cover (Cover))
import Data.Bandcamp.Current (Current (Current))
import Data.Bandcamp.Track (Track)

type Artist = String

data Album = Album Current [Track] Artist Cover

instance FromJSON Album where
	parseJSON (Object o) = Album <$> o .: "current" <*> o .: "trackinfo"
		<*> o .: "artist" <*> (Cover <$> o .: "art_id")

instance Downloadable Album where
	download a@(Album (Current album) tracks artist cover) =
		download cover *> concurrently_download tracks where

		concurrently_download ts = do
			pb <- lift $ newProgressBar defStyle 10 $ Progress 0 20 ()
			void . mapReaderT runConcurrently . traverse (download_track pb) $ ts

		download_track pb track = mapReaderT Concurrently $
			download track *> lift (incProgress pb 1)
