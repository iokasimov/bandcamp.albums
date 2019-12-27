module Data.Bandcamp.Album where

import "aeson" Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import "async" Control.Concurrent.Async (Concurrently (Concurrently, runConcurrently))
import "base" Control.Applicative (pure, (<*>), (*>))
import "base" Control.Monad (void)
import "base" Data.Function ((.), ($))
import "base" Data.Functor ((<$>))
import "base" Data.String (String)
import "base" Data.Traversable (sequence, traverse)
import "joint" Control.Joint.Core (type (:=))
import "joint" Control.Joint.Modulator ((-<$>-))
import "joint" Control.Joint.Transformer (type (:>), embed, build, unite)
import "joint" Control.Joint.Base.Reader (Reader, ask)
-- import "terminal-progress-bar" System.ProgressBar
-- 	(Progress (Progress), defStyle, incProgress, newProgressBar)

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
	download a@(Album (Current album) tracks artist cover) = do
		download cover *> void (traverse download tracks)

		-- download cover *> concurrently_download tracks where
		--
		-- concurrently_download ts = do
		-- 	pb <- lift $ newProgressBar defStyle 10 $ Progress 0 20 ()
		-- 	void . mapReaderT runConcurrently . traverse (download_track pb) $ ts
		--
		-- download_track pb track = mapReaderT Concurrently $
		-- 	download track *> lift (incProgress pb 1)
