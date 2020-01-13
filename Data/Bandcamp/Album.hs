module Data.Bandcamp.Album where

import "aeson" Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import "async" Control.Concurrent.Async (Concurrently (Concurrently, runConcurrently))
import "base" Control.Applicative (pure, (<*>), (*>))
import "base" Control.Monad (void, (>>=))
import "base" Data.Function ((.), ($))
import "base" Data.Functor ((<$>))
import "base" Data.String (String)
import "base" Data.Traversable (sequence, traverse)
import "joint" Control.Joint.Core (type (:=))
import "joint" Control.Joint.Abilities.Modulator ((-<$>-))
import "joint" Control.Joint.Abilities.Transformer (type (:>), embed, build, unite)
import "joint" Control.Joint.Effects.Reader (Reader, ask)
-- import "terminal-progress-bar" System.ProgressBar
-- 	(Progress (Progress), defStyle, incProgress, newProgressBar)

import Data.Downloadable (Downloadable (download))
import Data.Bandcamp.Title (Title, parse_title)
import Data.Bandcamp.Cover (Cover (Cover))
import Data.Bandcamp.Track (Track)

type Artist = String

data Album = Album (Title Album) [Track] Artist Cover

instance FromJSON Album where
	parseJSON (Object o) = Album <$> (o .: "current" >>= parse_title)
		<*> o .: "trackinfo" <*> o .: "artist" <*> (Cover <$> o .: "art_id")

instance Downloadable Album where
	download a@(Album _ tracks artist cover) = do
		download cover *> void (traverse download tracks)
