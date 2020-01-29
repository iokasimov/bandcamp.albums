module Data.Bandcamp.Album where

import "aeson" Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import "async" Control.Concurrent.Async (Concurrently (Concurrently, runConcurrently))
import "base" Control.Applicative ((<*>))
import "base" Control.Monad (void)
import "base" Data.Traversable (for)
import "joint" Control.Joint.Core (type (:=))
import "joint" Control.Joint.Abilities (lift, run, (:>))
import "joint" Control.Joint.Effects (Reader, get)

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
	download (Album _ tracks _ cover) = lift (get @FilePath) >>= \path ->
		let actions = download cover : (download <$> tracks) in
		lift . void . runConcurrently . for actions $ Concurrently . run . flip run path
