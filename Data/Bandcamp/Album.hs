module Data.Bandcamp.Album where

import "aeson" Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import "base" Control.Applicative ((<*>), (*>))
import "base" Control.Monad (void)
import "base" Data.Functor ((<$>))
import "base" Data.String (String)
import "base" Data.Traversable (traverse)

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
	download a@(Album (Current album) ts artist cover) =
		download cover *> void (traverse download ts)
