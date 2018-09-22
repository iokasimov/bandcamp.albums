module Data.Bandcamp (Album (..), Current (..), File (..), Track (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import "base" Control.Applicative ((<*>))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int)
import "text" Data.Text (Text)

data File = File Text

instance FromJSON File where
	parseJSON (Object o) = File <$> o .: "mp3-128"

data Track = Track Text File

instance FromJSON Track where
	parseJSON (Object o) = Track <$> o .: "title" <*> o .: "file"

type Artist = Text

data Album = Album Current [Track] Text Int

instance FromJSON Album where
	parseJSON (Object o) = Album <$> o .: "current" <*> o .: "trackinfo" <*> o .: "artist" <*> o .: "art_id"

data Current = Current Text

instance FromJSON Current where
	parseJSON (Object o) = Current <$> o .: "title"
