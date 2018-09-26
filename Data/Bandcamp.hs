module Data.Bandcamp (Album (..), Current (..), Filename (..), Track (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import "base" Control.Applicative ((<*>))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int)
import "text" Data.Text (Text)

data Filename = Filename Text

instance FromJSON Filename where
	parseJSON (Object o) = Filename <$> o .: "mp3-128"

data Track = Track Text Filename

instance FromJSON Track where
	parseJSON (Object o) = Track <$> o .: "title" <*> o .: "file"

type Artist = Text

data Album = Album Current [Track] Text Int

instance FromJSON Album where
	parseJSON (Object o) = Album <$> o .: "current" <*> o .: "trackinfo" <*> o .: "artist" <*> o .: "art_id"

data Current = Current Text

instance FromJSON Current where
	parseJSON (Object o) = Current <$> o .: "title"
