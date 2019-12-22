module Data.Bandcamp (Album (..), Current (..), Filename (..), Track (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), Value (Object, Null), (.:))
import "base" Control.Applicative (pure, (<*>))
import "base" Data.Function ((.), ($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int)
import "base" Data.Maybe (Maybe (Just, Nothing))
import "base" Data.String (String)

data Filename = Filename (Maybe String)

instance FromJSON Filename where
	parseJSON (Object o) = Filename <$> o .: "mp3-128"
	parseJSON Null = pure . Filename $ Nothing

data Track = Track String Filename

instance FromJSON Track where
	parseJSON (Object o) = Track <$> o .: "title" <*> o .: "file"

type Artist = String

data Album = Album Current [Track] Artist Int

instance FromJSON Album where
	parseJSON (Object o) = Album <$> o .: "current" <*> o .: "trackinfo" <*> o .: "artist" <*> o .: "art_id"

data Current = Current String

instance FromJSON Current where
	parseJSON (Object o) = Current <$> o .: "title"
