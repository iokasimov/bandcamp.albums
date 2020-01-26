module Data.Bandcamp.Filename where

import "aeson" Data.Aeson (FromJSON (parseJSON), Value (Object, Null), (.:))

data Filename = Filename (Maybe String)

instance FromJSON Filename where
	parseJSON (Object o) = Filename <$> o .: "mp3-128"
	parseJSON Null = pure . Filename $ Nothing
