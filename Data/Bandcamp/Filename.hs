module Data.Bandcamp.Filename where

import "aeson" Data.Aeson (FromJSON (parseJSON), Value (Object, Null), (.:))
import "base" Control.Applicative (pure)
import "base" Data.Function ((.), ($))
import "base" Data.Functor ((<$>))
import "base" Data.Maybe (Maybe (Just, Nothing))
import "base" Data.String (String)

data Filename = Filename (Maybe String)

instance FromJSON Filename where
	parseJSON (Object o) = Filename <$> o .: "mp3-128"
	parseJSON Null = pure . Filename $ Nothing
