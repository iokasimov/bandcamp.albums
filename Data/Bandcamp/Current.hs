module Data.Bandcamp.Current where

import "aeson" Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import "base" Data.Functor ((<$>))
import "base" Data.String (String)

data Current = Current String

instance FromJSON Current where
	parseJSON (Object o) = Current <$> o .: "title"
