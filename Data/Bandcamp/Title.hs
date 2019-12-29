module Data.Bandcamp.Title where

import "aeson" Data.Aeson (withObject, (.:))
import "aeson" Data.Aeson.Types (Parser, Value)
import "base" Data.String (String)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "tagged" Data.Tagged (Tagged (Tagged))

type family Title a where
	Title a = Tagged a String

parse_title :: Value -> Parser (Title a)
parse_title = withObject "Title" $ \v -> Tagged <$> v .: "title"
