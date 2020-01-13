module Data.Bandcamp.Track where

import "aeson" Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import "async" Control.Concurrent.Async (Concurrently (Concurrently))
import "base" Control.Applicative ((<*>))
import "base" Data.Either (Either, either)
import "base" Data.Function ((.), ($))
import "base" Data.Functor ((<$>))
import "base" Data.Maybe (Maybe (Just, Nothing))
import "base" Data.Monoid ((<>))
import "base" System.IO (print)
import "filepath" System.FilePath.Posix (FilePath, (</>))
import "joint" Control.Joint.Abilities.Modulator ((-<$>-))
import "joint" Control.Joint.Abilities.Transformer (embed)
import "tagged" Data.Tagged (untag)

import Data.Downloadable (Downloadable (download), load)
import Data.Bandcamp.Title (Title, parse_title)
import Data.Bandcamp.Filename (Filename (Filename))

data Track = Track (Title Track) Filename

instance FromJSON Track where
	parseJSON (Object o) = Track <$> (parse_title $ Object o) <*> o .: "file"

instance Downloadable Track where
	download (Track title (Filename Nothing)) = embed . Concurrently . print $ "Track <" <> untag title <> "> not found..."
	download (Track title (Filename (Just link))) = Concurrently -<$>- load link place (untag title) where

		place :: FilePath -> FilePath
		place dir = dir </> untag title <> ".mp3"
