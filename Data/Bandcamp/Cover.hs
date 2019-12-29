module Data.Bandcamp.Cover where

import "async" Control.Concurrent.Async (Concurrently (Concurrently))
import "base" Data.Int (Int)
import "base" Data.Monoid ((<>))
import "base" Data.String (String)
import "base" Text.Show (show)
import "filepath" System.FilePath.Posix (FilePath, (</>))
import "joint" Control.Joint.Modulator ((-<$>-))

import Data.Downloadable (Downloadable (download), load)

newtype Cover = Cover Int

instance Downloadable Cover where
	download (Cover aid) = Concurrently -<$>- load link place "album's cover" where

		link :: String
		link = "http://f4.bcbits.com/img/a" <> show aid <> "_10.jpg"

		place :: FilePath -> FilePath
		place dir = dir </> "cover.jpg"
