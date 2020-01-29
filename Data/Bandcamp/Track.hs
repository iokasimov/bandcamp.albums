module Data.Bandcamp.Track where

import "aeson" Data.Aeson (FromJSON (parseJSON), Value (Object), withObject, (.:))
import "filepath" System.FilePath.Posix ((</>))
import "joint" Control.Joint (lift)
import "tagged" Data.Tagged (untag)

import Data.Downloadable (Downloadable (download), load)
import Data.Bandcamp.Title (Title, parse_title)
import Data.Bandcamp.Filename (Filename (Filename))

data Track = Track (Title Track) Filename

instance FromJSON Track where
	parseJSON = withObject "Title" $ \o -> Track <$> (parse_title $ Object o) <*> o .: "file"


instance Downloadable Track where
	download (Track title (Filename Nothing)) = lift . print $ "Track <" <> untag title <> "> not found..."
	download (Track title (Filename (Just link))) = load link place (untag title) where

		place :: FilePath -> FilePath
		place dir = dir </> untag title <> ".mp3"
