module Entities.File (File(..)) where

import Data.Aeson
import qualified Data.Text as T

data File = File { link :: T.Text } deriving Show

instance FromJSON File where
	parseJSON (Object o) = File
		<$> o .: "mp3-128"