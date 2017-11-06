module Entities.Current (Current(..)) where

import Data.Aeson
import qualified Data.Text as T

data Current = Current { atitle :: T.Text } deriving Show

instance FromJSON Current where
	parseJSON (Object o) = Current
		<$> o .: "title"