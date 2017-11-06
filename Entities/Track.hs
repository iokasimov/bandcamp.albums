module Entities.Track (Track(..), download) where

import Data.Aeson
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Exception
import Control.Lens
import Network.Wreq
import Network.HTTP.Client hiding (responseBody)
import qualified Data.ByteString.Lazy as Lazybytes

import Entities.File

type Lazybytes = Lazybytes.ByteString

data Track = Track { stitle :: T.Text, file :: File } deriving Show

instance FromJSON Track where
	parseJSON (Object o) = Track
		<$> o .: "title"
		<*> o .: "file"

download :: String -> Track -> IO ()
download dir track = requesting track >>= saving track dir where

	requesting :: Track -> IO (Either HttpException (Response Lazybytes))
	requesting track = try $ get $ T.unpack $ (link . file) track

	saving :: Track -> String -> Either HttpException (Response Lazybytes) -> IO ()
	saving track dir (Right res) = case res ^? responseBody of
		Nothing -> T.putStrLn $ "Error: failed downloading: " <> (stitle track)
		Just bytes -> Lazybytes.writeFile (dir <> "/" <> (T.unpack $ stitle track) <> ".mp3") bytes
	saving _ _ (Left error) = print error