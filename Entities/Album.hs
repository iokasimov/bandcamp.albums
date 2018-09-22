module Entities.Album (Album (..), download) where

import Data.Aeson
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Exception
import Control.Lens
import Control.Concurrent.Async
import Network.Wreq
import Network.HTTP.Client hiding (responseBody)
import System.Directory (createDirectory, makeRelativeToCurrentDirectory)
import qualified Data.ByteString.Lazy as Lazybytes

import qualified Entities.Track as Track
import qualified Entities.Current as Current

type Track = Track.Track
type Current = Current.Current
type Lazybytes = Lazybytes.ByteString

data Album = Album { current :: Current, trackinfo :: [Track], artist :: T.Text, aid :: Integer } deriving Show

instance FromJSON Album where
	parseJSON (Object o) = Album
		<$> o .: "current"
		<*> o .: "trackinfo"
		<*> o .: "artist"
		<*> o .: "art_id"

create_folder :: Album -> IO FilePath
create_folder album = makeRelativeToCurrentDirectory
	((mappend "Temporary/" . T.unpack . Current.atitle . current) album) >>=
		\dir -> createDirectory dir >> return dir

download_cover :: FilePath -> Integer -> IO ()
download_cover dir aid = requesting aid >>= saving dir where

	requesting :: Integer -> IO (Either HttpException (Response Lazybytes))
	requesting aid = try $ get $ "http://f4.bcbits.com/img/a" <> (show aid) <> "_10.jpg"

	saving :: String -> Either HttpException (Response Lazybytes) -> IO ()
	saving dir (Right res) = case res ^? responseBody of
		Nothing -> T.putStrLn $ "Error: failed downloading cover"
		Just cover -> Lazybytes.writeFile (dir <> "/" <> "cover.jpg") cover
	saving _ (Left error) = print error

download :: Album -> IO ()
download album = do
	dir <- create_folder album
	download_cover dir $ aid album
	_ <- runConcurrently $
		traverse (Concurrently . Track.download dir) $
		trackinfo album
	return ()
