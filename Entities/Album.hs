module Entities.Album (Album (..), download) where

import Prelude hiding (writeFile)
import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import Control.Exception (try)
import Control.Lens (preview)
import Control.Concurrent.Async (Concurrently (Concurrently), runConcurrently)
import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (ReaderT, runReaderT), ask)
import Network.Wreq (get, responseBody)
import Network.HTTP.Client (HttpException, Response)
import System.Directory (createDirectory, makeRelativeToCurrentDirectory)
import Data.ByteString.Lazy (ByteString, writeFile)

import Entities.Current (Current, atitle)
import Entities.Track (Track)

import qualified Entities.Track as Track (download)

type Artist = Text

data Album = Album Current [Track] Text Integer
	deriving Show

instance FromJSON Album where
	parseJSON (Object o) = Album
		<$> o .: "current" <*> o .: "trackinfo"
		<*> o .: "artist" <*> o .: "art_id"

create_folder :: Album -> IO FilePath
create_folder (Album current _ _ _) = makeRelativeToCurrentDirectory
	(mappend "Temporary/" . unpack . atitle $ current) >>=
		\dir -> createDirectory dir >> return dir

download_cover :: Integer -> ReaderT FilePath IO ()
download_cover aid = lift request >>= either (lift . print)
	(maybe failed save . preview responseBody) where

	request :: IO (Either HttpException (Response ByteString))
	request = try . get $ "http://f4.bcbits.com/img/a" <> (show aid) <> "_10.jpg"

	save :: ByteString -> ReaderT FilePath IO ()
	save bytes = ask >>= \dir -> lift $ writeFile
		(dir <> "/" <> "cover.jpg") bytes

	failed :: ReaderT FilePath IO ()
	failed = lift $ print "Failed: downloading cover"

download_tracks :: [Track] -> ReaderT FilePath IO ()
download_tracks ts = ask >>= \dir -> lift .	void . runConcurrently
	. traverse (Concurrently . Track.download dir) $ ts

download :: Album -> IO ()
download album@(Album _ tracks _ aid') = create_folder album >>=
	runReaderT (download_cover aid' *> download_tracks tracks)
