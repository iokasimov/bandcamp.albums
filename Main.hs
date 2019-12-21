import "aeson" Data.Aeson (decode)
import "async" Control.Concurrent.Async (Concurrently (Concurrently), runConcurrently)
import "base" Control.Applicative ((*>), pure)
import "base" Control.Monad (void, (>>=))
import "base" Control.Exception (try)
import "base" Data.Bool (Bool (True))
import "base" Data.Either (Either (Left, Right), either)
import "base" Data.Function ((.), ($), flip)
import "base" Data.Functor ((<$>), ($>))
import "base" Data.Int (Int)
import "base" Data.Maybe (Maybe (Just, Nothing), maybe)
import "base" Data.Monoid ((<>))
import "base" Data.String (String)
import "base" Data.Traversable (traverse)
import "base" System.IO (FilePath, IO, print)
import "base" Text.Show (show)
import "bytestring" Data.ByteString.Lazy (ByteString, readFile, writeFile)
import "directory" System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import "filepath" System.FilePath.Posix ((</>))
import "http-client" Network.HTTP.Client (HttpException, Response)
import "lens" Control.Lens (preview)
import "text" Data.Text (Text, unpack)
import "transformers" Control.Monad.Trans.Class (lift)
import "transformers" Control.Monad.Trans.Reader (ReaderT (ReaderT, runReaderT), ask)
import "wreq" Network.Wreq (get, responseBody)

import Data.Bandcamp (Album (..), Current (..), Filename (..), Track (..))

track :: Track -> ReaderT FilePath IO ()
track (Track title (Filename link)) = lift request >>= either (lift . print)
	(maybe failed save . preview responseBody) where

	request :: IO (Either HttpException (Response ByteString))
	request = try . get $ link

	save :: ByteString -> ReaderT FilePath IO ()
	save bytes = ask >>= lift . flip writeFile bytes . place

	place :: FilePath -> FilePath
	place dir = dir </> title <> ".mp3"

	failed :: ReaderT FilePath IO ()
	failed = lift . print $ "Failed downloading track: " <> title

cover :: Int -> ReaderT FilePath IO ()
cover aid = lift request >>= either (lift . print)
	(maybe failed save . preview responseBody) where

	request :: IO (Either HttpException (Response ByteString))
	request = try . get $ "http://f4.bcbits.com/img/a" <> show aid <> "_10.jpg"

	save :: ByteString -> ReaderT FilePath IO ()
	save bytes = ask >>= lift . flip writeFile bytes . place

	place :: FilePath -> FilePath
	place dir = dir </> "cover.jpg"

	failed :: ReaderT FilePath IO ()
	failed = lift $ print "Failed: downloading cover"

album :: Album -> IO ()
album (Album (Current album) ts artist aid') = do
	dir <- (\cur -> cur </> "Temporary" </> artist </> album) <$> getCurrentDirectory
	createDirectoryIfMissing True dir *> runReaderT (cover aid' *> void (traverse track ts)) dir

main = decode @Album <$> readFile "scheme.json" >>=
	maybe (print "Error: album.json is invalid...") album
