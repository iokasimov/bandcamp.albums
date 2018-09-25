import "aeson" Data.Aeson (decode)
import "async" Control.Concurrent.Async (Concurrently (Concurrently), runConcurrently)
import "base" Control.Applicative ((*>), pure)
import "base" Control.Monad (void, (>>=))
import "base" Control.Exception (try)
import "base" Data.Either (Either (Left, Right), either)
import "base" Data.Function ((.), ($), flip)
import "base" Data.Functor ((<$>), ($>))
import "base" Data.Int (Int)
import "base" Data.Maybe (maybe)
import "base" Data.Monoid ((<>))
import "base" Data.Traversable (traverse)
import "base" System.IO (FilePath, IO, print)
import "base" Text.Show (show)
import "bytestring" Data.ByteString.Lazy (ByteString, readFile, writeFile)
import "directory" System.Directory (createDirectory, makeRelativeToCurrentDirectory)
import "http-client" Network.HTTP.Client (HttpException, Response)
import "lens" Control.Lens (preview)
import "text" Data.Text (Text, unpack)
import "transformers" Control.Monad.Trans.Class (lift)
import "transformers" Control.Monad.Trans.Reader (ReaderT (ReaderT, runReaderT), ask)
import "wreq" Network.Wreq (get, responseBody)

import Data.Bandcamp (Album (..), Current (..), File (..), Track (..))

track :: Track -> ReaderT FilePath IO ()
track (Track title (File link)) = lift request >>= either (lift . print)
	(maybe failed save . preview responseBody) where

	request :: IO (Either HttpException (Response ByteString))
	request = try . get . unpack $ link

	save :: ByteString -> ReaderT FilePath IO ()
	save bytes = ask >>= lift . flip writeFile bytes . path

	path :: FilePath -> FilePath
	path dir = dir <> "/" <> unpack title <> ".mp3"

	failed :: ReaderT FilePath IO ()
	failed = lift . print $ "Failed downloading track: " <> title

cover :: Int -> ReaderT FilePath IO ()
cover aid = lift request >>= either (lift . print)
	(maybe failed save . preview responseBody) where

	request :: IO (Either HttpException (Response ByteString))
	request = try . get $ "http://f4.bcbits.com/img/a" <> show aid <> "_10.jpg"

	save :: ByteString -> ReaderT FilePath IO ()
	save bytes = ask >>= lift . flip writeFile bytes . path

	path :: FilePath -> FilePath
	path dir = dir <> "/cover.jpg"

	failed :: ReaderT FilePath IO ()
	failed = lift $ print "Failed: downloading cover"

album :: Album -> IO ()
album (Album (Current title) ts _ aid') = make_directory >>=
	runReaderT (cover aid' *> void (traverse track ts)) where

	make_directory :: IO FilePath
	make_directory = do
		let dir = "Temporary/" <> (unpack title)
		createDirectory dir $> dir

main = decode @Album <$> readFile "scheme.json" >>= maybe
	(print "Error: album.json is invalid...") album
