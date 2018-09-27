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
import "base" System.IO (IO, print)
import "base" Text.Show (show)
import "bytestring" Data.ByteString.Lazy (ByteString, readFile, writeFile)
import "http-client" Network.HTTP.Client (HttpException, Response)
import "lens" Control.Lens (preview)
import "monopati" System.Monopati (Path (Path), Reference (Absolute), Points (Directory, File), part, (<^>), (</>))
import "monopati" System.Monopati.Posix (create)
import "text" Data.Text (Text, unpack)
import "transformers" Control.Monad.Trans.Class (lift)
import "transformers" Control.Monad.Trans.Reader (ReaderT (ReaderT, runReaderT), ask)
import "wreq" Network.Wreq (get, responseBody)

import Data.Bandcamp (Album (..), Current (..), Filename (..), Track (..))

track :: Track -> ReaderT (Path Absolute Directory) IO ()
track (Track title (Filename link)) = lift request >>= either (lift . print)
	(maybe failed save . preview responseBody) where

	request :: IO (Either HttpException (Response ByteString))
	request = try . get $ link

	save :: ByteString -> ReaderT (Path Absolute Directory) IO ()
	save bytes = ask >>= lift . flip writeFile bytes . show . place

	place :: Path Absolute Directory -> Path Absolute File
	place dir = dir </> part (title <> ".mp3")

	failed :: ReaderT (Path Absolute Directory) IO ()
	failed = lift . print $ "Failed downloading track: " <> title

cover :: Int -> ReaderT (Path Absolute Directory) IO ()
cover aid = lift request >>= either (lift . print)
	(maybe failed save . preview responseBody) where

	request :: IO (Either HttpException (Response ByteString))
	request = try . get $ "http://f4.bcbits.com/img/a" <> show aid <> "_10.jpg"

	save :: ByteString -> ReaderT (Path Absolute Directory) IO ()
	save bytes = ask >>= lift . flip writeFile bytes . show . place

	place :: Path Absolute Directory -> Path Absolute File
	place dir = dir </> part "cover.jpg"

	failed :: ReaderT (Path Absolute Directory) IO ()
	failed = lift $ print "Failed: downloading cover"

album :: Album -> IO ()
album (Album (Current title) ts _ aid') =
	create (part "Temporary" <^> part title) >>=
		runReaderT (cover aid' *> void (traverse track ts)) where

main = decode @Album <$> readFile "scheme.json" >>=
	maybe (print "Error: album.json is invalid...") album
