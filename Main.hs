import "aeson" Data.Aeson (decode)
import "async" Control.Concurrent.Async (Concurrently (Concurrently), runConcurrently)
import "base" Control.Applicative ((*>), pure)
import "base" Control.Monad (void, (>>=))
import "base" Control.Exception (try)
import "base" Data.Either (Either (Left, Right), either)
import "base" Data.Function ((.), ($), flip)
import "base" Data.Functor ((<$>), ($>))
import "base" Data.Int (Int)
import "base" Data.Maybe (Maybe (Just, Nothing), maybe)
import "base" Data.Monoid ((<>))
import "base" Data.String (String)
import "base" Data.Traversable (traverse)
import "base" System.IO (IO, print)
import "base" Text.Show (show)
import "bytestring" Data.ByteString.Lazy (ByteString, readFile, writeFile)
import "http-client" Network.HTTP.Client (HttpException, Response)
import "lens" Control.Lens (preview)
import "monopati" System.Monopati.Posix -- (Path (Path), Reference (Absolute), Points (Directory, File), create, part, (<^>), (</>))
import "text" Data.Text (Text, unpack)
import "transformers" Control.Monad.Trans.Class (lift)
import "transformers" Control.Monad.Trans.Reader (ReaderT (ReaderT, runReaderT), ask)
import "wreq" Network.Wreq (get, responseBody)

import Data.Bandcamp (Album (..), Current (..), Filename (..), Track (..))

track :: Track -> ReaderT (Absolute Path To Directory) IO ()
track (Track title (Filename link)) = lift request >>= either (lift . print)
	(maybe failed save . preview responseBody) where

	request :: IO (Either HttpException (Response ByteString))
	request = try . get $ link

	save :: ByteString -> ReaderT (Absolute Path To Directory) IO ()
	save bytes = ask >>= lift . flip writeFile bytes . show . place

	place :: Absolute Path To Directory -> Absolute Path To File
	place dir = dir </> part @Vague @File (title <> ".mp3")

	failed :: ReaderT (Absolute Path To Directory) IO ()
	failed = lift . print $ "Failed downloading track: " <> title

cover :: Int -> ReaderT (Absolute Path To Directory) IO ()
cover aid = lift request >>= either (lift . print)
	(maybe failed save . preview responseBody) where

	request :: IO (Either HttpException (Response ByteString))
	request = try . get $ "http://f4.bcbits.com/img/a" <> show aid <> "_10.jpg"

	save :: ByteString -> ReaderT (Absolute Path To Directory) IO ()
	save bytes = ask >>= lift . flip writeFile bytes . show . place

	place :: Absolute Path To Directory -> Absolute Path To File
	place dir = dir </> part @Vague @File "cover.jpg"

	failed :: ReaderT (Absolute Path To Directory) IO ()
	failed = lift $ print "Failed: downloading cover"

create_absolute_path :: String -> Maybe (Absolute Path To Directory) -> Absolute Path To Directory
create_absolute_path title (Just current_dir) = current_dir </> part @Now @Directory "Temporary" </> part @Vague @Directory title
create_absolute_path title Nothing = part @Root @Directory "Temporary" </> part @Vague @Directory title

album :: Album -> IO ()
album (Album (Current title) ts _ aid') = do
	dir <- create_absolute_path title <$> current
	create dir *> runReaderT (cover aid' *> void (traverse track ts)) dir

main = decode @Album <$> readFile "scheme.json" >>=
	maybe (print "Error: album.json is invalid...") album
