module Data.Downloadable where

import "base" System.IO (IO)
import "filepath" System.FilePath.Posix (FilePath)
import "transformers" Control.Monad.Trans.Reader (ReaderT)

class Downloadable a where
	download :: a -> ReaderT FilePath IO ()
