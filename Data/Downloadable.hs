module Data.Downloadable where

import "async" Control.Concurrent.Async (Concurrently)
import "base" System.IO (IO)
import "filepath" System.FilePath.Posix (FilePath)
import "joint" Control.Joint.Core (type (:=))
import "joint" Control.Joint.Transformer (type (:>))
import "joint" Control.Joint.Base.Reader (Reader)

class Downloadable a where
	download :: a -> Reader FilePath :> Concurrently := ()
