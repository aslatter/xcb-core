module Graphics.Xhb.Types where

import qualified Graphics.Xhb.Internal as I

import Control.Concurrent.MVar
import Data.IORef
import Foreign
import Foreign.C


type Lock = MVar ()

newLock :: IO Lock
newLock = newMVar ()

withLock :: Lock -> IO a -> IO a
withLock l k =
    withMVar l $ const k

data Connection
    = C {c_conn :: I.Connection,
         c_has_socket :: ForeignPtr CInt,
         c_lock :: Lock,
         c_last_req :: IORef Word64
        }
