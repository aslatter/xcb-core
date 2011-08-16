
-- Commented out exports represent things left
-- to do, or things that no longer make sense

module Graphics.X11.Xcb
 ( Connection
 , connect
 , connect'
 -- , I.parseDisplay -- no reason not to export, but not sure why we would?
 , writeRequests
 , maximumRequestLength
 , prefetchMaximumReuestLength
 , connectionHasError
 , I.Cookie(..)
 , waitForEvent
 , waitForReply
 , generateId
 , getSetup
 , I.Extension(..)
 , prefetchExtension
 , extensionPresent
 , extensionMajorOpcode
 , extensionFirstEvent
 , extensionFirstError
 , withForeignConnection
 , withConnectionPtr
 ) where

import qualified Graphics.X11.Xcb.Internal as I
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Control.Applicative ((<$>))
import Control.Concurrent.MVar
import Data.IORef
import Data.List (foldl')
import Data.Word
import Foreign
import Foreign.C


type Lock = MVar ()

newLock :: IO Lock
newLock = newMVar ()

withLock :: Lock -> IO a -> IO a
withLock l k =
    withMVar l $ const k

{-

XCB socket handoff works as follows:

* When we want to use the socket, we tell XCB and
give it a callback to use to take it back (and a callback
closure).

* XCB either flushes its own buffers, or asks the
current socket owner to give the socket back.

* XCB calls the supplied callback when it wants us
to return ownership of the socket.

Creating C-usable callbacks from Haskell functions
which have indeterminate lifetimes is hard.

Since the only thing we need to know is if the
socket has been taken back or not (there are
no buffers to flush on the Haskell end yet) we
can do all of the callbacks with C code.

-}

data Connection
    = C {c_conn :: I.Connection,
         c_has_socket :: ForeignPtr CInt,
         c_lock :: Lock,
         c_last_req :: IORef Word64
        }

-- | For passing to foreign libraries which
-- can make use of a libxcb connection.
withConnectionPtr :: Connection -> (Ptr Connection -> IO a) -> IO a
withConnectionPtr c k = I.withConnection (c_conn c) (k . castPtr)

-- | For a libxcb connection passed in from another library.
-- Be carefule forking threads or saving off a reference to the connection -
-- we can't coordinate the closing of the connection with the source
-- of the connection.
withForeignConnection :: Ptr Connection -> (Connection -> IO a) -> IO a
withForeignConnection ptr k =
  I.mkConnection_ (castPtr ptr) >>= dressIConn >>= k

-- | The the Haskell xcb connection.
-- This has nothing to do with the lock used
-- on the inside of libxcb.
lockCon :: Connection -> IO a -> IO a
lockCon c = withLock (c_lock c)

foreign import ccall "&xcb_ffi_return_socket"
        c_return_socket :: FunPtr (Ptr () -> IO ())

hasSocket :: Connection -> IO Bool
hasSocket c =
    toBool <$> withForeignPtr (c_has_socket c) peek

takeSocket :: Connection ->  [I.RequestFlags] -> IO Bool
takeSocket c fs = do
  has <- hasSocket c
  if has then return True else do
  withForeignPtr (c_has_socket c) $ \hasPtr -> do
  let flags = foldl' (.&.) 0 (map (fromIntegral . fromEnum) fs)
  res <- I.takeSocket (c_conn c) c_return_socket (castPtr hasPtr) flags
  case res of
    Nothing -> return False
    Just lastSeq
        -> do
      writeIORef (c_last_req c) lastSeq
      withForeignPtr (c_has_socket c) (flip poke $ fromBool True)
      return True

guardIO :: Bool -> IO ()
guardIO p = if p then return () else fail "guard failed"

{- |

Write raw bytes on to the connection with the server.
Returns the sequence number of the last request sent.


-}
writeRequests :: Connection -> L.ByteString -> Int -> IO Word64
writeRequests c bytes num
    = lockCon c $ do
        -- should probably have better error here
        takeSocket c [] >>= guardIO
        lastSeq <- readIORef (c_last_req c)
        let newLast = lastSeq + fromIntegral num
        ret <- I.writev (c_conn c) bytes (fromIntegral num)
        guardIO ret -- should have better error here
        writeIORef (c_last_req c) newLast
        return newLast

connect :: IO (Maybe Connection)
connect = connect' ""

connect' :: String -> IO (Maybe Connection)
connect' displayStr
    = do
  ret <- I.connect displayStr
  case ret of
    Nothing -> return Nothing
    Just (icon,_)
        -> Just <$> dressIConn icon

dressIConn :: I.Connection -> IO Connection
dressIConn ptr = do
  lastRef <- newIORef 0
  hasPtr <- mallocForeignPtr
  withForeignPtr hasPtr (flip poke 0)
  lock <- newLock
  return $ C ptr hasPtr lock lastRef

-- | Maximum size of a single request.
-- May not be an asynchronous call, as we'll make a call out
-- on the big-requests extension to find out if we can use that.
maximumRequestLength :: Connection -> IO Word32
maximumRequestLength = I.maximumRequestLength . c_conn

-- | Asynchronous version of 'maximumRequestLength'.  The return
-- from the server is cached for subsequent callse to 'maximumRequestLength'.
prefetchMaximumReuestLength :: Connection -> IO ()
prefetchMaximumReuestLength = I.prefetchMaximumReuestLength . c_conn

-- | Is the connection in an error state?
connectionHasError :: Connection -> IO Bool
connectionHasError = I.connectionHasError . c_conn

-- | Allocation a new xid. May involve server interaction
-- (but likely won't).
generateId :: Connection -> IO Word32
generateId = I.generateId . c_conn


-- | Returns an event or error from the queue.
-- Errors associated with checked requests will not
-- be available from this call.
waitForEvent :: Connection -> IO S.ByteString
waitForEvent c = I.waitForEvent (c_conn c) >>= I.unsafeEventData
-- I haven't convinced myself this is safe to do without a lock.
-- Maybe a separate events lock on the connection?

waitForReply :: Connection -> I.Cookie -> IO (Either S.ByteString S.ByteString)
waitForReply c cookie
    = do
  resp <- I.waitForReply (c_conn c) cookie
  case resp of
    Left err  -> Left <$> I.unsafeErrorData err
    Right rep -> Right <$> I.unsafeReplyData rep
-- for locking, we might want to put a lock in the cookie

-- | Non-blocking query for extension data
prefetchExtension :: Connection -> I.Extension -> IO ()
prefetchExtension c e = I.prefetchExtension (c_conn c) e

extensionPresent :: Connection -> I.Extension -> IO Bool
extensionPresent c e
    = I.unsafeGetExtensionData (c_conn c) e >>= I.extensionPresent

extensionMajorOpcode :: Connection -> I.Extension -> IO Word8
extensionMajorOpcode c e
    = I.unsafeGetExtensionData (c_conn c) e >>= I.extensionMajorOpcode

extensionFirstEvent :: Connection -> I.Extension -> IO Word8
extensionFirstEvent c e
    = I.unsafeGetExtensionData (c_conn c) e >>= I.extensionFirstEvent

extensionFirstError :: Connection -> I.Extension -> IO Word8
extensionFirstError c e
    = I.unsafeGetExtensionData (c_conn c) e >>= I.extensionFirstError

getSetup :: Connection -> IO S.ByteString
getSetup c = S.copy <$> (I.unsafeGetSetup (c_conn c) >>= I.unsafeSetupData)
-- TODO: need to prevent the connection getting GCd until after S.copy
-- future thought: cache a copy on the connection wrapper
