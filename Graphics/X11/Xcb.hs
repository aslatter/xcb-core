
-- Commented out exports represent things left
-- to do, or things that no longer make sense

module Graphics.X11.Xcb
 ( Connection
 , connect
 , connect'
 , I.parseDisplay
 , writeRequests
 -- ,I.maximumRequestLength
 -- ,I.prefetchMaximumReuestLength
 -- ,I.connectionHasError
 -- ,sendRequest
 -- ,I.RequestFlags(..)
 -- ,I.RequestInfo
 -- ,I.mkRequestInfo
 ,I.Cookie(..)
 -- ,requestCheck
 ,waitForReply
 -- ,I.generateId
 ,getSetup
 ,I.Extension(..)
 -- ,I.prefetchExtension
 ,extensionPresent
 ,extensionMajorOpcode
 ,extensionFirstEvent
 ,extensionFirstError
 -- ,withForeignConnection
 -- ,I.withConnection
 ) where

import qualified Graphics.X11.Xcb.Internal as I
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Control.Applicative ((<$>))
import Control.Concurrent.MVar
import Control.Monad
import Data.Bits((.&.))
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
which have indeterminant lifetimes is hard.

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
    Just last
        -> do
      writeIORef (c_last_req c) last
      withForeignPtr (c_has_socket c) (flip poke $ fromBool True)
      return True

guardIO p = if p then return () else fail "guard failed"

-- | Returns the sequence number of the last request sent.
writeRequests :: Connection -> L.ByteString -> Int -> IO Word64
writeRequests c bytes num
    = lockCon c $ do
        ret <- takeSocket c []
        guardIO ret -- should probably have better error here
        last <- readIORef (c_last_req c)
        let newLast = last + fromIntegral num
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
        -> do
      last <- newIORef 0
      hasPtr <- mallocForeignPtr
      withForeignPtr hasPtr (flip poke 0)
      lock <- newLock
      return $ Just $ C icon hasPtr lock last


-- | Try not to do something sneaky like
-- 'withForeignConnection ptr return'.  The Connection
-- is not garaunteed outside the scope of this call.
withForeignConnection :: Ptr I.Connection
                      -> (I.Connection -> IO a)
                      -> IO a
withForeignConnection cPtr f
    = do
  c <- I.mkConnection_ cPtr
  x <- f c
  return x

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
-- future thought: cache a copy on the connection wrapper
