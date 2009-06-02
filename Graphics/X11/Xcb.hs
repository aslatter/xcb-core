

module Graphics.X11.Xcb
 (I.Connection
 ,I.connect
 ,I.parseDisplay
 ,I.flush
 ,I.maximumRequestLength
 ,I.prefetchMaximumReuestLength
 ,I.connectionHasError
 ,sendRequest
 ,I.RequestFlags(..)
 ,I.RequestInfo
 ,I.mkRequestInfo
 ,I.Cookie
 ,requestCheck
 ,waitForReply
 ,I.generateId
 ,getSetup
 ,I.Extension
 ,I.prefetchExtension
 ,extensionPresent
 ,extensionMajorOpcode
 ,extensionFirstEvent
 ,extensionFirstError
 ,withForeignConnection
 ,I.withConnection
 ) where

import qualified Graphics.X11.Xcb.Internal as I
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Control.Applicative ((<$>))
import Data.Bits((.&.))
import Data.List (foldl')
import Data.Word
import Foreign(Ptr)

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

-- | Send an XCB request, consisting of the passed in Bytes. 
sendRequest :: I.Connection -> [I.RequestFlags] -> L.ByteString -> I.RequestInfo -> IO I.Cookie
sendRequest c flagsList bytes rInfo
    = let flags = foldl' (.&.) 0 (map (fromIntegral . fromEnum) flagsList)
      in I.sendRequest c flags bytes rInfo 

-- | If the passed in request has generated an error (and the request
-- was set checked) this will return the data for the error.
requestCheck :: I.Connection -> I.Cookie -> IO (Maybe S.ByteString)
requestCheck c cookie
    = do
  resp <- I.requestCheck c cookie
  case resp of
    Nothing -> return Nothing
    Just err ->
        Just <$> I.unsafeErrorData err

-- | Returns an event or error from the queue.
-- Errors associated with checked requests will not
-- be available from this call.
waitForEvent :: I.Connection -> IO S.ByteString
waitForEvent c = I.waitForEvent c >>= I.unsafeEventData

waitForReply :: I.Connection -> I.Cookie -> IO (Either S.ByteString S.ByteString)
waitForReply c cookie
    = do
  resp <- I.waitForReply c cookie
  case resp of
    Left err  -> Left <$> I.unsafeErrorData err
    Right rep -> Right <$> I.unsafeReplyData rep

extensionPresent :: I.Connection -> I.Extension -> IO Bool
extensionPresent c e
    = I.unsafeGetExtensionData c e >>= I.extensionPresent

extensionMajorOpcode :: I.Connection -> I.Extension -> IO Word8
extensionMajorOpcode c e
    = I.unsafeGetExtensionData c e >>= I.extensionMajorOpcode

extensionFirstEvent :: I.Connection -> I.Extension -> IO Word8
extensionFirstEvent c e
    = I.unsafeGetExtensionData c e >>= I.extensionFirstEvent

extensionFirstError :: I.Connection -> I.Extension -> IO Word8
extensionFirstError c e
    = I.unsafeGetExtensionData c e >>= I.extensionFirstError

getSetup :: I.Connection -> IO S.ByteString
getSetup c = S.copy <$> (I.unsafeGetSetup c >>= I.unsafeSetupData)
