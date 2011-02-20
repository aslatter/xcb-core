-- -*-haskell-*-

{-# LANGUAGE
   StandaloneDeriving
  ,GeneralizedNewtypeDeriving
  #-}

module Graphics.X11.Xcb.Internal where

import Control.Monad
import Foreign
import Foreign.C.String
import Foreign.C.Types

import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy as L

{#import Foreign.IOVec#}

#include <xcb/xcb.h>
#include <xcb/xcbext.h>

peekIntConv ptr = fromIntegral `liftM` peek ptr

{-
  Welcome to my sometimes-dodgy FFI bindings to XCB.
  The idea isn't really to write something to directly
  write X apps with, but to write a foundation for 
  something higher-level and safer.

  So referential transparency can definetly be abused
  with a lot of the calls in here.

  So don't do it.
 -}

{#context lib="xcb" prefix="xcb_"#}

{#pointer *connection_t as Connection foreign newtype#}
{#pointer *setup_t as Setup newtype#} -- no finalizer, part of the Connection

-- | Returns a connection to the indicated X server, as well
-- as the screen connected to.
-- The passed in string may be null, in which case
-- it will be the same as passing in the DISPLAY environment
-- variable.
connect :: String -> IO (Maybe (Connection, Int))
connect display
    = withCString display $ \displayPtr ->
      alloca $ \screenPtr -> do

      cPtr <- {#call connect as connect_#} displayPtr screenPtr
      screen <- peekIntConv screenPtr

      if cPtr == nullPtr then return Nothing else do

      c <- mkConnection cPtr
      return $ Just (c, screen)

-- | Parse the host, display and screen from a string.
-- The same as what we would do if the string were
-- passed to 'connect'.
parseDisplay :: String -> IO (Maybe (String, Int, Int))
parseDisplay display
    = withCString display $ \displayPtr ->
      alloca $ \hostStringPtr ->
      alloca $ \displayNumPtr ->
      alloca $ \screenPtr -> do
      success <- liftM toBool $
                 {#call unsafe parse_display#} displayPtr hostStringPtr displayNumPtr screenPtr
      if not success then return Nothing else do

      host <- peek hostStringPtr >>= peekCString
      peek hostStringPtr >>= free

      displayNum <- peekIntConv displayNumPtr
      screen <- peekIntConv screenPtr

      return $ Just (host, displayNum, screen)
      
-- | Flush the write buffers.
flush :: Connection -> IO Bool
flush c = withConnection c $ \cPtr ->
          liftM toBool $ {#call flush as flush_#} cPtr

-- | The memory backing this data structure is owned
-- by the passed in Connection - and will magicaly vanish
-- if your 'Connection' gets GCd before the return value
-- of this function.  Watch out.
unsafeGetSetup :: Connection -> IO Setup
unsafeGetSetup c = withConnection c $ \cPtr ->
                   {#call unsafe get_setup#} cPtr

-- | Length of the setup data in units of four bytes.
setupLength :: Setup -> IO Word16
setupLength (Setup s) = liftM fromIntegral $ {#get setup_t->length#} s

-- | This ByteString is just as unsafe as the passed in connection
-- object.  Please make a copy.
unsafeSetupData :: Setup -> IO S.ByteString
unsafeSetupData s@(Setup sPtr) = do
  len <- liftM (4*) $ setupLength s
  sFPtr <- newForeignPtr_ sPtr
  return $ S.fromForeignPtr (castForeignPtr sFPtr) 0 (fromIntegral len)

-- | Maximum size of a single request.
-- May not be an asynchronous call, as we'll make a call out
-- on the big-requests extension to find out if we can use that.
maximumRequestLength :: Connection -> IO Word32
maximumRequestLength c = withConnection c $ \cPtr ->
                         liftM fromIntegral $ {#call get_maximum_request_length#} cPtr

-- | Asynchronous version of 'maximumRequestLength'.  The return
-- from the server is cached for subsequent callse to 'maximumRequestLength'.
prefetchMaximumReuestLength :: Connection -> IO ()
prefetchMaximumReuestLength c = withConnection c $ \cPtr ->
                                {#call prefetch_maximum_request_length#} cPtr

-- | Is the connection in an error state?
connectionHasError :: Connection -> IO Bool
connectionHasError c = withConnection c $ \cPtr ->
                       liftM toBool $ {#call connection_has_error#} cPtr

newtype Cookie = Cookie CUInt

-- Working with extensions
{#pointer *extension_t as Extension newtype#} -- static data, no finalizer

-- :-(
instance Storable Extension where
    sizeOf (Extension ex) = sizeOf ex
    alignment (Extension ex) = alignment ex
    peekElemOff ptr off = liftM Extension $ peekElemOff (castPtr ptr) off
    pokeElemOff ptr off (Extension ex) = pokeElemOff (castPtr ptr) off ex
    peekByteOff ptr off = liftM Extension $ peekByteOff ptr off
    pokeByteOff ptr off (Extension ex) = pokeByteOff ptr off ex
    peek ptr = liftM Extension $ peek (castPtr ptr)
    poke ptr (Extension ex) = poke (castPtr ptr) ex

{#pointer *query_extension_reply_t as ExtensionInfo newtype#} -- freed with Connection, no finalizer

-- | May block if the data is not already cached.  For internal
-- use only - make a copy if you need something that will live
-- longer than the scope of the connection.
unsafeGetExtensionData :: Connection -> Extension -> IO ExtensionInfo
unsafeGetExtensionData c ext
    = withConnection c $ \cPtr ->
      {#call get_extension_data#} cPtr ext

extensionPresent :: ExtensionInfo -> IO Bool
extensionPresent (ExtensionInfo ext)
    = liftM toBool $ {#get query_extension_reply_t->present#} ext

extensionMajorOpcode :: ExtensionInfo -> IO Word8
extensionMajorOpcode (ExtensionInfo ext)
    = liftM fromIntegral $ {#get query_extension_reply_t->major_opcode#} ext

extensionFirstEvent :: ExtensionInfo -> IO Word8
extensionFirstEvent (ExtensionInfo ext)
    = liftM fromIntegral $ {#get query_extension_reply_t->first_event#} ext

extensionFirstError :: ExtensionInfo -> IO Word8
extensionFirstError (ExtensionInfo ext)
    = liftM fromIntegral $ {#get query_extension_reply_t->first_error#} ext

-- | Non-blocking query for extension data
prefetchExtension :: Connection -> Extension -> IO ()
prefetchExtension c ext
    = withConnection c $ \cPtr ->
      {#call prefetch_extension_data#} cPtr ext

class HasResponseType a where
    responseType :: a -> IO Int

isBigEvent :: HasResponseType a => a -> IO Bool
isBigEvent ev = (== 35) `liftM` responseType ev

isError :: HasResponseType a => a -> IO Bool
isError ev = (== 2) `liftM` responseType ev

-- Events
{#pointer *generic_event_t as GenericEvent foreign newtype#}
{#pointer *ge_event_t as GenericBigEvent foreign newtype#}

-- | The XCB documentation seems to imply that this can also return
-- errors, so be careful.
waitForEvent :: Connection -> IO GenericEvent
waitForEvent c = withConnection c $ \cPtr -> do
                 evPtr <- {#call wait_for_event#} cPtr
                 liftM GenericEvent $ newForeignPtr finalizerFree evPtr

instance HasResponseType GenericEvent where
    responseType = eventResponseType

eventResponseType :: GenericEvent -> IO Int
eventResponseType ev = withGenericEvent ev $ \evPtr ->
                       liftM fromIntegral $ {#get generic_event_t->response_type#} evPtr

-- | The conversion is not checked.             
unsafeToBigEvent :: GenericEvent -> GenericBigEvent
unsafeToBigEvent (GenericEvent ev) = GenericBigEvent . castForeignPtr $ ev

-- | The conversion is not checked.
unsafeToError :: GenericEvent -> GenericError
unsafeToError (GenericEvent ev) = GenericError . castForeignPtr $ ev

-- | In units of four bytes.  Extra +1 for the 'full_sequence' field
-- introduced when XCB decodes from the wire.
bigEventLength :: GenericBigEvent -> IO Word32
bigEventLength bge = withGenericBigEvent bge $ \bgePtr -> do
                     lenField <- fromIntegral `liftM` {#get ge_event_t->length#} bgePtr
                     return $ 8 + 1 + lenField

-- | Unsafe in that referential transperency may be broken - 
-- the ByteString is aliased to the same memory location as the
-- the event.
unsafeEventData :: GenericEvent -> IO S.ByteString
unsafeEventData ge = do
  isError <- isError ge
  if isError then unsafeErrorData (unsafeToError ge) else do

  isBig <- isBigEvent ge
  if isBig
     then do
       let bge@(GenericBigEvent bgeFPtr) = unsafeToBigEvent ge
       len <- bigEventLength bge
       return $ S.fromForeignPtr (castForeignPtr bgeFPtr) 0 (4 * fromIntegral len)
     else
       let GenericEvent evFPtr = ge
       in return $ S.fromForeignPtr (castForeignPtr evFPtr) 0 32

-- Errors
{#pointer *generic_error_t as GenericError foreign newtype#}

requestCheck :: Connection -> Cookie -> IO (Maybe GenericError)
requestCheck c cookie
    = withConnection c $ \cPtr -> do
      errPtr <- xcb_request_check cPtr cookie
      if errPtr == nullPtr then return Nothing else do
      liftM (Just . GenericError) $ newForeignPtr finalizerFree errPtr

foreign import ccall xcb_request_check :: Ptr Connection -> Cookie -> IO (Ptr GenericError)

-- | The returned ByteString is aliased to the same memory
-- location as the GenericError.
unsafeErrorData :: GenericError -> IO S.ByteString
unsafeErrorData (GenericError errFPtr) =
    return $ S.fromForeignPtr (castForeignPtr errFPtr) 0 32

-- Replies
{#pointer *generic_reply_t as GenericReply foreign newtype#}

-- | Returns the length of the reply in units of four bytes
replyLength :: GenericReply -> IO Word32
replyLength rep = withGenericReply rep $ \repPtr -> do
                  lenField <- liftM fromIntegral $ {#get generic_reply_t->length#} repPtr
                  return $ lenField + 1 -- for the inserted full_sequence field

instance HasResponseType GenericReply where
    responseType = replyResponseType

replyResponseType :: GenericReply -> IO Int
replyResponseType rep = withGenericReply rep $ \repPtr ->
                        liftM fromIntegral $ {#get generic_reply_t->response_type#} repPtr

-- | Unchecked conversion
unsafeReplyToError :: GenericReply -> GenericError
unsafeReplyToError (GenericReply rep) = GenericError . castForeignPtr $ rep

-- | The returned ByteString is aliased to the same memory
-- location as the 'GenericReply'
unsafeReplyData :: GenericReply -> IO S.ByteString
unsafeReplyData rep@(GenericReply repFPtr) = do
  len <- fromIntegral `liftM` replyLength rep
  return $ S.fromForeignPtr (castForeignPtr repFPtr) 0 (4*len)

-- Sending requests
{#pointer *protocol_request_t as RequestInfo foreign newtype#}

mkRequestInfo :: (Maybe Extension) -> CUInt -> Bool -> IO RequestInfo
mkRequestInfo ext opcode isVoid = do
      fptr <- mallocForeignPtrBytes {#sizeof protocol_request_t#}
      withForeignPtr fptr $ \ptr -> do
        {#set protocol_request_t->ext#} ptr $ maybe (Extension nullPtr) id ext
        {#set protocol_request_t->opcode#} ptr (fromIntegral opcode)
        {#set protocol_request_t->isvoid#} ptr (fromBool isVoid)
      return $ RequestInfo fptr
      
{#enum send_request_flags_t as RequestFlags {underscoreToCase} deriving(Eq, Show)#}


-- |Note: the 'count' field in the request info is always
-- over-written to the number of chunks in the ByteString
sendRequest :: Connection -> Int -> L.ByteString -> RequestInfo -> IO Cookie
sendRequest c flags bytes rInfo
    = withConnection c $ \cPtr ->
      withRequestInfo rInfo $ \rPtr ->
      withLazyByteString bytes $ \vec vecNum ->
      withIOVec vec $ \vecPtr -> do
      {#set protocol_request_t->count#} rPtr (fromIntegral vecNum)
      liftM (Cookie . fromIntegral) $ {#call send_request#} cPtr (fromIntegral flags) (castPtr vecPtr) rPtr

waitForReply :: Connection -> Cookie -> IO (Either GenericError GenericReply)
waitForReply c (Cookie request) =
    withConnection c $ \cPtr ->
    alloca $ \errPtrPtr -> do
    repPtr <- {#call wait_for_reply#} cPtr (fromIntegral request) errPtrPtr
    if repPtr == nullPtr then do
        errPtr <- peek errPtrPtr
        errFPtr <- newForeignPtr finalizerFree errPtr
        return . Left . GenericError $ errFPtr
     else do
       repFPtr <- newForeignPtr finalizerFree $ castPtr repPtr
       return . Right . GenericReply $ repFPtr


-- Other
generateId :: Connection -> IO Word32
generateId c = withConnection c $ \cPtr ->
               liftM fromIntegral $ {#call generate_id#} cPtr


-- Internal utils

-- | Create a 'Connection' from a reference.
-- The connection will be shut down when it goes out of scope.
mkConnection :: Ptr Connection -> IO Connection
mkConnection = liftM Connection . newForeignPtr finalizerDisconnect

-- | Creates a connection from an unsafe reference,
-- with no asociated finalzer.
-- Only use this if the Haskell code does not own the connection.
mkConnection_ :: Ptr Connection -> IO Connection
mkConnection_ = liftM Connection . newForeignPtr_

-- | A finalizer for a Ptr Connection which shuts down the connection
-- and frees all resources associated with it.
finalizerDisconnect :: FinalizerPtr Connection
finalizerDisconnect = xcb_disconnect

foreign import ccall "&xcb_disconnect" xcb_disconnect :: FunPtr (Ptr Connection -> IO ())
