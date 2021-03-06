-- -*-haskell-*-

{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}

{- |
 I reserve the right to make this module private at any time.
 Specifically, this module will become private once I can do
 so without breaking c2hs preprocessing.
 -}
module Foreign.IOVec
    (IOVec()
    ,withLazyByteString
    ,withByteString
    ,withIOVec
    ,writev
    )
    where

import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Unsafe as S
import qualified Data.ByteString.Lazy as L

import Foreign
import Foreign.C.Types

#include <sys/uio.h>

#c
typedef struct iovec hs_iovec;
#endc

-- | The IOVev type is a newtype around
-- an FFI `Ptr`, so you can pass it as is
-- to FFI calls.
{#pointer *hs_iovec as IOVec newtype#}

withIOVec :: IOVec -> (Ptr IOVec -> IO a) -> IO a
withIOVec (IOVec ptr) f = f ptr
{-# INLINE withIOVec #-}

withByteString :: S.ByteString -> (IOVec -> Int -> IO b) -> IO b
withByteString b = withLazyByteString (L.fromChunks [b])

-- | This is intented for calling into something like writev.
-- But I suppose that nothing stops you from calling into
-- readv and blowing away the passed-in ByteString. Don't do that.
--
-- The number passed to the callback is the number of blocks
-- in the `IOVec`.
--
-- The spin of the lazy bytestring is forced prior to traversal.
withLazyByteString :: L.ByteString -> (IOVec -> Int -> IO b) -> IO b
withLazyByteString lbs f =
    let bs = L.toChunks lbs
        num = length bs
    in allocaBytes (num * {#sizeof hs_iovec#}) $
           \vecAry -> do
             fill vecAry 0 bs
             x <- f (IOVec vecAry) num
             touchAllBS bs
             return x

   where fill _vecAry !_off [] = return ()
         fill vecAry !off (b:bs) =

             let vec = vecAry `plusPtr` (off * {#sizeof hs_iovec#})

             in do
               S.unsafeUseAsCStringLen b $ \(bsptr,bslen) -> do
                  {#set hs_iovec->iov_base#} vec $ castPtr $ bsptr
                  {#set hs_iovec->iov_len#}  vec $ fromIntegral bslen
               fill vecAry (off+1) bs

-- utility functions
touchBS :: S.ByteString -> IO ()
touchBS b = case S.toForeignPtr b of (fptr,_,_) -> touchForeignPtr fptr

touchAllBS :: [S.ByteString] -> IO ()
touchAllBS xs = mapM_ touchBS xs

-- | Binding to writev using 'withLazyByteString'. For testing.
writev :: CInt -> L.ByteString -> IO CSize
writev fd bs = withLazyByteString bs $ \iov count ->
               c_writev fd iov (fromIntegral count)

foreign import ccall unsafe "writev" c_writev :: CInt -> IOVec -> CInt -> IO CSize
