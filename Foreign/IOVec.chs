-- -*-haskell-*-

{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}

module Foreign.IOVec
    (IOVec()
    ,withByteString
    ,withLazyByteString
    ,withIOVec
    ,writev
    )
    where

import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy as L

import Foreign
import Foreign.C.Types

#include <sys/uio.h>

#c
typedef struct iovec hs_iovec;
#endc

{#pointer *hs_iovec as IOVec newtype#}

withIOVec :: IOVec -> (Ptr IOVec -> IO a) -> IO a
withIOVec (IOVec ptr) f = f ptr
{-# INLINE withIOVec #-}

withByteString :: S.ByteString -> (IOVec -> Int -> IO b) -> IO b
withByteString b = withLazyByteString (L.fromChunks [b])

-- | This is intented for calling into something like writev.
-- But I suppose that nothing stops you from calling into
-- readv and blowing away the passed-in ByteString.
withLazyByteString :: L.ByteString -> (IOVec -> Int -> IO b) -> IO b
withLazyByteString b f =
    let bs = L.toChunks b
        num = length bs
    in allocaBytes (num * {#sizeof hs_iovec#}) $ \vecAry -> go vecAry 0 bs

   where go vecAry !off [] = f (IOVec vecAry) off
         go vecAry !off (b:bs) =

             let vec = vecAry `plusPtr` (off * {#sizeof hs_iovec#})
                 (fptr, bsOff, bsLen) = S.toForeignPtr b

             in withForeignPtr fptr $ \bsPtr -> do
                  {#set hs_iovec->iov_base#} vec $ castPtr $ bsPtr `plusPtr` bsOff
                  {#set hs_iovec->iov_len#}  vec $ fromIntegral bsLen
                  go vecAry (off+1) bs

-- | Binding to writev using 'withLazyByteString'
writev :: CInt -> L.ByteString -> IO CSize
writev fd bs = withLazyByteString bs $ \iov count ->
               c_writev fd iov (fromIntegral count)

foreign import ccall unsafe "writev" c_writev :: CInt -> IOVec -> CInt -> IO CSize
