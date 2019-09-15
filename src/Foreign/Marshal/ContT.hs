{-|
Module      : Foreign.Marshal.ContT
Description : A ContT-based wrapper for Haskell-to-C marshalling functions.
Copyright   : (c) Alexis Williams, 2019
License     : MPL-2.0
Maintainer  : alexis@typedr.at
Stability   : provisional
Portability : portable

This library wraps the standard @base@ bracketed allocation primitives (along
with those from 'Data.ByteString') in a 'ContT'-based interface to ease the
chaining of complex marshalling operations.
-}
module Foreign.Marshal.ContT 
    ( 
    -- * @alloca@
      alloca, allocaWith, allocaBytes, allocaBytesAligned
    -- * @calloc@
    , calloc, callocBytes
    -- * @allocaArray@
    , allocaArray, allocaArrayWith, allocaArray0, allocaArrayWith0
    -- * @callocArray@
    , callocArray, callocArray0
    -- * @withForeignPtr@
    , withForeignPtr
    -- * @ToCString@
    , ToCString(..)
    ) where

import           Control.Monad.Cont
import           Control.Monad.IO.Class
import qualified Data.ByteString       as BS 
import qualified Data.ByteString.Short as SBS 
import           Data.Foldable
import           Foreign.C.String      ( CString, CStringLen )
import qualified Foreign.C.String      as C
import qualified Foreign.ForeignPtr    as C
import           Foreign.ForeignPtr    ( ForeignPtr )
import qualified Foreign.Marshal.Alloc as C
import qualified Foreign.Marshal.Array as C
import           Foreign.Ptr
import           Foreign.Storable

-- | 'alloca' is a continuation that provides access to a pointer into a
--   temporary block of memory sufficient to hold values of type @a@.
alloca :: Storable a => ContT r IO (Ptr a)
alloca = ContT C.alloca
{-# INLINE alloca #-}

-- | 'allocaWith' @a@ is a continuation that provides access to a pointer into
--   a temporary block of memory containing @a@.
allocaWith :: Storable a => a -> ContT r IO (Ptr a)
allocaWith val = do
    ptr <- alloca
    liftIO $ poke ptr val
    return ptr

-- | 'allocaBytes' @n@ is a continuation that provides access to a pointer into
--   a temporary block of memory sufficient to hold @n@ bytes, with
--   machine-standard alignment.
allocaBytes :: Int -> ContT r IO (Ptr a)
allocaBytes size = ContT $ C.allocaBytes size
{-# INLINE allocaBytes #-}

-- | 'allocaBytesAligned' @n@ @a@ is a continuation that provides access to a
--   pointer into a temporary block of memory sufficient to hold @n@ bytes, 
--   with @a@-byte alignment.
allocaBytesAligned :: Int -> Int -> ContT r IO (Ptr a)
allocaBytesAligned size alignment = ContT $ C.allocaBytesAligned size alignment
{-# INLINE allocaBytesAligned #-}

--

-- | 'calloc' is a continuation that provides access to a pointer into a
--   temporary block of zeroed memory sufficient to hold values of type @a@.
calloc :: forall r a. Storable a => ContT r IO (Ptr a)
calloc = ContT $ \f -> do
    ptr <- C.calloc
    out <- f ptr
    C.free ptr
    return out
{-# INLINE calloc #-}

-- | 'callocBytes' @n@ is a continuation that provides access to a pointer into
--   a temporary block of zeroed memory sufficient to hold @n@ bytes, with
--   machine-standard alignment.
callocBytes :: Int -> ContT r IO (Ptr a)
callocBytes size = ContT $ \f -> do
    ptr <- C.callocBytes size
    out <- f ptr
    C.free ptr
    return out
{-# INLINE callocBytes #-}

--

-- | 'allocaArray' @\@a@ @n@ is a continuation that provides access to a
--   pointer into a temporary block of memory sufficient to hold @n@ values of
--   type @a@.
allocaArray :: Storable a => Int -> ContT r IO (Ptr a)
allocaArray = ContT . C.allocaArray
{-# INLINE allocaArray #-}

-- | 'allocaArrayWith' @xs@ is a continuation that provides access to a
--   pointer into a temporary block of memory containing the values of @xs@.
allocaArrayWith :: (Traversable t, Storable a) => t a -> ContT r IO (Ptr a)
allocaArrayWith t = do
    ptr <- allocaArray (length t)
    liftIO $ foldrM go ptr t
    return ptr
    where
        go x ptr = do
            poke ptr x
            return (C.advancePtr ptr 1)
{-# INLINE allocaArrayWith #-}

-- | 'allocaArray0' @\@a@ @n@ is a continuation that provides access to a
--   pointer into a temporary block of memory sufficient to hold @n@ values of
--   type @a@, along with a final terminal element.
allocaArray0 :: Storable a => Int -> ContT r IO (Ptr a)
allocaArray0 = ContT . C.allocaArray0
{-# INLINE allocaArray0 #-}

-- | 'allocaArrayWith' @xs@ @end@ is a continuation that provides access to a
--   pointer into a temporary block of memory containing the values of @xs@,
--   terminated with @end@.
allocaArrayWith0 :: (Traversable t, Storable a) => t a -> a -> ContT r IO (Ptr a)
allocaArrayWith0 t end = do
    ptr <- allocaArray0 (length t)
    endPtr <- liftIO $ foldrM go ptr t
    liftIO $ poke endPtr end
    return ptr
    where
        go x ptr = do
            poke ptr x
            return (C.advancePtr ptr 1)
{-# INLINE allocaArrayWith0 #-}

--

-- | 'callocArray0' @\@a@ @n@ is a continuation that provides access to a
--   pointer into a temporary block of zeroed memory sufficient to hold @n@
--   values of type @a@.
callocArray :: Storable a => Int -> ContT r IO (Ptr a)
callocArray len = ContT $ \f -> do
    ptr <- C.callocArray len
    out <- f ptr
    C.free ptr
    return out
{-# INLINE callocArray #-}

-- | 'callocArray0' @\@a@ @n@ is a continuation that provides access to a
--   pointer into a temporary block of zeroed memory sufficient to hold @n@
--   values of type @a@, along with a final terminal element.
callocArray0 :: Storable a => Int -> ContT r IO (Ptr a)
callocArray0 len = ContT $ \f -> do
    ptr <- C.callocArray0 len
    out <- f ptr
    C.free ptr
    return out
{-# INLINE callocArray0 #-}

--

-- | 'withForeignPtr' @ptr@ is a continuation that provides safe access to the
--   backing pointer of @ptr@.
withForeignPtr :: ForeignPtr a -> ContT r IO (Ptr a)
withForeignPtr = ContT . C.withForeignPtr
{-# INLINE withForeignPtr #-}

--

-- | 'ToCString' @a@ is a class for types @a@ that can be encoded into 
--   'CString's.
class ToCString a where
    -- | 'withCString' @a@ is a continuation that provides access to @a@ as a
    --   'CString'.
    withCString    :: a -> ContT r IO CString
    -- | 'withCStringLen' @a@ is a continuation that provides access to @a@ as a
    --   'CStringLen'.
    withCStringLen :: a -> ContT r IO CStringLen

instance ToCString String where
    withCString    = ContT . C.withCString
    {-# INLINE withCString #-}
    
    withCStringLen = ContT . C.withCStringLen
    {-# INLINE withCStringLen #-}

instance ToCString BS.ByteString where
    withCString    = ContT . BS.useAsCString
    {-# INLINE withCString #-}

    withCStringLen = ContT . BS.useAsCStringLen
    {-# INLINE withCStringLen #-}

instance ToCString SBS.ShortByteString where
    withCString    = ContT . SBS.useAsCString
    {-# INLINE withCString #-}

    withCStringLen = ContT . SBS.useAsCStringLen
    {-# INLINE withCStringLen #-}
