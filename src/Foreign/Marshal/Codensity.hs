{-|
Module      : Foreign.Marshal.Codensity
Description : A Codensity-based wrapper for Haskell-to-C marshalling functions.
Copyright   : (c) Alexis Williams, 2019
License     : MPL-2.0
Maintainer  : alexis@typedr.at
Stability   : provisional
Portability : portable

This library wraps the standard @base@ bracketed allocation primitives (along
with those from "Data.ByteString" and "Data.ByteString.Short") in a
'Codensity'-based interface to ease the chaining of complex marshalling
operations.
-}
module Foreign.Marshal.Codensity
    (
    -- * @alloca@
      alloca, allocaWith, allocaBytes, allocaBytesAligned
    -- * @calloc@
    , calloc, callocBytes
    -- * @allocaArray@
    , allocaArray, allocaArrayWith, allocaArrayWithOf
    , allocaArray0, allocaArrayWith0, allocaArrayWith0Of
    -- * @callocArray@
    , callocArray, callocArray0
    -- * @withForeignPtr@ (and alternatives)
    , withForeignPtr
    , bracketCodensity
    -- * @ToCString@
    , ToCString(..)
    -- * Reexports
    , CString, CStringLen
    , Ptr, nullPtr
    , Codensity(..)
    , lowerCodensity
    -- * Projected variants
    -- | These variants work in the same way as their corresponding functions
    --   without the terminal prime, but with a function argument that projects
    --   the results of the (possibly implicit) 'Fold' into a 'Storable' value.
    --
    --   As in the @lens@ library, the variants beginning with an @i@ use
    --   'IndexedFold's rather than 'Fold's, and no versions without terminal
    --   primes exist because there is no generic enough use for indexes to
    --   give a sensible default.
    , AnIndexedFold
    , allocaArrayWith', allocaArrayWithOf'
    , iallocaArrayWith', iallocaArrayWithOf'
    , allocaArrayWith0', allocaArrayWith0Of'
    , iallocaArrayWith0', iallocaArrayWith0Of'
    ) where

import           Control.Exception     ( finally )
import           Control.Lens.Fold
import           Control.Lens.Getter
import           Control.Lens.Indexed
-- For documentation:
import           Control.Lens.Type     ( IndexedTraversal, IndexedLens )
import           Control.Monad.Codensity
import           Control.Monad.IO.Class
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Short as SBS
import           Data.Foldable         ( foldrM )
import           Data.Functor          ( ($>) )
import qualified Foreign.C.String      as C
import           Foreign.C.String      ( CString, CStringLen )
import qualified Foreign.ForeignPtr    as C
import           Foreign.ForeignPtr    ( ForeignPtr )
import qualified Foreign.Marshal.Alloc as C
import qualified Foreign.Marshal.Array as C
import           Foreign.Marshal.Utils ( fillBytes )
import           Foreign.Ptr
import           Foreign.Storable

-- | 'alloca' @\@a@ is a continuation that provides access to a pointer into a
--   temporary block of memory sufficient to hold values of type @a@.
alloca :: Storable a => Codensity IO (Ptr a)
alloca = Codensity C.alloca
{-# INLINE alloca #-}

-- | 'allocaWith' @a@ is a continuation that provides access to a pointer into
--   a temporary block of memory containing @a@.
allocaWith :: Storable a => a -> Codensity IO (Ptr a)
allocaWith val = do
    ptr <- alloca
    liftIO $ poke ptr val
    return ptr
{-# INLINE allocaWith #-}

-- | 'allocaBytes' @n@ is a continuation that provides access to a pointer into
--   a temporary block of memory sufficient to hold @n@ bytes, with
--   machine-standard alignment.
allocaBytes :: Int -> Codensity IO (Ptr a)
allocaBytes size = Codensity $ C.allocaBytes size
{-# INLINE allocaBytes #-}

-- | 'allocaBytesAligned' @n@ @a@ is a continuation that provides access to a
--   pointer into a temporary block of memory sufficient to hold @n@ bytes, 
--   with @a@-byte alignment.
allocaBytesAligned :: Int -> Int -> Codensity IO (Ptr a)
allocaBytesAligned size align = Codensity $ C.allocaBytesAligned size align
{-# INLINE allocaBytesAligned #-}

-- | 'bracketCodensity' allows one to add initialization and finalization hooks to
--   an existing 'Codensity' that will be executed even in cases where an exception
--   occurs.
--
--   This provides an alternative to 'ForeignPtr's when the pointer will only
--   be used in code that is written with this library in mind.
bracketCodensity :: (a -> IO b) -> (a -> IO c) -> Codensity IO a -> Codensity IO a
bracketCodensity init' final m = do
    a <- m
    liftIO $ init' a
    wrapCodensity (`finally` final a)
    return a
{-# INLINE bracketCodensity #-}

--

-- | 'calloc' @\@a@ is a continuation that provides access to a pointer into a
--   temporary block of zeroed memory sufficient to hold values of type @a@.
calloc :: forall a. Storable a => Codensity IO (Ptr a)
calloc = do
    ptr <- alloca
    let size = sizeOf (undefined :: a)
    liftIO $ fillBytes ptr 0 size
    return ptr
{-# INLINE calloc #-}

-- | 'callocBytes' @n@ is a continuation that provides access to a pointer into
--   a temporary block of zeroed memory sufficient to hold @n@ bytes, with
--   machine-standard alignment.
callocBytes :: Int -> Codensity IO (Ptr a)
callocBytes size = do
    ptr <- allocaBytes size
    liftIO $ fillBytes ptr 0 size
    return ptr
{-# INLINE callocBytes #-}

--

-- | 'allocaArray' @\@a@ @n@ is a continuation that provides access to a
--   pointer into a temporary block of memory sufficient to hold @n@ values of
--   type @a@.
allocaArray :: Storable a => Int -> Codensity IO (Ptr a)
allocaArray size = Codensity (C.allocaArray size)
{-# INLINE allocaArray #-}

-- | 'allocaArrayWith' @xs@ is a continuation that provides access to a
--   pointer into a temporary block of memory containing the values of @xs@.
allocaArrayWith :: (Foldable f, Storable a) => f a -> Codensity IO (Ptr a)
allocaArrayWith = allocaArrayWith' return
{-# INLINE allocaArrayWith #-}

-- Why isn't this defined as `allocaArrayWithOf' folded`? I don't want to
-- lose the potential performance benefits of a specialized `length`.
allocaArrayWith' :: (Foldable f, Storable b) 
                 => (a -> Codensity IO b) 
                 -> f a -> Codensity IO (Ptr b)
allocaArrayWith' f xs = do
    ptr <- allocaArray (length xs)
    _ <- foldrM go ptr xs
    return ptr
    where
        go x ptr = do
            x' <- f x
            liftIO $ poke ptr x'
            return (C.advancePtr ptr 1)
{-# INLINE allocaArrayWith' #-}

-- | 'allocaArrayWithOf' @f@ works in the same way as 'allocaArrayWith', but
--   using the 'Fold' @f@ rather than any 'Foldable' instance.
allocaArrayWithOf :: (Storable a) => Fold s a -> s -> Codensity IO (Ptr a)
allocaArrayWithOf fold = allocaArrayWithOf' fold return 
{-# INLINE allocaArrayWithOf #-}

allocaArrayWithOf' :: (Storable b) 
                   => Fold s a
                   -> (a -> Codensity IO b) 
                   -> s -> Codensity IO (Ptr b)
allocaArrayWithOf' fold f xs = do
    ptr <- allocaArray (lengthOf fold xs)
    _ <- foldrMOf fold go ptr xs
    return ptr
    where
        go x ptr = do
            x' <- f x
            liftIO $ poke ptr x'
            return (C.advancePtr ptr 1)
{-# INLINE allocaArrayWithOf' #-}

iallocaArrayWith' :: (FoldableWithIndex i f, Storable b)
                  => (i -> a -> Codensity IO b)
                  -> f a -> Codensity IO (Ptr b)
iallocaArrayWith' f xs = do
    ptr <- allocaArray (length xs)
    _ <- ifoldrMOf ifolded go ptr xs
    return ptr
    where
        go i x ptr = do
            x' <- f i x
            liftIO $ poke ptr x'
            return (C.advancePtr ptr 1)
{-# INLINE iallocaArrayWith' #-}

-- | A generic 'IndexedFold' or equivalent, taking an 'IndexedGetter',
--   'IndexedFold' (obviously), 'Control.Lens.Type.IndexedTraversal', or
--   'Control.Lens.Type.IndexedLens'.
type AnIndexedFold i s a = forall m p. (Indexable i p)
                        => p a (Const m a) 
                        -> s -> Const m s

iallocaArrayWithOf' :: (Storable b) 
                    => AnIndexedFold i s a
                    -> (i -> a -> Codensity IO b)
                    -> s -> Codensity IO (Ptr b)
iallocaArrayWithOf' fold f xs = do
    ptr <- allocaArray (lengthOf fold xs)
    _ <- ifoldrMOf fold go ptr xs
    return ptr
    where
        go i x ptr = do
            x' <- f i x
            liftIO $ poke ptr x'
            return (C.advancePtr ptr 1)
{-# INLINE iallocaArrayWithOf' #-}

--

-- | 'allocaArray0' @\@a@ @n@ is a continuation that provides access to a
--   pointer into a temporary block of memory sufficient to hold @n@ values of
--   type @a@, along with a final terminal element.
allocaArray0 :: Storable a => Int -> Codensity IO (Ptr a)
allocaArray0 len = Codensity (C.allocaArray0 len)
{-# INLINE allocaArray0 #-}

-- | 'allocaArrayWith0' @xs@ @end@ is a continuation that provides access to a
--   pointer into a temporary block of memory containing the values of @xs@,
--   terminated with @end@.
allocaArrayWith0 :: (Foldable f, Storable a) 
                 => f a -> a -> Codensity IO (Ptr a)
allocaArrayWith0 = allocaArrayWith0' return
{-# INLINE allocaArrayWith0 #-}

allocaArrayWith0' :: (Foldable f, Storable b)
                  => (a -> Codensity IO b)
                  -> f a -> b -> Codensity IO (Ptr b)
allocaArrayWith0' f xs end = do
    ptr <- allocaArray (length xs)
    endPtr <- foldrMOf folded go ptr xs
    liftIO $ poke endPtr end
    return ptr
    where
        go x ptr = do
            x' <- f x
            liftIO $ poke ptr x'
            return (C.advancePtr ptr 1)
{-# INLINE allocaArrayWith0' #-}

-- | 'allocaArrayWith0Of' @t@ works in the same way as 'allocaArrayWith0', but
--   using the 'Fold' @t@ rather than any 'Foldable' instance.
allocaArrayWith0Of :: (Storable a) => Fold s a -> s -> a -> Codensity IO (Ptr a)
allocaArrayWith0Of fold = allocaArrayWith0Of' fold return 
{-# INLINE allocaArrayWith0Of #-}

allocaArrayWith0Of' :: (Storable b) 
                    => Fold s a
                    -> (a -> Codensity IO b) 
                    -> s -> b -> Codensity IO (Ptr b)
allocaArrayWith0Of' fold f xs end = do
    ptr <- allocaArray (lengthOf fold xs)
    endPtr <- foldrMOf fold go ptr xs
    liftIO $ poke endPtr end
    return ptr
    where
        go x ptr = do
            x' <- f x
            liftIO $ poke ptr x'
            return (C.advancePtr ptr 1)
{-# INLINE allocaArrayWith0Of' #-}

iallocaArrayWith0' :: (FoldableWithIndex i f, Storable b)
                   => (i -> a -> Codensity IO b)
                   -> f a -> b -> Codensity IO (Ptr b)
iallocaArrayWith0' f xs end = do
    ptr <- allocaArray (length xs)
    endPtr <- ifoldrMOf ifolded go ptr xs
    liftIO $ poke endPtr end
    return ptr
    where
        go i x ptr = do
            x' <- f i x
            liftIO $ poke ptr x'
            return (C.advancePtr ptr 1)
{-# INLINE iallocaArrayWith0' #-}

iallocaArrayWith0Of' :: (Storable b)
                     => AnIndexedFold i s a
                     -> (i -> a -> Codensity IO b)
                     -> s -> b -> Codensity IO (Ptr b)
iallocaArrayWith0Of' fold f xs end = do
    ptr <- allocaArray (lengthOf fold xs)
    endPtr <- ifoldrMOf fold go ptr xs
    liftIO $ poke endPtr end
    return ptr
    where
        go i x ptr = do
            x' <- f i x
            liftIO $ poke ptr x'
            return (C.advancePtr ptr 1)
{-# INLINE iallocaArrayWith0Of' #-}

--

-- | 'callocArray' @\@a@ @n@ is a continuation that provides access to a
--   pointer into a temporary block of zeroed memory sufficient to hold @n@
--   values of type @a@.
callocArray :: forall a . Storable a => Int -> Codensity IO (Ptr a)
callocArray len = do
    ptr <- allocaArray len
    let size = sizeOf (undefined :: a)
    liftIO $ fillBytes ptr 0 (len * size)
    return ptr
{-# INLINE callocArray #-}

-- | 'callocArray0' @\@a@ @n@ is a continuation that provides access to a
--   pointer into a temporary block of zeroed memory sufficient to hold @n@
--   values of type @a@, along with a final terminal element.
callocArray0 :: forall a . Storable a => Int -> Codensity IO (Ptr a)
callocArray0 len = do
    ptr <- allocaArray0 len
    let size = sizeOf (undefined :: a)
    liftIO $ fillBytes ptr 0 (len * size)
    return ptr
{-# INLINE callocArray0 #-}

--

-- | 'withForeignPtr' @ptr@ is a continuation that provides safe access to the
--   backing pointer of @ptr@.
withForeignPtr :: ForeignPtr a -> Codensity IO (Ptr a)
withForeignPtr ptr = Codensity (C.withForeignPtr ptr)
{-# INLINE withForeignPtr #-}

--

-- | 'ToCString' @a@ is a class for types @a@ that can be encoded into 
--   'CString's.
class ToCString a where
    -- | 'withCString' @a@ is a continuation that provides access to @a@ as a
    --   'CString'.
    withCString    :: a -> Codensity IO CString
    -- | 'withCStringLen' @a@ is a continuation that provides access to @a@ as a
    --   'CStringLen'.
    withCStringLen :: a -> Codensity IO CStringLen

instance ToCString String where
    withCString s    = Codensity (C.withCString s)
    {-# INLINE withCString #-}
    
    withCStringLen s = Codensity (C.withCStringLen s)
    {-# INLINE withCStringLen #-}

instance ToCString BS.ByteString where
    withCString s    = Codensity (BS.useAsCString s)
    {-# INLINE withCString #-}

    withCStringLen s = Codensity (BS.useAsCStringLen s)
    {-# INLINE withCStringLen #-}

instance ToCString SBS.ShortByteString where
    withCString    s = Codensity (SBS.useAsCString s)
    {-# INLINE withCString #-}

    withCStringLen s = Codensity (SBS.useAsCStringLen s)
    {-# INLINE withCStringLen #-}
