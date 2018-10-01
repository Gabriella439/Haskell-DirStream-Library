{-# LANGUAGE CPP, OverloadedStrings, FlexibleContexts #-}

{-| Use this module to stream directory contents lazily in constant memory in
    conjunction with @pipes@
-}

module Data.DirStream
    ( -- * Directory Traversals
      -- $traversals
      childOf
    , descendentOf

    -- * Utilities
    , unixVisible
    , isDirectory

    -- * Tutorial
    -- $tutorial
    ) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.Fail (MonadFail)
#ifdef mingw32_HOST_OS
import Data.Bits ((.&.))
#endif
import Data.List (isPrefixOf)
import Pipes (ListT(Select), yield, liftIO)
import Pipes.Safe (bracket, MonadSafe)
import System.Directory (readable, getPermissions)
import qualified Filesystem.Path.CurrentOS as F
import Filesystem.Path ((</>))
import Filesystem (isDirectory)
#ifdef mingw32_HOST_OS
import qualified System.Win32 as Win32
#else
import System.Posix (openDirStream, readDirStream, closeDirStream)
#endif

{- $traversals
    There many possible recursion schemes for traversing directories.  Rather
    than provide them all, I prefer that you learn to assemble your own
    recursion schemes, using the source code for 'descendentOf' as a starting
    point.
-}

#ifdef mingw32_HOST_OS
fILE_ATTRIBUTE_REPARSE_POINT :: Win32.FileAttributeOrFlag
fILE_ATTRIBUTE_REPARSE_POINT = 1024

reparsePoint :: Win32.FileAttributeOrFlag -> Bool
reparsePoint attr = fILE_ATTRIBUTE_REPARSE_POINT .&. attr /= 0
#endif

{-| Select all immediate children of the given directory, ignoring @\".\"@ and
    @\"..\"@.

    Returns zero children if the directory is not readable or (on Windows) if
    the directory is actually a reparse point.
-}
childOf :: (MonadSafe m, MonadFail m) => F.FilePath -> ListT m F.FilePath
childOf path = Select $ do
    let path' = F.encodeString path
    canRead <- liftIO $ fmap readable $ getPermissions path'
#ifdef mingw32_HOST_OS
    reparse <- liftIO $ fmap reparsePoint $ Win32.getFileAttributes path'
    when (canRead && not reparse) $
        bracket
            (liftIO $ Win32.findFirstFile (F.encodeString (path </> "*")))
            (\(h, _) -> liftIO $ Win32.findClose h)
            $ \(h, fdat) -> do
                let loop = do
                        file' <- liftIO $ Win32.getFindDataFileName fdat
                        let file = F.decodeString file'
                        when (file' /= "." && file' /= "..") $
                            yield (path </> file)
                        more  <- liftIO $ Win32.findNextFile h fdat
                        when more loop
                loop
#else
    when (canRead) $
        bracket (liftIO $ openDirStream path') (liftIO . closeDirStream) $
            \dirp -> do
            let loop = do
                    file' <- liftIO $ readDirStream dirp
                    case file' of
                        [] -> return ()
                        _  -> do
                            let file = F.decodeString file'
                            when (file' /= "." && file' /= "..") $
                                yield (path </> file)
                            loop
            loop
#endif
{-# INLINABLE childOf #-}

-- | Select all recursive descendents of the given directory
descendentOf :: (MonadSafe m, MonadFail m) => F.FilePath -> ListT m F.FilePath
descendentOf path = do
    child <- childOf path
    isDir <- liftIO $ isDirectory child
    if isDir
        then return child <|> descendentOf child
        else return child
{-# INLINABLE descendentOf #-}

{-| Determine if a file is visible according to Unix conventions, defined as the
    base name not beginning with a @\'.\'@
-}
unixVisible :: F.FilePath -> Bool
unixVisible path = not $ "." `isPrefixOf` F.encodeString (F.basename path)
{-# INLINABLE unixVisible #-}

-- $tutorial
--  The following example shows a simple program that enumerates the contents of
--  a single directory:
-- 
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Data.DirStream
-- > import Pipes
-- > import Pipes.Safe
-- >
-- > main1 = runSafeT $ runEffect $
-- >     for (every (childOf "/tmp")) (liftIO . print)
--
-- >>> main1
-- FilePath "/tmp"
-- FilePath "/tmp/dir1"
-- FilePath "/tmp/dir2"
-- FilePath "/tmp/fileE"
--
--  The 'childOf' function streams the list of files in constant memory,
--  allowing you to traverse very large directory lists.
--
--  You can use 'ListT' to assemble more sophisticated traversals, such as the
--  recursive 'descendentOf' traversal, which has the following definition:
--
-- > descendentOf :: F.FilePath -> ListT (SafeT IO) F.FilePath
-- > descendentOf path = do
-- >     child <- childOf path
-- >     isDir <- liftIO $ isDirectory child
-- >     if isDir
-- >         then return child <|> descendentOf child
-- >         else return child
--
--  These recursive traversals will promptly open and close nested directory
--  streams as they traverse the directory tree:
--
-- > main2 = runSafeT $ runEffect $
-- >     for (every (descendentOf "/tmp")) (liftIO . print)
--
-- >>> main2
-- FilePath "/tmp"
-- FilePath "/tmp/dir1"
-- FilePath "/tmp/dir1/fileA"
-- FilePath "/tmp/dir1/fileB"
-- FilePath "/tmp/dir2"
-- FilePath "/tmp/dir2/fileC"
-- FilePath "/tmp/dir2/fileD"
-- FilePath "/tmp/fileE"
--
--  These traverals are lazy and will open the minimal number of directories
--  necessary to satisfy downstream demand:
--
-- > import qualified Pipes.Prelude as P
-- >
-- > main3 = runSafeT $ runEffect $
-- >     for (every (descendentOf "/tmp") >-> P.take 3) (liftIO . print)
--
-- >>> main3  -- This never opens the "/tmp/dir2" directory
-- FilePath "/tmp"
-- FilePath "/tmp/dir1"
-- FilePath "/tmp/dir1/fileA"
