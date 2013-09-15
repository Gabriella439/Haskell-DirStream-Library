{-# LANGUAGE TypeFamilies, CPP, OverloadedStrings #-}

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
import Data.List (isPrefixOf)
import Pipes (ListT(Select), yield, liftIO)
import Pipes.Safe (bracket, MonadSafe, Base)
import System.Directory (readable, getPermissions)
import qualified Filesystem.Path.CurrentOS as F
import Filesystem.Path ((</>))
import Filesystem (isDirectory)
#ifdef mingw32_HOST_OS
import System.Win32 (
    findFirstFile, findClose, getFindDataFileName, findNextFile )
#else
import System.Posix (openDirStream, readDirStream, closeDirStream)
#endif

{- $traversals
    There many possible recursion schemes for traversing directories.  Rather
    than provide them all, I prefer that you learn to assemble your own
    recursion schemes, using the source code for 'descendentOf' as a starting
    point.
-}

{-| Select all immediate children of the given directory, ignoring @\".\"@,
    @\"..\"@, and files without read permissions
-}
childOf :: (MonadSafe m, Base m ~ IO) => F.FilePath -> ListT m F.FilePath
childOf path = Select $ do
    let path' = F.encodeString path
    canRead <- liftIO $ fmap readable $ getPermissions path'
    when canRead $
#ifdef mingw32_HOST_OS
        bracket
            (findFirstFile (F.encodeString (path </> "*")))
            (\(h, _) -> findClose h)
            $ \(h, fdat) -> do
                let loop = do
                        filename <- liftIO $ getFindDataFileName fdat
                        yield (F.decodeString filename)
                        more     <- liftIO $ findNextFile h fdat
                        when more loop
                loop
#else
        bracket (openDirStream path') closeDirStream $ \dirp -> do
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
descendentOf :: (MonadSafe m, Base m ~ IO) => F.FilePath -> ListT m F.FilePath
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
-- > import qualified Pipes.Prelude as P
-- >
-- > main1 = runSafeT $ runEffect $ every (childOf "/tmp") >-> P.print
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
-- > main2 = runSafeT $ runEffect $ every (descendentOf "/tmp") >-> P.print
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
-- > main3 = runSafeT $ runEffect $
-- >     every (descendentOf "/tmp") >-> P.take 3 >-> P.print
--
-- >>> main3  -- This never opens the "/tmp/dir2" directory
-- FilePath "/tmp"
-- FilePath "/tmp/dir1"
-- FilePath "/tmp/dir1/fileA"
