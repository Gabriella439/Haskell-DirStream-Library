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
import Pipes (ListT(Select), yield, lift)
import Pipes.Core ((>\\))
import Pipes.Safe (bracket, SafeT)
import System.Directory (readable, getPermissions, doesDirectoryExist)
import qualified Filesystem.Path.CurrentOS as F
import Filesystem.Path ((</>))
import Filesystem (isDirectory)
import System.Posix (openDirStream, readDirStream, closeDirStream)

{- $traversals
    There many possible recursion schemes for traversing directories.  Rather
    than provide them all, I prefer that you learn to assemble your own
    recursion schemes, using the source code for 'descendentOf' as a starting
    point.
-}

{-| Select all immediate children of the given directory, ignoring @\".\"@ and
    @\"..\"@
-}
childOf :: F.FilePath -> ListT (SafeT IO) F.FilePath
childOf path = Select $ do
    let path' = F.encodeString path
    canRead <- lift $ lift $ fmap readable $ getPermissions path'
    when canRead $ bracket (openDirStream path') closeDirStream $ \dirp -> do
        let loop = do
                file' <- lift $ lift $ readDirStream dirp
                case file' of
                    [] -> return ()
                    _  -> do
                        let file = F.decodeString file'
                        when (file' /= "." && file' /= "..") $
                            yield (path </> file)
                        loop
        loop
{-# INLINABLE childOf #-}

-- | Select all recursive descendents of the given directory
descendentOf :: F.FilePath -> ListT (SafeT IO) F.FilePath
descendentOf path = do
    child <- childOf path
    isDir <- lift $ lift $ isDirectory child
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
-- > main1 = runSafeT $ run $
-- >     every (childOf "/tmp") >-> P.show >-> hoist lift P.stdout
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
-- >     isDir <- lift $ lift $ isDirectory child
-- >     if isDir
-- >         then return child <|> descendentOf child
-- >         else return child
--
--  These recursive traversals will promptly open and close nested directory
--  streams as they traverse the directory tree:
--
-- > main2 = runSafeT $ run $
-- >     every (descendentOf "/tmp") >-> P.show >-> hoist lift P.stdout
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
-- > main3 = runSafeT $ run $
-- >     every (descendentOf "/tmp") >-> P.take 3 >-> P.show >-> hoist lift P.stdout
--
-- >>> main3  -- This never opens the "/tmp/dir2" directory
-- FilePath "/tmp"
-- FilePath "/tmp/dir1"
-- FilePath "/tmp/dir1/fileA"
