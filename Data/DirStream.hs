{-| Use this module to stream directory contents in conjunction with @pipes@.

    The following example shows the simplest possible program you can write
    using @dirstream@: enumerating the contents of a single directory:

> {-# LANGUAGE OverloadedStrings #-}
>
> import Data.DirStream
> import Pipes
> import qualified Pipes.Prelude as P
>
> main = runSafeT $ run $ every (childOf "/tmp") >-> P.show >-> P.stdout

-}

module Data.DirStream
    ( -- * Directory Traversals
      childOf

    -- * Utilities
    , unixVisible
    , isDirectory
    ) where

import Control.Monad (when)
import Data.List (isPrefixOf)
import Pipes (ListT(Select), yield, lift)
import Pipes.Core ((>\\))
import Pipes.Safe (bracket, SafeT)
import System.Directory (readable, getPermissions, doesDirectoryExist)
import qualified Filesystem.Path.CurrentOS as F
import Filesystem.Path ((</>))
import qualified Filesystem as F
import System.Posix (openDirStream, readDirStream, closeDirStream)

-- | Select all children of the given directory, ignoring @\".\"@ and @\"..\"@
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

{-| Determine if a file is visible according to Unix conventions, defined as the
    base name not beginning with a @\'.\'@
-}
unixVisible :: F.FilePath -> Bool
unixVisible path = not $ "." `isPrefixOf` F.encodeString (F.basename path)

-- | Determine if a file is a directory
isDirectory :: F.FilePath -> ListT (SafeT IO) Bool
isDirectory = lift . lift . F.isDirectory
