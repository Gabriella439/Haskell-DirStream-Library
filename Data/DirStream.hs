module Data.DirStream
    ( -- * Directory Traversals

      childOf

    -- * Utilities
    , visible
    , isDirectory
    ) where

import Control.Monad (when)
import Data.List (isPrefixOf)
import Pipes (ListT(Select), yield, lift)
import Pipes.Core ((>\\))
import Pipes.Safe (bracket, SafeT)
import System.Directory (readable, getPermissions, doesDirectoryExist)
import System.FilePath ((</>), takeFileName)
import System.Posix (openDirStream, readDirStream, closeDirStream)

childOf :: FilePath -> ListT (SafeT IO) FilePath
childOf path = Select $ do
    canRead <- lift $ lift $ fmap readable $ getPermissions path
    when canRead $ bracket (openDirStream path) closeDirStream $ \dirp -> do
        let loop = do
                file <- lift $ lift $ readDirStream dirp
                case file of
                    [] -> return ()
                    _  -> do
                        yield (path </> file)
                        loop
        loop

visible :: FilePath -> Bool
visible path = not $ "." `isPrefixOf` takeFileName path

isDirectory :: FilePath -> ListT (SafeT IO) Bool
isDirectory = lift . lift . doesDirectoryExist
