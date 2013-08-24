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
import qualified Filesystem.Path.CurrentOS as F
import Filesystem.Path ((</>))
import qualified Filesystem as F
import System.Posix (openDirStream, readDirStream, closeDirStream)

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
                        yield (path </> file)
                        loop
        loop

visible :: F.FilePath -> Bool
visible path = not $ "." `isPrefixOf` F.encodeString (F.basename path)

isDirectory :: F.FilePath -> ListT (SafeT IO) Bool
isDirectory = lift . lift . F.isDirectory
