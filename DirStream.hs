module DirStream (
    -- * Directory Traversals
    contents,
    ls,

    -- * Utilities
    visible,
    directory
    ) where

import Control.Monad (when)
import Data.List (isPrefixOf)
import qualified Pipes as P
import Pipes ((>\\))
import Pipes.Safe (SafeIO, tryIO, bracket)
import qualified Pipes.Prelude as P
import System.Directory (readable, getPermissions, doesDirectoryExist)
import System.FilePath ((</>), takeFileName)
import System.Posix (openDirStream, readDirStream, closeDirStream)

contents :: FilePath -> P.ListT SafeIO FilePath
contents path = P.RespondT $ do
    canRead <- tryIO $ fmap readable $ getPermissions path
    when canRead $ bracket (openDirStream path) closeDirStream $ \dirp -> do
        let loop = do
                file <- tryIO $ readDirStream dirp
                case file of
                    [] -> return ()
                    _  -> do
                        P.respond (path </> file)
                        loop
        loop

ls :: P.ListT' SafeIO FilePath -> () -> P.Producer FilePath SafeIO ()
ls l () = (\_ -> return ()) >\\ P.runRespondT l

visible :: FilePath -> P.ListT SafeIO Bool
visible path = return $ not $ "." `isPrefixOf` takeFileName path

directory :: FilePath -> P.ListT SafeIO Bool
directory = P.lift . tryIO . doesDirectoryExist
