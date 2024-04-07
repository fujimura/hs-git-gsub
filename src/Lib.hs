{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Control.Concurrent.Async (mapConcurrently_)
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.FilePath (dropFileName, (</>))
import System.IO
  ( BufferMode (NoBuffering),
    hClose,
    hSetBuffering,
    stdin,
  )
import System.IO.Temp (withSystemTempFile)
import System.Process (readProcessWithExitCode)
import Text.RE.Replace (replaceAll)
import Text.RE.TDFA (RE, reRegex, (*=~))
import qualified Text.RE.TDFA.ByteString.Lazy
import qualified Text.RE.TDFA.String
import Text.Regex.TDFA (matchTest, (=~))
import Types

run :: Options -> IO ()
run Options {from, to, path, rename} = do
  hSetBuffering stdin NoBuffering
  targets <- getTargetFiles path
  bsRE <- Text.RE.TDFA.ByteString.Lazy.compileRegex from
  stringRE <- Text.RE.TDFA.String.compileRegex from
  let to' = T.encodeUtf8 . T.pack $ to
  mapConcurrently_ (runSubstitution bsRE to') targets
  when rename $ mapConcurrently_ (runRename stringRE to) targets

getTargetFiles :: FilePath -> IO [FilePath]
getTargetFiles path = do
  (_, result, _) <- readProcessWithExitCode "git" ["ls-files", path] []
  (_, pwd, _) <- readProcessWithExitCode "git" ["rev-parse", "--show-toplevel"] []
  return $ map (trim pwd </>) $ lines result
  where
    trim :: String -> String
    trim = unwords . words

runSubstitution ::
  RE -> -- From
  ByteString -> -- To
  FilePath -> -- File
  IO ()
runSubstitution re to file = do
  e <- doesFileExist file -- TODO: Test
  when e $ do
    content <- BS.readFile file
    when (matchTest (reRegex re) content) $ do
      let newContent = replaceAll to (content *=~ re)
      seq (BS.length newContent) (BS.writeFile file newContent)

runRename ::
  RE -> -- From
  String -> -- To
  FilePath -> -- File
  IO ()
runRename re to path = do
  e <- doesFileExist path -- TODO: Test
  when (e && matchTest (reRegex re) path) $ do
    let newPath = replaceAll to (path *=~ re)
    createDirectoryIfMissing True (dropFileName newPath)
    renameFile path newPath
