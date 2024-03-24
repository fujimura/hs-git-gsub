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
import System.FilePath ((</>) , dropFileName)
import System.IO
  ( BufferMode (NoBuffering),
    hClose,
    hSetBuffering,
    stdin,
  )
import System.IO.Temp (withSystemTempFile)
import System.Process (readProcessWithExitCode)
import Text.RE.Replace (replaceAll)
import Text.RE.TDFA (reRegex)
import Text.RE.TDFA.ByteString.Lazy (RE, compileRegex, (*=~))
import Text.Regex.TDFA (matchTest, (=~))
import Types

run :: Options -> IO ()
run Options {from, to, path, rename} = do
  hSetBuffering stdin NoBuffering
  targets <- getTargetFiles path
  re <- compileRegex from
  let to' = T.encodeUtf8 . T.pack $ to
  mapConcurrently_ (processFile re to' rename) targets

getTargetFiles :: FilePath -> IO [FilePath]
getTargetFiles path = do
  (_, result, _) <- readProcessWithExitCode "git" ["ls-files", path] []
  (_, pwd, _) <- readProcessWithExitCode "git" ["rev-parse", "--show-toplevel"] []
  return $ map (trim pwd </>) $ lines result
  where
    trim :: String -> String
    trim = unwords . words

processFile ::
  RE -> -- From
  ByteString -> -- To
  Bool -> -- Rename
  FilePath -> -- File
  IO ()
processFile re to rename path = do
  e <- doesFileExist path -- TODO: Test
  when e $ do
    substitute re to path
    when (rename && matchTest (reRegex re) path) $ rename' re to path
  where
    substitute :: RE -> ByteString -> FilePath -> IO ()
    substitute re to path = do
      content <- BS.readFile path
      when (matchTest (reRegex re) content) $ do
        let newContent = replaceAll to (content *=~ re)
        seq (BS.length newContent) (BS.writeFile path newContent)
    rename' :: RE -> ByteString -> FilePath -> IO ()
    rename' re to path = do
      let newPath = T.unpack . T.decodeUtf8 $ replaceAll to ((T.encodeUtf8 . T.pack) path *=~ re)
      createDirectoryIfMissing True (dropFileName newPath)
      renameFile path newPath
