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
import System.Directory (doesFileExist)
import System.FilePath ((</>))
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
run Options {from, to, path} = do
  hSetBuffering stdin NoBuffering
  targets <- getTargetFiles path
  re <- compileRegex from
  let to' = T.encodeUtf8 . T.pack $ to
  mapConcurrently_ (substitute re to') targets

getTargetFiles :: FilePath -> IO [FilePath]
getTargetFiles path = do
  (_, result, _) <- readProcessWithExitCode "git" ["ls-files", path] []
  (_, pwd, _) <- readProcessWithExitCode "git" ["rev-parse", "--show-toplevel"] []
  return $ map (trim pwd </>) $ lines result
  where
    trim :: String -> String
    trim = unwords . words

substitute ::
  RE -> -- From
  ByteString -> -- To
  FilePath -> -- File
  IO ()
substitute re to file = do
  e <- doesFileExist file -- TODO: Test
  when e $ do
    content <- BS.readFile file
    when (matchTest (reRegex re) content) $ do
      let newContent = replaceAll to (content *=~ re)
      seq (BS.length newContent) (BS.writeFile file newContent)
