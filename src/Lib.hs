{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Control.Concurrent.Async (mapConcurrently_)
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
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
import Text.RE.TDFA.ByteString (RE, compileRegex, (*=~))
import Text.Regex.TDFA (matchTest, (=~))
import Types

run :: Options -> IO ()
run Options {from, to, path, interactive} = do
  hSetBuffering stdin NoBuffering
  targets <- getTargetFiles path
  re <- compileRegex from
  let to' = T.encodeUtf8 . T.pack $ to
  if interactive
    then mapM_ (substituteInteractive re to') targets
    else mapConcurrently_ (substitute re to') targets

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
      let newContent :: BS.ByteString = replaceAll to (content *=~ re)
      BS.writeFile file newContent

substituteInteractive ::
  RE -> -- From
  ByteString -> -- To
  FilePath -> -- File
  IO ()
substituteInteractive re to file = do
  e <- doesFileExist file -- TODO: Test
  when e $ do
    original <- BS.readFile file
    let changed :: ByteString = replaceAll to (original *=~ re)
    withSystemTempFile ("git-gsub" ++ ".") $ \tmpFile hFile -> do
      BS.hPutStr hFile changed
      hClose hFile
      (_, diff, _) <- readProcessWithExitCode "git" ["diff", "--no-index", "--color", file, tmpFile] []
      putStrLn diff
      putStrLn "Apply this change?(y|Enter/n)"
      answer <- getChar
      when (answer `elem` "y\n") $ BS.writeFile file changed
