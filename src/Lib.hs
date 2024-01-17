{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Control.Concurrent.Async (mapConcurrently_)
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import System.Directory (doesFileExist)
import System.IO
  ( BufferMode (NoBuffering),
    hClose,
    hSetBuffering,
    stdin,
  )
import System.IO.Temp (withSystemTempFile)
import System.Process (readProcessWithExitCode)
import Text.RE.Replace (replaceAll)
import Text.RE.TDFA.Text (RE, compileRegex, (*=~))
import Types

run :: Options -> IO ()
run Options {from, to, path, interactive} = do
  hSetBuffering stdin NoBuffering
  targets <- getTargetFiles path
  re <- compileRegex from
  let to' = T.pack to
  if interactive
    then mapM_ (substituteInteractive re to') targets
    else mapConcurrently_ (substitute re to') targets

getTargetFiles :: FilePath -> IO [FilePath]
getTargetFiles path = do
  (_, result, _) <- readProcessWithExitCode "git" ["ls-files", path] []
  return (lines result)

substitute ::
  RE -> -- From
  T.Text -> -- To
  FilePath -> -- File
  IO ()
substitute re to file = do
  e <- doesFileExist file -- TODO: Test
  when e $ do
    b <- BS.readFile file
    case T.decodeUtf8' b of
      Left _ -> return ()
      Right content -> do
        let newContent :: T.Text = replaceAll to (content *=~ re)
        T.writeFile file newContent

substituteInteractive ::
  RE -> -- From
  T.Text -> -- To
  FilePath -> -- File
  IO ()
substituteInteractive re to file = do
  e <- doesFileExist file -- TODO: Test
  when e $ do
    original <- T.readFile file
    let changed :: T.Text = replaceAll to (original *=~ re)
    withSystemTempFile ("git-gsub" ++ ".") $ \tmpFile hFile -> do
      T.hPutStr hFile changed
      hClose hFile
      (_, diff, _) <- readProcessWithExitCode "git" ["diff", "--no-index", "--color", file, tmpFile] []
      putStrLn diff
      putStrLn "Apply this change?(y|Enter/n)"
      answer <- getChar
      when (answer `elem` "y\n") $ T.writeFile file changed
