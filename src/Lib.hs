{-# LANGUAGE NamedFieldPuns #-}

module Lib where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO
  ( BufferMode (NoBuffering),
    hClose,
    hSetBuffering,
    stdin,
  )
import System.IO.Temp (withSystemTempFile)
import System.Process (readProcessWithExitCode)
import Types

run :: Options -> IO ()
run Options {from, to, path, interactive} = do
  hSetBuffering stdin NoBuffering
  targets <- getTargetFiles from path
  if interactive
    then mapM_ (substituteInteractive from to) targets
    else mapM_ (substitute from to) targets

getTargetFiles :: String -> FilePath -> IO [FilePath]
getTargetFiles from path = do
  (_, result, _) <- readProcessWithExitCode "git" (["grep", "-l", from, path]) []
  return (lines result)

substitute ::
  String -> -- From
  String -> -- To
  FilePath -> -- File
  IO ()
substitute from to file =
  T.replace (T.pack from) (T.pack to) <$> T.readFile file >>= T.writeFile file

substituteInteractive ::
  String -> -- From
  String -> -- To
  FilePath -> -- File
  IO ()
substituteInteractive from to file = do
  original <- T.readFile file
  let changed = T.replace (T.pack from) (T.pack to) original
  withSystemTempFile ("git-gsub" ++ ".") $ \tmpFile hFile -> do
    T.hPutStr hFile changed
    hClose hFile
    (_, diff, _) <- readProcessWithExitCode "git" ["diff", "--no-index", "--color", file, tmpFile] []
    putStrLn diff
    putStrLn "Apply this change?(y|Enter/n)"
    answer <- getChar
    when (answer `elem` "y\n") $ T.writeFile file changed
