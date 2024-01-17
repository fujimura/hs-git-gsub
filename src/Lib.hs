{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Text.RE.Replace (replaceAll)
import Text.RE.TDFA.Text (RE, compileRegex, (*=~))
import Types

run :: Options -> IO ()
run Options {from, to, path, interactive} = do
  hSetBuffering stdin NoBuffering
  targets <- getTargetFiles from path
  re <- compileRegex from
  let to' = T.pack to
  if interactive
    then mapM_ (substituteInteractive re to') targets
    else mapM_ (substitute re to') targets

getTargetFiles :: String -> FilePath -> IO [FilePath]
getTargetFiles from path = do
  (_, result, _) <- readProcessWithExitCode "git" (["grep", "-l", from, path]) []
  return (lines result)

substitute ::
  RE -> -- From
  T.Text -> -- To
  FilePath -> -- File
  IO ()
substitute re to file = do
  content <- T.readFile file
  let newContent :: T.Text = replaceAll to (content *=~ re)
  T.writeFile file newContent

substituteInteractive ::
  RE -> -- From
  T.Text -> -- To
  FilePath -> -- File
  IO ()
substituteInteractive re to file = do
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
