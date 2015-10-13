module Lib where

import           Control.Monad
import           Data.Maybe       (maybeToList)
import           Data.Text        (Text)
import qualified Data.Text        as T
import qualified Data.Text.IO     as T
import           System.Directory (doesDirectoryExist, getDirectoryContents)
import           System.Exit
import           System.FilePath  (getSearchPath)
import           System.IO        (BufferMode (NoBuffering), hClose,
                                   hSetBuffering, stdin)
import           System.IO.Temp   (withSystemTempFile)
import           System.Process   (callProcess, readProcessWithExitCode)

run :: String -> String -> Maybe FilePath -> IO ()
run from to path = do
  hSetBuffering stdin NoBuffering
  targets <- getTargetFiles from path
  mapM_ (substitute' from to) targets

getTargetFiles :: String -> Maybe FilePath -> IO [FilePath]
getTargetFiles from path = do
  (_,result,_) <- readProcessWithExitCode "git" (["grep", "-l", from] ++ maybeToList path ) []
  return (lines result)

substitute :: String -> -- From
              String -> -- To
              FilePath -> -- File
              IO ()
substitute from to file =
    T.replace (T.pack from) (T.pack to) <$> T.readFile file >>= T.writeFile file

substitute' :: String -> -- From
              String -> -- To
              FilePath -> -- File
              IO ()
substitute' from to file = do
   original <- T.readFile file
   let changed = T.replace (T.pack from) (T.pack to) original
   withSystemTempFile ("git-gsub" ++ ".") $ \tmpFile hFile -> do
     T.hPutStr hFile changed
     hClose hFile
     (_,diff,_) <- readProcessWithExitCode "git" ["diff", "--no-index", "--color", file, tmpFile] []
     putStrLn diff
     putStrLn "Apply this change?(y|Enter/n)"
     answer <- getChar
     when (answer `elem` "y\n") $ T.writeFile file changed
