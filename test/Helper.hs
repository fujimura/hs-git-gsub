{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Helper where

import           Control.Exception            (finally)
import           Data.String.Interpolate
import           Data.String.Interpolate.Util (unindent)
import           GHC.IO.Handle                (hDuplicate, hDuplicateTo)
import           System.Directory             (createDirectoryIfMissing)
import           System.FilePath              (takeDirectory)
import           System.IO                    (IOMode (ReadMode), hClose,
                                               hPutStrLn, openFile, stdin)
import           System.IO
import           System.IO.Temp               (withSystemTempFile)
import           System.Process               (system)
import           Test.Mockery.Directory       (inTempDirectory)

inTempRepo :: IO a -> IO a
inTempRepo action = inTempDirectory $ do
    system "git init ."
    write "foo.rb" [i|
                   class Foo
                     def foo
                     end
                   end
                   |]
    write "foo/bar.rb" [i|
                       class Foo::Bar
                         def foo
                         end
                       end
                       |]
    system "git add ."
    system "git commit -m \"Init\""
    action
  where
    write :: FilePath -> String -> IO ()
    write path content = do
      createDirectoryIfMissing True (takeDirectory path)
      writeFile path (unindent content)

runWithStdin :: String -> IO () -> IO ()
runWithStdin input action = withSystemTempFile "runWithInput" $ \path handle -> do
    -- Write input to temporary file and get its handle
    hPutStrLn handle input
    hClose handle
    handle' <- openFile path ReadMode
    -- Keep original stdin
    stdin' <- hDuplicate stdin
    -- Set handle of temporary file to stdin
    hDuplicateTo handle' stdin
    finally action $ do
      -- Recover stdin
      hDuplicateTo stdin' stdin
      hClose handle'
