{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Helper where

import           Control.Monad
import           Data.String.Interpolate
import           Data.String.Interpolate.Util
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO
import           System.IO.Silently           (capture, hSilence)
import           System.Process
import           Test.Hspec
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
