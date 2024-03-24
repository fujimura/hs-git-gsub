{-# LANGUAGE OverloadedStrings #-}

module Git.GsubSpec (main, spec) where

import qualified Cli
import Control.Exception (catch)
import Data.Version (showVersion)
import Helper
import qualified Paths_hs_git_gsub
import System.Exit (ExitCode (ExitSuccess))
import System.IO (stdout)
import System.IO.Silently (capture, hSilence)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = around_ (hSilence [stdout] . inTempRepo) $ do
  it "should substitute" $ do
    Cli.run ["foo", "bar"]
    actual <- readFile "foo.rb"
    actual `shouldContain` "def bar"

  it "should substitute using regular expression" $ do
    Cli.run ["f.o", "bar"]
    actual <- readFile "foo.rb"
    actual `shouldContain` "def bar"

  it "should substitute using regular expression with capture" $ do
    Cli.run ["f(oo)", "f$1$1"]
    actual <- readFile "foo.rb"
    actual `shouldContain` "def foooo"

  it "can specify directory" $ do
    Cli.run ["foo", "bar", "foo"]
    actual <- readFile "foo/bar.rb"
    actual `shouldContain` "def bar"

    actual' <- readFile "foo.rb"
    actual' `shouldNotContain` "def bar"

  it "should show version" $ do
    let run args = fst <$> (capture $ Cli.run args `catch` (\ExitSuccess -> return ()))

    run ["-v"] `shouldReturn` (showVersion Paths_hs_git_gsub.version) ++ "\n"
    run ["--version"] `shouldReturn` (showVersion Paths_hs_git_gsub.version) ++ "\n"
