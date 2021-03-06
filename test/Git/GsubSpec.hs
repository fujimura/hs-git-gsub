{-# LANGUAGE OverloadedStrings #-}

module Git.GsubSpec ( main, spec ) where

import qualified Cli
import qualified Paths_hs_git_gsub

import           Control.Exception  (catch)
import           Data.Version       (showVersion)
import           Helper
import           System.Exit        (ExitCode (ExitSuccess))
import           System.IO          (stdout)
import           System.IO.Silently (capture, hSilence)
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = around_ (hSilence [stdout] . inTempRepo) $ do
  it "should substitute" $ do
    Cli.run ["foo", "bar"]
    actual <- readFile "foo.rb"
    actual `shouldContain` "def bar"

  it "can specify directory" $ do
    Cli.run ["foo", "bar", "foo"]
    actual <- readFile "foo/bar.rb"
    actual `shouldContain` "def bar"

    actual' <- readFile "foo.rb"
    actual' `shouldNotContain` "def bar"

  it "should substitute interactively" $ do
    runWithStdin "y" $ Cli.run ["-i", "foo", "bar"]
    actual <- readFile "foo.rb"
    actual `shouldContain` "def bar"

  it "should show version" $ do
    let run args = fst <$> (capture $ Cli.run args `catch` (\ExitSuccess -> return ()))

    run ["-v"] `shouldReturn` (showVersion Paths_hs_git_gsub.version) ++ "\n"
    run ["--version"] `shouldReturn` (showVersion Paths_hs_git_gsub.version) ++ "\n"
