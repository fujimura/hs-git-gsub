{-# LANGUAGE OverloadedStrings #-}

module Git.GsubSpec ( main, spec ) where

import qualified Cli
import           Helper

import           System.IO                    (stdout)
import           System.IO.Silently           (hSilence)
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  around_ (hSilence [stdout] . inTempRepo) $ do
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
