import           System.Process
import           Test.DocTest

main :: IO ()
main = do
    files <- lines <$> readProcess "git" ["ls-files", "src"] []
    doctest $ ["-idist/build/"
              ,"-idist/build/autogen"
              ,"-optP-include"
              ,"-optPdist/build/autogen/cabal_macros.h"
              ] ++ files
