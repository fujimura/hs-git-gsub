import qualified Cli
import Criterion.Main
import System.Directory
import System.Process

benchmark1 :: IO ()
benchmark1 = do
  withCurrentDirectory "benchmark/git" $ do
    Cli.run ["git", "svn"]
    (exitcode, _, _) <- readProcessWithExitCode "git" ["reset", "--hard"] []
    return ()

main :: IO ()
main =
  defaultMain
    [ bench "benchmark 1" $ whnfIO benchmark1
    ]
