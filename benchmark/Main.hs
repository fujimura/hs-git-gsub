import Criterion.Main
import System.Directory

benchmark1 :: IO ()
benchmark1 = do
  withCurrentDirectory "benchmark/git" $ do
    return ()

main :: IO ()
main =
  defaultMain
    [ bench "benchmark 1" $ whnfIO benchmark1
    ]
