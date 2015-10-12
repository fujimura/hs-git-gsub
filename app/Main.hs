module Main where

import           Data.Maybe         (listToMaybe)
import           System.Environment (getArgs)

import qualified Lib

main :: IO ()
main = getArgs >>= run

run :: [String] -> IO ()
run []             = error "No argument to gsub was given"
run ["-v"]         = showVersion
run ["--version"]  = showVersion
run [_]            = error "Please specify `to`"
run (from:to:path) = Lib.run from to (listToMaybe path)

showVersion :: IO ()
showVersion = putStrLn "0.0.1" -- TODO
