module Cli where

import           Options.Applicative
import           System.Environment  (getArgs)
import           Data.Version        (showVersion)

import qualified Lib
import qualified Paths_hs_git_gsub
import           Types

run :: IO ()
run = getArgs >>= run'

run' :: [String] -> IO ()
run' xs = Cli.parseArgs xs >>= Lib.run

parseArgs :: [String] -> IO Options
parseArgs args = handleParseResult (execParserPure (prefs idm) opts args)
  where
    opts :: ParserInfo Options
    opts = info ( helper <*> (version <*> parseOptions))
           ( fullDesc
           <> header "Generate a haskell project based on a template from github."
           <> progDesc "git-gsub")
    version = infoOption (showVersion Paths_hs_git_gsub.version)
      (  short 'v'
      <> long "version"
      <> help "Print version information" )

parseOptions :: Parser Options
parseOptions = Options
   <$> argument str (help "from")
   <*> argument str (help "to")
   <*> optional (argument str (help "path"))
   <*> switch (long "interactive" <> short 'i' <> help "Run interactively")
