module Cli where

import Data.Version (showVersion)
import qualified Lib
import Options.Applicative
import qualified Paths_hs_git_gsub
import Types

run :: [String] -> IO ()
run xs = Cli.parseArgs xs >>= Lib.run

parseArgs :: [String] -> IO Options
parseArgs args = handleParseResult (execParserPure (prefs idm) opts args)
  where
    opts :: ParserInfo Options
    opts =
      info
        (helper <*> (version <*> parseOptions))
        ( fullDesc
            <> header "Generate a haskell project based on a template from github."
            <> progDesc "git-gsub"
        )
    version =
      infoOption
        (showVersion Paths_hs_git_gsub.version)
        ( short 'v'
            <> long "version"
            <> help "Print version information"
        )

parseOptions :: Parser Options
parseOptions =
  Options
    <$> argument str (help "from")
    <*> argument str (help "to")
    <*> argument str (help "path" <> value "./")
