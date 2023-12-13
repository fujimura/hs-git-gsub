module Types where

data Options = Options
  { from :: FilePath,
    to :: FilePath,
    path :: Maybe FilePath,
    interactive :: Bool
  }
