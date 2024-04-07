module Types where

data Options = Options
  { from :: FilePath,
    to :: FilePath,
    path :: FilePath,
    rename :: Bool
  }
