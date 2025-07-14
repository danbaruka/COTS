{-# LANGUAGE OverloadedStrings #-}

-- | Version management for COTS
module COTS.Version
  ( getVersion,
    getVersionString,
    getGitVersion,
    getBuildInfo,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

-- | Default version if git is not available
defaultVersion :: Text
defaultVersion = "0.1.0.0"

-- | Get the current version (semantic version only)
getVersion :: IO Text
getVersion = return defaultVersion

-- | Get version as a string
getVersionString :: IO String
getVersionString = T.unpack <$> getVersion

-- | Try to get version from git tags
getGitVersion :: IO (Maybe Text)
getGitVersion = do
  result <- tryGitCommand ["describe", "--tags", "--always", "--dirty"]
  case result of
    Just version -> return (Just (T.strip version))
    Nothing -> return Nothing

-- | Get build information
getBuildInfo :: IO Text
getBuildInfo = do
  version <- getVersion
  gitHash <- getGitHash
  gitDate <- getGitDate
  return $
    T.unwords
      [ "COTS v" <> version,
        "(" <> gitHash <> ")",
        "built on " <> gitDate
      ]

-- | Get git commit hash
getGitHash :: IO Text
getGitHash = do
  result <- tryGitCommand ["rev-parse", "--short", "HEAD"]
  case result of
    Just hash -> return (T.strip hash)
    Nothing -> return "unknown"

-- | Get git commit date
getGitDate :: IO Text
getGitDate = do
  result <- tryGitCommand ["log", "-1", "--format=%cd", "--date=short"]
  case result of
    Just date -> return (T.strip date)
    Nothing -> return "unknown"

-- | Try to execute a git command
tryGitCommand :: [String] -> IO (Maybe Text)
tryGitCommand args = do
  result <- tryReadProcess "git" args ""
  case result of
    Right output -> return (Just (T.pack output))
    Left _ -> return Nothing

-- | Try to read process output, returning Left on failure
tryReadProcess :: String -> [String] -> String -> IO (Either String String)
tryReadProcess cmd args input = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode cmd args input
  case exitCode of
    ExitSuccess -> return (Right stdout)
    ExitFailure _ -> return (Left stderr)