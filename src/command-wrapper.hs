-- |
-- Module:       Main
-- Description:  <Short text displayed on contents page>
-- Copyright:    (c) 2014 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  portable | non-portable (<reason>)
--
-- <module description starting at first column>
module Main (main)
    where

import Data.Maybe (listToMaybe)
import System.Environment (getArgs, getEnvironment, getProgName)
import System.Exit (exitFailure)

import System.FilePath ((</>))
import System.Directory (findFilesWith, executable, getPermissions)
import System.Posix.Process (executeFile)

import System.Environment.Executable (splitExecutablePath)


type E a = a -> a

constructSubcommandEnv :: FilePath -> FilePath -> E [(String, String)]
constructSubcommandEnv wrapperName rootDir = foldl (.) id
    [ (("CMD_WRAPPER_ROOT", rootDir) :)
    , (("CMD_WRAPPER_NAME", wrapperName) :)
    , (("CMD_WRAPPER_CONFIG", "/dev/null") :)
    ]

findSubcommand :: [FilePath] -> FilePath -> IO (Maybe FilePath)
findSubcommand = (fmap listToMaybe .) . findFilesWith isExecutable
  where
    isExecutable = fmap executable . getPermissions

execSubcommand :: FilePath -> FilePath -> FilePath -> [String] -> [(String, String)] -> IO ()
execSubcommand wrapperName rootDir subcommand args env = do
    exe <- findSubcommand [rootDir </> "lib"] subcommand
    case exe of
        Nothing -> subcommandNotFoundError
        Just exe' -> executeFile exe' False args
            . Just $ constructSubcommandEnv wrapperName rootDir env
  where
    subcommandNotFoundError = exitFailure

main :: IO ()
main = do
    (progPath, progName) <- splitExecutablePath
    progAlias <- getProgName
    args <- getArgs
    env <- getEnvironment

    case args of
        subcommand : subcommandArgs ->
            execSubcommand progAlias progPath subcommand subcommandArgs env
        [] -> exitFailure
