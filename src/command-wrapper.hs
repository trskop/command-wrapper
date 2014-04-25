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

constructSubsystemEnv :: FilePath -> FilePath -> E [(String, String)]
constructSubsystemEnv wrapperName rootDir = foldl (.) id
    [ (("CMD_WRAPPER_ROOT", rootDir) :)
    , (("CMD_WRAPPER_NAME", wrapperName) :)
    , (("CMD_WRAPPER_CONFIG", "/dev/null") :)
    ]

findSubsystem :: [FilePath] -> FilePath -> IO (Maybe FilePath)
findSubsystem = (fmap listToMaybe .) . findFilesWith isExecutable
  where
    isExecutable = fmap executable . getPermissions

execSubsystem :: FilePath -> FilePath -> FilePath -> [String] -> [(String, String)] -> IO ()
execSubsystem wrapperName rootDir subsystem args env = do
    exe <- findSubsystem [rootDir </> "lib"] subsystem
    case exe of
        Nothing -> subsystemNotFoundError
        Just exe' -> executeFile exe' False args
            . Just $ constructSubsystemEnv wrapperName rootDir env
  where
    subsystemNotFoundError = exitFailure

main :: IO ()
main = do
    (progPath, progName) <- splitExecutablePath
    progAlias <- getProgName
    args <- getArgs
    env <- getEnvironment

    case args of
        subsystem : subsystemArgs ->
            execSubsystem progAlias progPath subsystem subsystemArgs env
        [] -> exitFailure
