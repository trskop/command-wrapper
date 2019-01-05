-- |
-- Module:      CommandWrapper.Internal.Subcommand.Completion
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018-2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- TODO: Module description.
module CommandWrapper.Internal.Subcommand.Completion
    ( CompletionMode(..)
    , completion
    , completionSubcommandHelp
    )
  where

import Control.Applicative (pure)
import Control.Monad ((>>=), join)
import Data.Bool (Bool(False))
import qualified Data.Char as Char (toLower)
import Data.Eq ((==))
import Data.Foldable (mapM_)
import Data.Function (($), (.), const, id)
import Data.Functor (Functor, (<$>), fmap)
import qualified Data.List as List
    ( break
    , filter
    , isPrefixOf
    , lookup
    , nub
    , takeWhile
    )
import Data.Maybe (Maybe(..))
import Data.Monoid (Endo(Endo))
import Data.Semigroup ((<>))
import Data.String (String, unlines)
import Data.Tuple (fst)
import GHC.Generics (Generic)
import System.IO (IO, putStrLn)
import Text.Show (Show)

import qualified Mainplate (applySimpleDefaults)
import qualified Safe (headMay)
import Text.Fuzzy as Fuzzy (Fuzzy)
import qualified Text.Fuzzy as Fuzzy (Fuzzy(original), filter)

import qualified CommandWrapper.Config.Global as Global (Config(..))
import CommandWrapper.Environment (AppNames(AppNames, usedName))
import qualified CommandWrapper.External as External (findSubcommands)
import CommandWrapper.Internal.Utils (runMain)
--import CommandWrapper.Message
import CommandWrapper.Options.Alias (Alias(alias))


data Shell = Bash
  deriving (Generic, Show)

newtype WhatArgumentsToList
    = Subcommands String
  deriving (Generic, Show)
    -- TODO: Extend this data type to be able to list various combinations of
    -- following groups:
    --
    -- - External subcommands
    -- - Internal subcommands
    -- - Aliases
    -- - Global options

data CompletionMode a
    = CompletionMode String a
    | GenerateCompletionScriptMode Shell a
    | ListArgumentsMode WhatArgumentsToList a
  deriving (Functor, Generic, Show)

completion :: AppNames -> [String] -> Global.Config -> IO ()
completion appNames options globalConfig =
    runMain parseOptions defaults $ \case
        -- TODO:
        --
        -- - This subcommand will need access to global parser definition to
        --   provide completion for global options.
        --
        -- - When completing subcommand names we need to find possible
        --   completions in:
        --
        --     - All possible external and internal subcommands.
        --     - All aliases defined by `command-wrapper` and `TOOLSET` (if not
        --       invoking `command-wrapper` directly).
        --
        -- - When completing option/argument of a subcommand we need to execute
        --   the subcommand with bash completion options passed to it. This
        --   will require us to extend `SUBCOMMAND_PROTOCOL.md`. Should we rely
        --   on `optparse-applicative` for this?
        CompletionMode pat cfg -> do
            let subcmds = findSubcommands cfg pat

            completions <-
                case Safe.headMay pat of
                    Nothing  -> (findOptions cfg ('-' : pat) <>) <$> subcmds
                    Just '-' -> pure (findOptions cfg pat)
                    _        -> subcmds

            mapM_ putStrLn completions

        -- TODO:
        --
        -- - Completion script should be configurable.
        --
        -- - By default it should be printed to `stdout`, but we should support
        --   writting it into a file without needing to redirect `stdout`.
        GenerateCompletionScriptMode _shell _config -> pure ()

        ListArgumentsMode whatToList cfg -> case whatToList of
            Subcommands pat ->
                findSubcommandsFuzzy cfg pat
                    >>= mapM_ (putStrLn . Fuzzy.original)
  where
    defaults = Mainplate.applySimpleDefaults (CompletionMode "" globalConfig)

    parseOptions :: IO (Endo (CompletionMode Global.Config))
    parseOptions = do
        let pat = case options of
                []           -> ""
                ["--"]       -> ""
                "--" : p : _ -> p
                p : _        -> p

        -- TODO: Temporary hack to make this subcommand somewhat useful.
        pure . Endo $ const (CompletionMode pat globalConfig)

    getSubcommands :: Global.Config -> IO [String]
    getSubcommands cfg = do
        extCmds <- External.findSubcommands appNames cfg

        let aliases = alias <$> Global.aliases cfg
            internalCommands = ["help", "config", "completion"]

        pure (List.nub $ aliases <> internalCommands <> extCmds)

    findSubcommandsFuzzy :: Global.Config -> String -> IO [Fuzzy String String]
    findSubcommandsFuzzy cfg pat = do
        cmds <- getSubcommands cfg

        let caseSensitiveSearch = False -- TODO: Move to config.
        pure (Fuzzy.filter pat cmds "" "" id caseSensitiveSearch)

    findSubcommands :: Global.Config -> String -> IO [String]
    findSubcommands cfg pat =
        List.filter (fmap Char.toLower pat `List.isPrefixOf`)
            <$> getSubcommands cfg

    findOptions :: Global.Config -> String -> [String]
    findOptions cfg pat =
        let verbosityValues =  ["silent", "normal", "verbose", "annoying"]
            colourWhences = ["always", "auto", "never"]

            shortOptions =
                [ "-v", "-vv", "-vvv"
                , "-s"
                ]

            longOptions :: [(String, Maybe (String -> [String]))]
            longOptions =
                [ ("--verbosity=", Just (findKeywords verbosityValues cfg))
                , ("--color=", Just (findKeywords colourWhences cfg))
                , ("--colour=", Just (findKeywords colourWhences cfg))
                , ("--no-color", Nothing)
                , ("--no-colour", Nothing)
                ]

        in  case List.takeWhile (== '-') pat of
                -- No option starts with "---"
                '-' : '-' : '-' : _ -> []

                -- Search for long option.
                '-' : '-' : pat' ->
                    case List.break (== '=') pat' of
                        (opt, '=' : pat'') ->
                            case join (List.lookup ("--" <> opt <> "=") longOptions) of
                                Just f -> f pat''
                                _ -> []
                        _ ->
                            List.filter
                                (fmap Char.toLower pat `List.isPrefixOf`)
                                (fst <$> longOptions)

                -- Search for both, long and short option.
                '-' : _ ->
                    List.filter (fmap Char.toLower pat `List.isPrefixOf`)
                        (shortOptions <> fmap fst longOptions)

                _ -> []

    findKeywords :: [String] -> Global.Config -> String -> [String]
    findKeywords keywords _cfg pat =
        List.filter (fmap Char.toLower pat `List.isPrefixOf`) keywords

completionSubcommandHelp :: AppNames -> String
completionSubcommandHelp AppNames{usedName} = unlines
    [ "Usage:"
    , ""
    , "  " <> usedName <> " [GLOBAL_OPTIONS] completion [--] [WORD]"
    , ""
    , "Options:"
    , ""
    , "  SUBCOMMAND"
    , "    Name of a subcommand for which to show help message."
    , ""
    , "Global options:"
    , ""
    , "  See output of '" <> usedName <> " help'."
    ]

