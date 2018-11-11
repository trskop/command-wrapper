{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
-- |
-- Module:      CommandWrapper.Internal
-- Description: Internal commands supported by CommandWrapper
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Internal commands supported by CommandWrapper.
module CommandWrapper.Internal
    (
    -- * Internal Commands
      Command(..)
    , run
    , command

    -- ** Help Command
    , HelpMode(..)
    , help

    -- ** Config Command
    , ConfigMode(..)
    , config

    -- ** Completion Command
    , CompletionMode(..)
    , completion

    -- * Generic Functions
    , runMain
    )
  where

import Control.Applicative (pure)
import Control.Monad ((>>=), join)
import Data.Bool (Bool(False))
import qualified Data.Char as Char (toLower)
import Data.Eq ((==))
import Data.Foldable (mapM_, traverse_)
import Data.Function (($), (.), const, id)
import Data.Functor (Functor, (<$>), fmap)
import qualified Data.List as List (break, filter, isPrefixOf, lookup, nub, takeWhile)
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid (Endo(Endo))
import Data.Semigroup ((<>))
import Data.String (String, unlines)
import Data.Tuple (fst)
import GHC.Generics (Generic)
import System.Exit (die)
import System.IO (IO, putStr, putStrLn)
import Text.Show (Show)

import qualified Mainplate (applySimpleDefaults, noConfigToRead, runAppWith)
import qualified Safe (headMay)
import Text.Fuzzy as Fuzzy (Fuzzy)
import qualified Text.Fuzzy as Fuzzy (Fuzzy(original), filter)

import qualified CommandWrapper.Config as Global (Config(..))
import CommandWrapper.Environment (AppNames(AppNames, usedName))
import qualified CommandWrapper.External as External
    ( executeCommand
    , findSubcommands
    )
import CommandWrapper.Options.Alias (Alias(alias), applyAlias)


data Command
    = HelpCmommand [String]
    | ConfigCommand [String]
    | CompletionCommand [String]
  deriving (Generic, Show)

-- | Smart constructor for 'Command'.
command
    :: String
    -- ^ Subcommand name.
    -> [String]
    -- ^ Subcommand arguments.
    -> Maybe Command
    -- ^ Return 'Nothing' if subcommand is not an internal command.
command = \case
    "help" -> Just . HelpCmommand
    "config" -> Just . ConfigCommand
    "completion" -> Just . CompletionCommand
    _ -> const Nothing

run :: AppNames -> Command -> Global.Config -> IO ()
run appNames = \case
    HelpCmommand options -> help appNames options
    ConfigCommand options -> config appNames options
    CompletionCommand options -> completion appNames options

-- {{{ Help Command -----------------------------------------------------------

-- TODO:
--
-- - Support displaying manual pages. Integration with `pandoc`?

data HelpMode a
    = MainHelp a
    | SubcommandHelp String a
  deriving (Functor, Generic, Show)

help :: AppNames -> [String] -> Global.Config -> IO ()
help appNames@AppNames{usedName} options globalConfig =
    runMain parseOptions defaults $ \case
        MainHelp Global.Config{Global.extraHelpMessage} -> do
            putStr helpMsg
            traverse_ putStrLn extraHelpMessage

        SubcommandHelp subcommand cfg ->
            External.executeCommand appNames subcommand ["--help"] cfg
  where
    defaults = Mainplate.applySimpleDefaults (MainHelp globalConfig)

    parseOptions :: IO (Endo (HelpMode Global.Config))
    parseOptions = case options of
        [] ->
            switchTo (MainHelp globalConfig)

        [subcmd] ->
            let (subcmd', _) =
                    applyAlias (Global.aliases globalConfig) subcmd []
            in switchTo
                $ if isJust (command subcmd' [])
                    then
                        -- TODO: We should have more detailed help for internal
                        -- commands.
                        MainHelp globalConfig
                    else
                        SubcommandHelp subcmd' globalConfig

        _ : arg : _ ->
            die ("Error: '" <> arg <> "': Too many arguments")

    switchTo = pure . Endo . const

    -- TODO: Generate using optparse-applicative.
    helpMsg = unlines
        [ "Usage:"
        , ""
        , "  " <> usedName <> " [GLOBAL_OPTIONS] SUBCOMMAND [SUBCOMMAND_ARGUMENTS]"
        , "  " <> usedName <> " config [SUBCOMMAND]"
        , "  " <> usedName <> " help [SUBCOMMAND]"
        , "  " <> usedName <> " {-h|--help}"
        , ""
        , "Global options:"
        , ""
        , "  -v                     Increment verbosity by one level. Can be\
          \ used multiple times."
        , "  --verbosity=VERBOSITY  Set verbosity level to VERBOSITY. Possible\
          \ values of VERBOSITY are 'silent', 'normal', 'verbose', and\
          \ 'annoying'."
        , "  -s, --silent           Silent mode. Suppress normal diagnostic or\
          \ result output."
        , "  --colo[u]r=WHEN        Set WHEN colourised output should be \
          \ produced. Possible values of WHEN are 'always', 'auto', and\
          \ 'never'."
        , "  --no-colo[u]r          Same as '--colour=no'."
        ]

-- }}} Help Command -----------------------------------------------------------

-- {{{ Config Command ---------------------------------------------------------

data ConfigMode a
    = InitConfig a
    | ConfigLib a
    | Dhall a
  deriving (Functor, Generic, Show)

config :: AppNames -> [String] -> Global.Config -> IO ()
config _appNames _options globalConfig =
    runMain parseOptions defaults $ \case
        InitConfig _ -> pure ()
        ConfigLib _ -> pure ()

        -- TODO:
        --
        -- - Merge in functionality of: `dhall`, `dhall-json`, `dhall-bash`,
        --   and `dhall-text`
        -- - Provide functionality for shell variables that transforms:
        --
        --     ```
        --     { name = "FOO"
        --     , value = "foo"
        --     }
        --     ```
        --
        --     Into:
        --
        --     ```
        --     export FOO=foo
        --     ```
        --
        --     It should also support transorming:
        --
        --     ```
        --     [ { name = "FOO"
        --       , value = "foo"
        --       }
        --     , { name = "BAR"
        --       , value = "bar"
        --       }
        --     ]
        --     ```
        --
        --     Into:
        --
        --     ```
        --     export FOO=foo
        --     export FOO=bar
        --     ```
        Dhall _ -> pure ()
  where
    defaults = Mainplate.applySimpleDefaults (InitConfig globalConfig)

    parseOptions :: IO (Endo (ConfigMode Global.Config))
    parseOptions = die "Error: config: Subcommand not yet implemented."

-- }}} Config Command ---------------------------------------------------------

-- {{{ Completion Command -----------------------------------------------------

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

-- }}} Completion Command -----------------------------------------------------

-- {{{ Generic Functions ------------------------------------------------------

-- | Simplified version of 'Mainplate.runAppWith' that assumes that we don't
-- need to parse any additional configuration file. This should be true for
-- internal commands.
runMain
    :: Functor mode
    => IO (Endo (mode config))
    -> (Endo (mode config) -> IO (mode config))
    -> (mode config -> IO ())
    -> IO ()
runMain parseOptions =
    Mainplate.runAppWith parseOptions (pure . Mainplate.noConfigToRead)

-- }}} ------------------------------------------------------------------------
