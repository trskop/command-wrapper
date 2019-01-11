{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
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

import Prelude (fromIntegral)

import Control.Applicative ((<*>), many, optional, pure)
import Control.Monad ((>>=), join)
import Data.Bool (Bool(False), otherwise)
import qualified Data.Char as Char (isDigit, toLower)
import Data.Either (Either(Left, Right))
import Data.Eq ((==))
import Data.Foldable (all, asum, forM_, mapM_, null)
import Data.Function (($), (.), const, id)
import Data.Functor (Functor, (<$>), (<&>), fmap)
import qualified Data.List as List
    ( break
    , filter
    , isPrefixOf
    , lookup
    , nub
    , takeWhile
    )
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (Endo(Endo, appEndo), mconcat, mempty)
import Data.Semigroup ((<>))
import Data.String (String, fromString)
import Data.Tuple (fst)
import Data.Word (Word)
import GHC.Generics (Generic)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (IO, putStrLn, stderr, stdout)
import Text.Read (readMaybe)
import Text.Show (Show)

import Data.CaseInsensitive as CI (mk)
import Data.Monoid.Endo (mapEndo)
import Data.Monoid.Endo.Fold (dualFoldEndo, foldEndo)
import Data.Text (Text)
import qualified Data.Text.IO as Text (putStrLn)
import Data.Text.Prettyprint.Doc ((<+>), pretty)
import qualified Data.Text.Prettyprint.Doc as Pretty
    ( Doc
    , brackets
    , squotes
    , vsep
    )
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty (AnsiStyle)
import qualified Data.Text.Prettyprint.Doc.Util as Pretty (reflow)
import qualified Dhall
import qualified Dhall.TH (staticDhallExpression)
import qualified Mainplate (applySimpleDefaults)
import qualified Options.Applicative as Options
    ( Parser
    , defaultPrefs
    , eitherReader
    , flag
    , info
    , long
    , metavar
    , option
    , short
    , strOption
    )
import Safe (atMay, headMay, lastMay)
import Text.Fuzzy as Fuzzy (Fuzzy)
import qualified Text.Fuzzy as Fuzzy (Fuzzy(original), filter)

import qualified CommandWrapper.Config.Global as Global (Config(..))
import CommandWrapper.Environment (AppNames(AppNames, exePath, usedName))
import qualified CommandWrapper.External as External (findSubcommands)
import CommandWrapper.Internal.Subcommand.Help
    ( globalOptionsHelp
    , helpOptions
    , longOption
    , metavar
    , optionDescription
    , section
    , toolsetCommand
    , usageSection
    , value
    )
import CommandWrapper.Internal.Utils (runMain)
import CommandWrapper.Message (Result, defaultLayoutOptions, errorMsg, message)
import CommandWrapper.Options.Alias (Alias(alias))
import qualified CommandWrapper.Options.ColourOutput as ColourOutput
    ( ColourOutput(Auto)
    )
import qualified CommandWrapper.Options.Optparse as Options
    ( internalSubcommandParse
    , splitArguments
    )


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
    = CompletionMode CompletionOptions a
    | ScriptMode ScriptOptions a
    | ListArgumentsMode WhatArgumentsToList a
    | HelpMode a
  deriving stock (Functor, Generic, Show)

data CompletionOptions = CompletionOptions
    { words :: [String]
    , index :: Maybe Word
    , shell :: Shell
    }
  deriving stock (Generic, Show)

data ScriptOptions = ScriptOptions
    { aliases :: [String]
    , shell :: Shell
    }
  deriving stock (Generic, Show)

-- | We'll change this data type to:
-- @
-- newtype MkCompletionScript = MkCompletionScript
--     { mkCompletionScript :: 'Shell' -> Text -> Text -> Text
--     }
-- @
newtype MkCompletionScript = MkCompletionScript
    { mkCompletionScript :: Text -> Text -> Text -> Scripts
    }
  deriving stock (Generic)

-- We'll get rid of this type once Dhall function is redefined to accept sum
-- type representing shell variant.
newtype Scripts = Scripts
    { bash :: Text
    }
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.Interpret)

class HasShell a where
    updateShell :: Endo Shell -> Endo a

instance HasShell CompletionOptions where
    updateShell = mapEndo $ \f CompletionOptions{words, index, shell} ->
        CompletionOptions{words, index, shell = f shell}

instance HasShell ScriptOptions where
    updateShell = mapEndo $ \f ScriptOptions{aliases, shell} ->
        ScriptOptions{aliases, shell = f shell}

instance HasShell (CompletionMode cfg) where
    updateShell f = Endo $ \case
        CompletionMode opts cfg ->
            CompletionMode (updateShell f `appEndo` opts) cfg

        ScriptMode opts cfg ->
            ScriptMode (updateShell f `appEndo` opts) cfg

        mode ->
            mode

defScriptOptions :: ScriptOptions
defScriptOptions = ScriptOptions
    { shell = Bash
    , aliases = []
    }

completion :: AppNames -> [String] -> Global.Config -> IO ()
completion appNames options config =
    runMain (parseOptions appNames config options) defaults $ \case
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
        CompletionMode CompletionOptions{words, index} () -> do
            let pat = fromMaybe ""
                    $ maybe (lastMay words) (atMay words . fromIntegral) index

                subcmds = findSubcommands config pat

            completions <-
                case headMay pat of
                    Nothing  -> (findOptions config ('-' : pat) <>) <$> subcmds
                    Just '-' -> pure (findOptions config pat)
                    _        -> subcmds

            mapM_ putStrLn completions

        -- TODO:
        --
        -- - Completion script should be configurable.
        --
        -- - By default it should be printed to `stdout`, but we should support
        --   writting it into a file without needing to redirect `stdout`.
        ScriptMode ScriptOptions{shell = Bash, aliases} () -> do
            let mkCompletionScriptExpr =
                    $(Dhall.TH.staticDhallExpression
                        "./dhall/CommandWrapper/completion.dhall"
                    )

                AppNames{exePath, usedName} = appNames

            case MkCompletionScript <$> Dhall.extract Dhall.auto mkCompletionScriptExpr of
                Nothing -> do
                    -- TODO: Figure out how to handle this better.
                    let Global.Config{colourOutput, verbosity} = config
                        colour = fromMaybe ColourOutput.Auto colourOutput
                        subcommand = pretty (usedName <> " completion:")
                    errorMsg subcommand verbosity colour stderr
                        "Failed to generate completion script."
                    -- TODO: This is probably not the best exit code.
                    exitWith (ExitFailure 1)

                Just (MkCompletionScript mkScript) ->
                    forM_ (usedName : aliases) $ \name ->
                        let Scripts{bash} = mkScript
                                (fromString name :: Text)
                                (fromString usedName :: Text)
                                (fromString exePath :: Text)

                        in Text.putStrLn bash

        ListArgumentsMode whatToList () -> case whatToList of
            Subcommands pat ->
                findSubcommandsFuzzy config pat
                    >>= mapM_ (putStrLn . Fuzzy.original)

        HelpMode () ->
            let Global.Config{colourOutput, verbosity} = config
                colour = fromMaybe ColourOutput.Auto colourOutput

            in message defaultLayoutOptions verbosity colour stdout
                (completionSubcommandHelp appNames)
  where
    defaults =
        let opts = CompletionOptions
                { words = []
                , index = Nothing
                , shell = Bash
                }

        in Mainplate.applySimpleDefaults (CompletionMode opts ())

    getSubcommands :: Global.Config -> IO [String]
    getSubcommands cfg = do
        extCmds <- External.findSubcommands appNames cfg

        let aliases = alias <$> Global.aliases cfg
            internalCommands = ["help", "config", "completion", "version"]

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
        let verbosityValues =
                ["silent", "quiet", "normal", "verbose", "annoying"]

            colourWhences = ["always", "auto", "never"]

            shortOptions =
                [ "-v", "-vv", "-vvv"
                , "-s", "-q"
                , "-V"
                ]

            longOptions :: [(String, Maybe (String -> [String]))]
            longOptions =
                [ ("--verbosity=", Just (findKeywords verbosityValues cfg))
                , ("--color=", Just (findKeywords colourWhences cfg))
                , ("--colour=", Just (findKeywords colourWhences cfg))
                , ("--no-color", Nothing)
                , ("--no-colour", Nothing)
                , ("--version", Nothing)
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
                    List.filter (pat `List.isPrefixOf`) shortOptions
                    <> List.filter (fmap Char.toLower pat `List.isPrefixOf`)
                        (fmap fst longOptions)

                _ -> []

    findKeywords :: [String] -> Global.Config -> String -> [String]
    findKeywords keywords _cfg pat =
        List.filter (fmap Char.toLower pat `List.isPrefixOf`) keywords

parseOptions
    :: AppNames
    -> Global.Config
    -> [String]
    -> IO (Endo (CompletionMode ()))
parseOptions appNames config arguments = do
    let (options, words) = Options.splitArguments arguments

    execParser options $ asum
        [ dualFoldEndo
            <$> scriptFlag
            <*> optional shellOption
            <*> many aliasOption

        , helpFlag

        , updateCompletionOptions words
            $ foldEndo
                <$> optional indexOption
                <*> optional shellOption
        ]
  where
    switchTo = Endo . const
    switchToScriptMode = switchTo (ScriptMode defScriptOptions ())
    switchToHelpMode = switchTo (HelpMode ())

    updateCompletionOptions words =
        fmap . mapEndo $ \f _ ->
            let defOpts = CompletionOptions
                    { words
                    , index = Nothing
                    , shell = Bash
                    }
            in CompletionMode (f defOpts) ()

    scriptFlag :: Options.Parser (Endo (CompletionMode ()))
    scriptFlag = Options.flag mempty switchToScriptMode (Options.long "script")

    indexOption :: Options.Parser (Endo CompletionOptions)
    indexOption =
        Options.option parse (Options.long "index" <> Options.metavar "NUM")
      where
        parse = Options.eitherReader $ \s ->
            if  | null s ->
                    Right mempty

                | all Char.isDigit s ->
                    case readMaybe s of
                        Nothing ->
                            Left "Non-negative number expected"

                        index ->
                            Right . Endo $ \CompletionOptions{words, shell} ->
                                CompletionOptions{words, index, shell}

                | otherwise ->
                    Left "Non-negative number expected"

    shellOption :: HasShell a => Options.Parser (Endo a)
    shellOption =
        Options.option parse
            (Options.long "shell" <> Options.metavar "SHELL")
      where
        parse = Options.eitherReader $ \s -> case CI.mk s of
            "bash" -> Right $ updateShell (Endo $ const Bash)
            _ -> Left "Unrecognised shell name"

    aliasOption :: Options.Parser (Endo (CompletionMode ()))
    aliasOption =
        Options.strOption (Options.long "alias" <> Options.metavar "ALIAS")
            <&> \alias -> Endo $ \case
                    ScriptMode ScriptOptions{aliases, shell} cfg ->
                        ScriptMode
                            ( ScriptOptions
                                { aliases = aliases <> [alias]
                                , shell
                                }
                            )
                            cfg
                    mode ->
                        mode

    helpFlag = Options.flag mempty switchToHelpMode $ mconcat
        [ Options.short 'h'
        , Options.long "help"
        ]

    execParser options parser =
        Options.internalSubcommandParse appNames config "completion"
            Options.defaultPrefs (Options.info parser mempty) options

completionSubcommandHelp :: AppNames -> Pretty.Doc (Result Pretty.AnsiStyle)
completionSubcommandHelp AppNames{usedName} = Pretty.vsep
    [ usageSection usedName
        [ "completion"
            <+> Pretty.brackets
                ( longOption "index" <> "=" <> metavar "NUM"
                )
            <+> Pretty.brackets
                ( longOption "shell" <> "=" <> metavar "SHELL"
                )
            <+> value "--"
            <+> Pretty.brackets (metavar "WORD" <+> "...")

        , "completion"
            <+> longOption "script"
            <+> Pretty.brackets (longOption "shell" <> "=" <> metavar "SHELL")
            <+> Pretty.brackets
                ( longOption "alias" <> "=" <> metavar "ALIAS" <+> "..."
                )

        , "completion" <+> helpOptions
        , "help completion"
        ]

    , section "Options:"
        [ optionDescription ["--index=NUM"]
            [ Pretty.reflow "Position of a", metavar "WORD"
            , Pretty.reflow "for which we want completion. In Bash this is the"
            , Pretty.reflow "value of", value "COMP_CWORD", "variable."
            ]

        , optionDescription ["--shell=SHELL"]
            [ Pretty.reflow "Provide completion or generate script for"
            , metavar "SHELL" <> "."
            , Pretty.reflow "Currently only supported value is"
            , value "bash" <> "."
            ]

        , optionDescription ["--script"]
            [ Pretty.reflow "Generate completion script suitable for sourcing"
            , Pretty.reflow "in your shell's *rc file."
            ]

        , optionDescription ["--alias=ALIAS"]
            [ metavar "ALIAS"
            , Pretty.reflow "under which Command Wrapper toolset is also known."
            , Pretty.reflow "This is usually name of a shell alias, e.g."
            , Pretty.squotes (value "alias ts=toolset")
            , "where", value "ts", Pretty.reflow "is an", metavar "ALIAS"
            , Pretty.reflow "for which we want command line completion to work"
            , Pretty.reflow "as well."
            ]

        , optionDescription ["--help", "-h"]
            [ Pretty.reflow "Print this help and exit. Same as"
            , Pretty.squotes (toolsetCommand usedName "help completion") <> "."
            ]

        , optionDescription ["WORD"]
            [ metavar "WORD" <> "s", Pretty.reflow "to complete. In Bash these"
            , Pretty.reflow "are the elements of", value "COMP_WORDS", "array."
            ]

        , globalOptionsHelp usedName
        ]

    , ""
    ]
