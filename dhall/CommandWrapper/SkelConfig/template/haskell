-- vim: filetype=dhall

''
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
-- |
-- Description: TODO: Subcommand description
--
-- TODO: Subcommand description.
--
-- Packages required to compile this template:
--
-- * @base@
-- * @command-wrapper@
-- * @dhall@
-- * @endo@
-- * @prettyprinter-ansi-terminal@
-- * @prettyprinter@
-- * @safe@
-- * @turtle@
module Main (main)
  where

import Data.Function (const)
import qualified Data.List as List (filter, isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import GHC.Generics (Generic)
import System.Environment (getArgs)

import qualified CommandWrapper.Core.Help.Pretty as Help
import qualified CommandWrapper.Subcommand.Prelude (SubcommandProps(..))
import CommandWrapper.Subcommand.Prelude
    ( Params(Params, name, subcommand)
    , Result
    , Shell
    , SubcommandProps(SubcommandProps)
    , dieWith
    , inputConfig
    , noPreprocessing
    , runSubcommand
    , stderr
    , subcommandParams
    )
import Data.Monoid.Endo (Endo(Endo))
import Data.Text.Prettyprint.Doc ((<+>))
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty (AnsiStyle)
import qualified Data.Text.Prettyprint.Doc.Util as Pretty (reflow)
import Dhall (FromDhall)
import qualified Dhall (auto)
import Safe (atMay, lastDef)
import qualified Turtle
  ( Parser
  )

-- If you have multiple Haskell commands that are build as part of a Haskell
-- package then they can share a common library.
--import ToolsetLibrary


-- | Subcommand configuration as it was passed to us by Command Wrapper using
-- Dhall.
data Config = Config
    {
    }
  deriving stock (Generic, Show)
  deriving anyclass (FromDhall)

-- | Default 'Config' value used if there is no configuration given to us.
defConfig :: Config
defConfig = Config

-- | Sumtype that describes list of actions that can be performed by this
-- subcommand.  Which action is performed is decided by command line options.
-- See 'parseOptions' function.
data Action
    = Action
  deriving stock (Generic, Show)

-- | Default action to perform if command line options parser returns identity.
-- That usually happens when there are no command line options to be parsed.
defAction :: Action
defAction = Action

-- | Execution environment for this subcommand that is used by functions that
-- perform either mandated actions or the 'Action's for which the subcommand
-- was constructed.  See 'doCompletion', 'helpMsg', and 'runAction'.
data Env = Env
    { params :: Params
    -- ^ Parameters passed to the subcommand by Command Wrapper via
    -- Subcommand Protocol.  See @command-wrapper-subcommand-protocol(7)@ for
    -- more details.

    , config :: Config
    -- ^ Configuration used by this subcommand.

    -- ...
    --
    -- TODO: You can add more stuff here, if it makes sense for your subcommand.
    }
  deriving stock (Generic, Show)

main :: IO ()
main = do
    subcommandProps <- do
        -- Function `subcommandParams` makes sure that this subcommand was
        -- executed using expected calling convention.  If it wasn't it will
        -- terminate this script with appropriate error message and exit
        -- status.  It is hard to guarantee anything in case of it being
        -- executed in any other way.  Mentioned calling convention is
        -- described in a dedicated manual page
        -- `command-wrapper-subcommand-protocol(7)`.
        params <- subcommandParams
        arguments <- getArgs

        -- Subcommands aren't required to use configuration files.  If you
        -- don't need it then just delete or comment-out configuration related
        -- code.
        --
        -- When subcommand is using configuration file and that doesn't exist
        -- it is allowed to do one of the following:
        --
        -- 1.   Use hardcoded defaults.
        -- 2.   Fail with error message indicating that the configuration file
        --      is missing.
        --
        -- See command-wrapper-subcommand-protocol(7) manual page for more
        -- information.
        --
        -- Following code assumes that the subcommand uses the former option
        -- (the one marked as 1.).  Code that doesn't allow subcommand to work
        -- without a configuration file is commented out below it.
        config <- fromMaybe defConfig <$> inputConfig Dhall.auto params

        -- Following is an example of how subcommand can require configuration
        -- to be available.  Note that this will try to parse the configuration
        -- before processing command line options.  If you want to provide
        -- option that prints default configuration then this code will have
        -- to be moved into the action handler, see 'runAction'.
        --
        -- If we want to keep `config` field in env, but parse it in
        -- 'runAction', then we can use the following trick:
        --
        -- > data Env config = Env
        -- >     { params :: Params
        -- >     , config :: config
        -- >     -- ...
        -- >     }
        --
        -- After that we can pass `Env Void` in 'SubcommandProps'.  This way
        -- all our code can use 'Env' as an argument, but we can make the
        -- distinction between those that have access to configuration and
        -- those that don't.
        --
        --config <- inputConfig Dhall.auto protocol >>= \case
        --    Nothing ->
        --        -- Passing 'Nothing' to 'dieMissingConfiguration' means "use
        --        -- default error message."
        --        dieMissingConfiguration params stderr Nothing
        --    Just config ->
        --        pure config

        -- 'SubcommandProps' value is passed to 'runSubcommand' function
        -- bellow.  They describe how subcommand implements parsing of command
        -- line options, command line completion, and what help message to
        -- print.
        pure SubcommandProps
            -- Processing phase allows us to modify command line options and
            -- 'Env'.  Usually we don't need this, but if we can't let
            -- underlying parser handle certain options then we may need to use
            -- this.  Reason for being able to modify 'Env' is that we may want
            -- to preserve the information about that options being
            -- intercepted.
            --
            -- Certainly all of this could have been done outside of
            -- 'runSubcommand' machinery, but this way we have a uniform API
            -- for functions like 'doCompletion', which than can have access to
            -- all that information via 'Env' only.
            { preprocess = noPreprocessing

            -- Function that provides command line completion.
            , doCompletion

            -- Help message to be printed when {--help|-h} are passed.
            , helpMsg

            -- Parser for subcommand options has type:
            --
            -- > Options.Parser Endo (Maybe Action)
            --
            -- The 'Nothing' value represents cases when we are in non-Action
            -- mode, but help mode, and we are able to switch into help mode by
            -- returning 'Nothing'.  This is useful if subcommand itself needs
            -- control over deciding when '--help' mode is reached.
            --
            -- Function `Endo . fmap` is specialised to:
            --
            -- > (Action -> Action) -> Endo (Maybe Action)
            --
            -- Which turns a parser that doesn't need to care about
            -- help-related stuff into a compatible parser.
            , actionOptions = Endo . fmap <$> parseOptions

            -- If we put 'Nothing' here then the default action would be
            -- printing help message.
            , defaultAction = Just defAction

            -- Initial value of 'Env' that will be then 'preprocess'ed and
            -- passed around to all the helper functions.  It must contain the
            -- value of Subcommand Protocol 'Params', hence the name.
            , params = Env
                { params
                , config
                }

            -- Command line arguments to be parsed.
            , arguments
            }

    -- Simplest way how to provide Command Wrapper requirements for its
    -- subcommand is to use `runSubcommand` to handle processing of command
    -- line options.
    --
    -- In principle subcommands are free to decide what command line options
    -- should be supported, and how they should be parsed, but each subcommand
    -- has to support following:
    --
    -- *   `--help`, `-h`
    -- *   `--completion-info` -- Command Wrapper library provides basic
    --     building block for this in the form of `completionInfoFlag` and
    --     `HaveCompletionInfo`.  See their respective documentation for more
    --     details.
    --
    -- In addition to the above following command line options are reserved for
    -- future use by the Subcommand Protocol:
    --
    -- *   `--completion-info-hash`
    -- *   `--config-constructor`
    -- *   `--config-constructor-hash`
    --
    -- Subcommands are encouraged to also support:
    --
    -- *   `--init-config` -- It's name is not mandated either.  Purpose of
    --     this option is to print initial configuration file to standard
    --     output.
    --
    -- See Command Wrapper's Subcommand Protocol for more information.  It is
    -- available in the form of command-wrapper-subcommand-protocol(7) manual
    -- page.
    runSubcommand subcommandProps runAction

-- | Functionality of this subcommand.
runAction :: Env -> Action -> IO ()
runAction Env{params} = \case
    Action ->
        -- TODO: Implement me!  Here is the place where the real functionality
        -- should be.
        dieWith params stderr 125 "Not yet implemented!"

-- | Parse command line options of this subcommand.
parseOptions :: Turtle.Parser (Action -> Action)
parseOptions =
    -- This function uses options parser from `turtle`, which is actually just
    -- re-exported parser from `optparse-applicative`.  Either of those
    -- packages can be used to define the parser.  The main distinction is that
    -- `turtle` provides simpler API, while `optparse-applicative` provides
    -- more control.
    pure (const Action)

-- | Help message printed if @--help|-h@ options are passed to the subcommand.
helpMsg :: Env -> Pretty.Doc (Result Pretty.AnsiStyle)
helpMsg Env{params = Params{name, subcommand}} = Pretty.vsep
    [ Pretty.reflow "TODO: Hereby I promise to describe this subcommand one\
        \ day."
    , ""  -- Empty line between short description and usage section.

    , Help.usageSection name
        -- TODO: Modify this to fit your subcommand.
        --
        -- TOOLSET [GLOBAL_OPTIONS] SUBCOMMAND [OPTION ...]
        [ subcommand'
            <+> Pretty.brackets (Help.metavar "OPTION" <+> "...")

        -- TOOLSET [GLOBAL_OPTIONS] SUBCOMMAND {--help|-h}
        , subcommand' <+> Help.helpOptions

        -- TOOLSET [GLOBAL_OPTIONS] help --man SUBCOMMAND
        , "help" <+> Pretty.brackets (Help.longOption "man") <+> subcommand'
        ]

    , Help.section "Options:"
        [ Help.optionDescription ["--help", "-h"]
            [ Pretty.reflow "Print this help and exit. Same as"
            , Pretty.squotes
                (Help.toolsetCommand name ("help" <+> subcommand')) <> "."
            ]

        , Help.globalOptionsHelp name
        ]
    , ""  -- Empty line at the end of the message.
    ]
  where
    subcommand' = fromString subcommand

-- | Provides command line completion for when the subcommand is invoked in
-- completion mode.
--
-- The Subcommand Protocol (see @command-wrapper-subcommand-protocol(7)@
-- describes the whole process of calling subcommand completion.  The core
-- principle is that we get three values:
--
-- 1. Index into a list of words which represent parsed command line, i.e. each
--    word is a command, option, argument, or shell operator (like redirection).
-- 2. Which shell we are in.  This allows subcommands to adjust to shell
--    specifics.  For example some shells allow description of the
--    option\/argument that is being completed.
-- 3. List of words which represent the parsed command line.  See also point 1.
doCompletion
    :: Env
    -> Word
    -- ^ Index into a list of words which represent the parsed command line.
    -> Shell
    -- ^ 'Shell' for which we are providing command line completion.
    -> [String]
    -- ^ List of words which represent the parsed command line.
    -> IO ()
doCompletion Env{} index _shell words' = do
    mapM_ putStrLn (List.filter (pat `List.isPrefixOf`) allOptions)
  where
    -- Word to which index is pointing.  We default to empty string if index is
    -- pointing outside of the `words'` list.
    pat = fromMaybe (lastDef "" words') (atMay words' (fromIntegral index))

    allOptions =
        [ "-h", "--help"
        ]
''
