{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.Subcommand.Prelude
-- Description: Give subcommands everything they need to seamlessly integrate.
-- Copyright:   (c) 2018-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- Give subcommands everything they need to seamlessly integrate.
module CommandWrapper.Subcommand.Prelude
    (
    -- * Subcommand Protocol
    --
    -- | Some definitions are re-exported from
    -- "CommandWrapper.Environment.Params" module.
      Params(..)
    , subcommandParams

    -- ** Verbosity
    --
    -- | Re-exported from "Data.Verbosity" module of
    -- <http://hackage.haskell.org/package/verbosity verbosity> package.
    , Verbosity(..)

    -- ** Colour Output
    --
    -- | Re-exported from "CommandWrapper.Options.ColourOutput" module.
    , ColourOutput(..)
    , shouldUseColours

    -- ** Command Line Options
    --
    -- $runSubcommandExample
    , SubcommandProps(..)
    , runSubcommand
    , splitArguments
    , splitArguments'
    , noPreprocessing

    -- *** Completion and Completion Info
    , Shell(..)
    , HaveCompletionInfo(..)
    , completionInfoFlag
    , completionOptions
    , printOptparseCompletionInfoExpression
    , printCommandWrapperStyleCompletionInfoExpression

    -- ** Config
    --
    -- $configExample
    , inputConfig
    , inputConfigWithSettings
    , dieMissingConfiguration

    -- * Messages
    --
    -- | Colour and terminal respectful messages.
    , out
    , outWith
    , warningMsg
    , errorMsg
    , debugMsg
    , Message.MessageType(..)
    , Message.Result(..)
    , Message.defaultLayoutOptions
    , message

    -- * Error Handling
    --
    -- | These functions respect 'verbosity' and 'colour'.
    , dieWith

    -- * IO
    --
    -- | Some of these are just reexported from "System.IO" from `base` package.
    , stderr
    , stdout
    , Message.withTerminal
    )
  where

import Control.Applicative (pure)
import Data.Bool ((||), otherwise)
import Data.Function (id)
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Monoid ((<>))
import Data.Ord ((<=), (>))
import System.IO (Handle, IO, stderr, stdout)
import System.Environment (getProgName)
import System.Exit (ExitCode(ExitFailure), exitWith)

import Data.Text (Text)
import qualified Data.Text as Text (null)
import Data.Text.Prettyprint.Doc (Pretty(pretty), (<+>))
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty (AnsiStyle)
import qualified Data.Text.Prettyprint.Doc.Util as Pretty (reflow)
import Data.Verbosity (Verbosity(..))
import qualified Options.Applicative as Options (Parser, flag)
import qualified System.Console.Terminal.Size as Terminal (Window)
import qualified Dhall (Decoder, InputSettings, input, inputWithSettings)

import qualified CommandWrapper.Core.Message as Message
    ( MessageType(..)
    , Result(..)
    , defaultLayoutOptions
    , errorMsg
    , debugMsg
    , message
    , outWith
    , warningMsg
    , withTerminal
    )
import CommandWrapper.Environment.Params (Params(..), askParams)
import CommandWrapper.Environment.Parser (parseEnvIO)
import CommandWrapper.Options (splitArguments, splitArguments')
import CommandWrapper.Options.ColourOutput (ColourOutput(..), shouldUseColours)
import CommandWrapper.Options.Shell (Shell(..))
import CommandWrapper.Subcommand.Options
    ( SubcommandProps(..)
    , completionInfoFlagFields
    , completionOptions
    , noPreprocessing
    , printCommandWrapperStyleCompletionInfoExpression
    , printOptparseCompletionInfoExpression
    , runSubcommand
    )


-- | Specialised version of @'parseEnvIO' onError 'askParams'@ that handles
-- edge cases correctly.
subcommandParams :: IO Params
subcommandParams = parseEnvIO protocolError askParams
  where
    protocolError err = do
        -- This is not the best thing we can use, but it's close enough to the
        -- `TOOLSET SUBCOMMAND@ format that it could be useful to the users.
        name <- getProgName
        Message.errorMsg (pretty name) Normal Auto stderr (mkMessage err)

        -- See `command-wrapper-subcommand-protocol(7)` manual page section
        -- /EXIT STATUS/.
        exitWith (ExitFailure 1)

    mkMessage err =
        Pretty.reflow "This command must be executed inside Command Wrapper\
            \ environment: Failed to parse environment variables:"
        <+> Pretty.viaShow err <> Pretty.colon
        <+> Pretty.reflow "See command-wrapper-subcommand-protocol(7) manual\
            \ page for more details."

errorMsg :: Params -> Handle -> Text -> IO ()
errorMsg Params{colour, name, subcommand, verbosity} h msg =
    let cmd = name <> " " <> subcommand
    in Message.errorMsg (pretty cmd) verbosity colour h (pretty msg)

warningMsg :: Params -> Handle -> Text -> IO ()
warningMsg Params{colour, name, subcommand, verbosity} h msg =
    let cmd = name <> " " <> subcommand
    in Message.warningMsg (pretty cmd) verbosity colour h (pretty msg)

debugMsg :: Params -> Handle -> Text -> IO ()
debugMsg Params{colour, name, subcommand, verbosity} h msg =
    let cmd = name <> " " <> subcommand
    in Message.debugMsg (pretty cmd) verbosity colour h (pretty msg)

outWith
    :: (Maybe (Terminal.Window Int) -> Pretty.LayoutOptions)
    -> Params
    -> Handle
    -> Pretty.Doc (Message.Result Pretty.AnsiStyle)
    -> IO ()
outWith mkLayoutOptions Params{colour, verbosity} h doc =
    Message.outWith mkLayoutOptions verbosity colour h doc

out :: Params
    -> Handle
    -> Pretty.Doc (Message.Result Pretty.AnsiStyle)
    -> IO ()
out params h doc = outWith Message.defaultLayoutOptions params h doc

message
    :: forall ann
    .  Message.MessageType ann
    => (Maybe (Terminal.Window Int) -> Pretty.LayoutOptions)
    -> Params
    -> Handle
    -> Pretty.Doc ann
    -> IO ()
message mkLayoutOptions Params{colour, verbosity} h doc =
    Message.message mkLayoutOptions verbosity colour h doc

-- | Terminate with specified error message and exit status.  Colour and
-- verbosity settings are respected.
dieWith
    :: Params
    -> Handle
    -> Int
    -- ^ Exit status between @1@ (including) and @255@ (including).
    -> Text
    -> IO a
dieWith params h n msg = do
    errorMsg params h msg
    exitWith (ExitFailure exitCode)
  where
    exitCode =
        -- Exit code can be only between 0-255; 0 is success so we are
        -- excluding it as well.
        if n <= 0 || n > 255
            then 255
            else n

class HaveCompletionInfo a where
    completionInfoMode :: a -> a

completionInfoFlag
    :: HaveCompletionInfo a
    => Options.Parser (a -> a)
completionInfoFlag =
    Options.flag id completionInfoMode completionInfoFlagFields

-- {{{ Configuration ----------------------------------------------------------

-- $configExample
--
-- @
-- data Config = Config
--     { ...
--     }
--   deriving stock ('GHC.Generics.Generic')
--   deriving newtype ('Dhall.FromDhall')
--
-- defConfig :: Config
-- defConfig = Config
--     { ...
--     }
--
-- -- | Use this when you want to provide default values if configuration file
-- -- doesn't exist.
-- readConfig :: 'Params' -> IO Config
-- readConfig params =
--     'fromMaybe' defConfig '<$>' 'inputConfig' 'Dhall.auto' params
--
-- -- | Use this when you want to fail if the configuration file is not
-- -- provided.
-- readConfigOrDie :: 'Params' -> IO Config
-- readConfigOrDie params = 'inputConfig' 'Dhall.auto' params '>>=' \\case
--    Just conifg -> 'pure' config
--    Nothing -> 'dieMissingConfiguration' params 'stderr' Nothing
-- @

-- | Evaluate Dhall expression passed down through Subcommand Protocol as
-- configuration.
--
-- See 'Dhall.input' for more information.
inputConfig
    :: Dhall.Decoder a
    -- ^ The decoder for the Dhall value of subcommand configuration.
    -> Params
    -> IO (Maybe a)
    -- ^ Returns 'Nothing' if there was no configuration, i.e. Command Wrapper
    -- was unable to find a configuration file that could be passed to us.
inputConfig decoder Params{config}
  | Text.null config = pure Nothing
  | otherwise = Just <$> Dhall.input decoder config

-- | Variant of 'inputConfig' with a custom settings for Dhall evaluator.
--
-- See 'Dhall.inputWithSettings' for more information.
inputConfigWithSettings
    :: Dhall.InputSettings
    -> Dhall.Decoder a
    -- ^ The decoder for the Dhall value of subcommand configuration.
    -> Params
    -> IO (Maybe a)
    -- ^ Returns 'Nothing' if there was no configuration, i.e. Command Wrapper
    -- was unable to find a configuration file that could be passed to us.
inputConfigWithSettings settings decoder Params{config}
  | Text.null config = pure Nothing
  | otherwise = Just <$> Dhall.inputWithSettings settings decoder config

-- | Terminate with a correct status code for missing configuration file.
dieMissingConfiguration
    :: Params
    -> Handle
    -> Maybe Text
    -- ^ 'Nothing' means use default error message for missing config.
    -> IO a
dieMissingConfiguration params h msg =
    dieWith params h 1 (fromMaybe defMessage msg)
  where
    defMessage = "Configuration file is required and it's missing."

-- }}} Configuration ----------------------------------------------------------

-- $runSubcommandExample
--
-- @
-- module Main
--   where
--
-- -- ...
-- import qualified CommandWrapper.Subcommand.Prelude ('SubcommandProps'(..))
-- import CommandWrapper.Subcommand.Prelude
--     ( 'SubcommandProps'('SubcommandProps')
--     , 'noPreprocessing'
--     , 'runSubcommand'
--     )
--
--
-- data Mode = ...
--
-- defMode :: Mode
-- defMode = ...

-- main :: IO ()
-- main = do
--     params <- 'subcommandParams'
--     arguments <- getArgs
--     'runSubcommand' 'SubcommandProps'
--         { 'preprocess' = 'noPreprocessing'
--         , 'doCompletion'
--         , 'helpMsg'
--         , 'actionOptions' = parseOptions
--         , 'defaultAction' = Just defMode)
--         , 'params'
--         , 'arguments'
--         }
--         arguments mainAction
--
-- mainAction :: 'Params' -> Mode -> IO ()
-- mainAction Params{} Mode{..} = ...
--
-- parseOptions :: 'Options.Parser' ('Endo' (Maybe Mode))
-- parseOptions = ...
--
-- helpMsg :: 'Params' -> 'Pretty.Doc' ('Result' 'Pretty.AnsiStyle')
-- helpMsg Params{} = ...
--
-- doCompletion :: 'Params' a -> Word -> 'Shell' -> [String] -> IO ()
-- doCompletion Params{} index shell words = ...
-- @
