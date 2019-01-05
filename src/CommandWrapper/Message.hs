{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:      CommandWrapper.Message
-- Description: Helper functions for printing messages.
-- Copyright:   (c) 2018-2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- Helper functions for printing messages, especially error and warning
-- messages.
module CommandWrapper.Message
    ( errorMsg
    , warningMsg
    , debugMsg

    -- * Print Error and Die
    , dieFailedToParseEnvironment
    , dieFailedToParseOptions
    , dieSubcommandNotYetImplemented
    , dieTooManyArguments
    , dieUnableToExecuteSubcommand
    , dieUnableToFindSubcommandExecutable
    )
  where

import Control.Exception (bracket_)
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (Handle, hPutStr, hPutStrLn)

import Data.Text (Text)
import qualified Data.Text as Text (null)
import qualified Data.Text.IO as Text (hPutStrLn)
import Data.Verbosity (Verbosity(Annoying, Silent))
import qualified Options.Applicative.Help as Options
    ( Chunk(..)
    , ParserHelp(ParserHelp, helpError)
    , colon
    , displayS
    , hsep
    , renderPretty
    , text
    )
import qualified System.Console.ANSI as Terminal
    ( Color(Red, White, Yellow)
    , ColorIntensity(Dull, Vivid)
    , ConsoleLayer(Foreground)
    , SGR(Reset, SetColor)
    , setSGRCode
    )
import qualified System.Console.Terminal.Size as Terminal
    ( Window(width)
    , hSize
    )

import CommandWrapper.Environment.Parser (ParseEnvError)
import CommandWrapper.Options.ColourOutput (ColourOutput(..), shouldUseColours)


-- TODO: Generalise the pattern in 'errorMsg' and 'warningMsg' functions.

errorMsg
    :: Text
    -- ^ Command that is being executed.  It should either be @TOOLSET@ or
    -- @TOOLSET SUBCOMMAND@.
    -> Verbosity
    -> ColourOutput
    -> Handle
    -> Text
    -- ^ Error message.
    -> IO ()
errorMsg command verbosity colourOutput h msg =
    when (verbosity > Silent) $ do
        useColours <- shouldUseColours h colourOutput
        withColour h useColours vividRed $ \h' ->
            Text.hPutStrLn h' (formatCommand command <> "Error: " <> msg)
  where
    vividRed = Terminal.SetColor
        Terminal.Foreground
        Terminal.Vivid
        Terminal.Red

warningMsg
    :: Text
    -- ^ Command that is being executed.  It should either be @TOOLSET@ or
    -- @TOOLSET SUBCOMMAND@.
    -> Verbosity
    -> ColourOutput
    -> Handle
    -> Text
    -- ^ Warning message.
    -> IO ()
warningMsg command verbosity colourOutput h msg =
    when (verbosity > Silent) $ do
        useColours <- shouldUseColours h colourOutput
        withColour h useColours vividYellow $ \h' ->
            Text.hPutStrLn h' (formatCommand command <> "Warning: " <> msg)
  where
    vividYellow = Terminal.SetColor
        Terminal.Foreground
        Terminal.Vivid
        Terminal.Yellow

debugMsg
    :: Text
    -- ^ Command that is being executed.  It should either be @TOOLSET@ or
    -- @TOOLSET SUBCOMMAND@.
    -> Verbosity
    -> ColourOutput
    -> Handle
    -> Text
    -- ^ Debug message.
    -> IO ()
debugMsg command verbosity colourOutput h msg =
    when (verbosity >= Annoying) $ do
        useColours <- shouldUseColours h colourOutput
        withColour h useColours dullWhite $ \h' ->
            Text.hPutStrLn h' (formatCommand command <> "Debug: " <> msg)
  where
    dullWhite = Terminal.SetColor
        Terminal.Foreground
        Terminal.Dull
        Terminal.White

withColour :: Handle -> Bool -> Terminal.SGR -> (Handle -> IO ()) -> IO ()
withColour h useColours colour action =
    if useColours
        then hSetSgrCode h colour `bracket_` hReset h $ action h
        else action h

formatCommand :: Text -> Text
formatCommand command =
    if Text.null command
        then ""
        else command <> ": "

hSetSgrCode :: Handle -> Terminal.SGR -> IO ()
hSetSgrCode h code = hPutStr h $ Terminal.setSGRCode [code]

hReset :: Handle -> IO ()
hReset h = hSetSgrCode h Terminal.Reset

-- | Die with exit code @127@ which was choosen based on
-- <http://www.tldp.org/LDP/abs/html/exitcodes.html>
dieUnableToFindSubcommandExecutable
    :: Text
    -- ^ Name under which Command Wrapper was executed.
    -> Verbosity
    -> ColourOutput
    -> Handle
    -> Text
    -- ^ Subcommand name for which we tried to find an executable.
    -> IO a
dieUnableToFindSubcommandExecutable name verbosity colourOutput h
  subcommand = do
    errorMsg name verbosity colourOutput h
        ( subcommand
        <> ": Unable to find suitable executable for this subcommand"
        )
    exitWith (ExitFailure 127)

-- | Die with exit code @126@ which was choosen based on
-- <http://www.tldp.org/LDP/abs/html/exitcodes.html>
dieUnableToExecuteSubcommand
    :: Text
    -- ^ Name under which Command Wrapper was executed.
    -> Verbosity
    -> ColourOutput
    -> Handle
    -> Text
    -- ^ Subcommand name for which we tried to execute the executable.
    -> Text
    -- ^ Executable that we tried to execute.
    -> IO a
dieUnableToExecuteSubcommand name verbosity colourOutput h subcommand
  executable = do
    errorMsg name verbosity colourOutput h
        ( fromString (show subcommand)
        <> ": Unable to execute external subcommand executable: '"
        <> fromString (show executable) <> "'"
        )
    exitWith (ExitFailure 126)

dieTooManyArguments
    :: Text
    -- ^ Name under which Command Wrapper was executed.
    -> Text
    -- ^ Subcommand name.
    -> Verbosity
    -> ColourOutput
    -> Handle
    -> Text
    -- ^ Argument responsible for the error.
    -> IO a
dieTooManyArguments name subcommand verbosity colourOutput h argument = do
    errorMsg (name <> " " <> subcommand) verbosity colourOutput h
        $ fromString (show argument) <> ": Too many arguments."
    exitWith (ExitFailure 1)

dieSubcommandNotYetImplemented
    :: Text
    -- ^ Name under which Command Wrapper was executed.
    -> Verbosity
    -> ColourOutput
    -> Handle
    -> Text
    -- ^ Name of the subcommand that is not implemented at the moment.
    -> IO a
dieSubcommandNotYetImplemented name verbosity colourOutput h subcommand = do
    errorMsg name verbosity colourOutput h
        $ fromString (show subcommand) <> ": Subcommand not yet implemented."
    exitWith (ExitFailure 126)

dieFailedToParseEnvironment
    :: Text
    -- ^ Name under which Command Wrapper was executed.
    -> Verbosity
    -> ColourOutput
    -> Handle
    -> ParseEnvError
    -> IO a
dieFailedToParseEnvironment name verbosity colourOutput h err = do
    errorMsg name verbosity colourOutput h
        $ "Failed to parse environment: " <> fromString (show err)
    exitWith (ExitFailure 1)

dieFailedToParseOptions
    :: String
    -> Verbosity
    -> ColourOutput
    -> Handle
    -> Options.ParserHelp
    -> IO a
dieFailedToParseOptions name verbosity colour h
  Options.ParserHelp{Options.helpError} = do
    -- TODO: Duplicity.
    when (verbosity > Silent) $ do
        useColours <- shouldUseColours h colour
        withColour h useColours vividRed $ \h' -> do
            width <- maybe 80 Terminal.width <$> Terminal.hSize h'
            hPutStrLn h' (renderError width)

    exitWith (ExitFailure 1)
  where
    renderError width =
        (`Options.displayS` "") . Options.renderPretty 1.0 width $ Options.hsep
            [ Options.text name <> Options.colon
            , "Error:"
            , fromChunk helpError <> Options.colon
            , Options.text "See"
            , Options.text ("'" <> name <> " help'")
            , Options.text "for more details."
            ]

    fromChunk (Options.Chunk possiblyDoc) =
        fromMaybe "Parsing command line arguments failed." possiblyDoc

    -- TODO: Duplicity.
    vividRed = Terminal.SetColor
        Terminal.Foreground
        Terminal.Vivid
        Terminal.Red
