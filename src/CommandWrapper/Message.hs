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
    , noticeMsg
    , debugMsg

    , withTerminal

    -- * Print Error and Die
    , dieFailedToParseEnvironment
    , dieFailedToParseOptions
    , dieSubcommandNotYetImplemented
    , dieTooManyArguments
    , dieUnableToExecuteSubcommand
    , dieUnableToFindSubcommandExecutable

    -- * Pretty
    , MessageType(..)

    , Result(..)

    , Error(..)
    , errorDoc

    , Warning(..)
    , warningDoc

    , Notice(..)
    , noticeDoc

    , Info(..)
    , infoDoc

    , Debug(..)
    , debugDoc

    , message
    , defaultLayoutOptions
    )
  where

import Control.Exception (bracket_)
import Control.Monad (when)
import Data.Coerce (Coercible, coerce)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(Proxy))
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (Handle, IOMode(WriteMode), hPutStr, hPutStrLn, withFile)

import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty(pretty), (<+>))
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty
import Data.Verbosity (Verbosity(Annoying, Normal, Silent, Verbose))
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
    ( Color(Red)
    , ColorIntensity(Vivid)
    , ConsoleLayer(Foreground)
    , SGR(Reset, SetColor)
    , setSGRCode
    )
import qualified System.Console.Terminal.Size as Terminal
    ( Window(Window, width)
    , hSize
    )

import CommandWrapper.Environment.Parser (ParseEnvError)
import CommandWrapper.Options.ColourOutput (ColourOutput(..), shouldUseColours)


errorMsg
    :: (forall ann. Pretty.Doc ann)
    -- ^ Command that is being executed.  It should either be @TOOLSET@ or
    -- @TOOLSET SUBCOMMAND@.
    -> Verbosity
    -> ColourOutput
    -> Handle
    -> (forall ann. Pretty.Doc ann)
    -- ^ Error message.
    -> IO ()
errorMsg command verbosity colourOutput h msg =
    message defaultLayoutOptions verbosity colourOutput h
        $ errorDoc (command <> Pretty.colon <+> "Error:" <+> msg) <> Pretty.line

warningMsg
    :: (forall ann. Pretty.Doc ann)
    -- ^ Command that is being executed.  It should either be @TOOLSET@ or
    -- @TOOLSET SUBCOMMAND@.
    -> Verbosity
    -> ColourOutput
    -> Handle
    -> (forall ann. Pretty.Doc ann)
    -- ^ Warning message.
    -> IO ()
warningMsg command verbosity colourOutput h msg =
    message defaultLayoutOptions verbosity colourOutput h
        $ warningDoc (command <> Pretty.colon <+> "Warning:" <+> msg) <> Pretty.line

noticeMsg
    :: (forall ann. Pretty.Doc ann)
    -- ^ Command that is being executed.  It should either be @TOOLSET@ or
    -- @TOOLSET SUBCOMMAND@.
    -> Verbosity
    -> ColourOutput
    -> Handle
    -> (forall ann. Pretty.Doc ann)
    -- ^ Debug message.
    -> IO ()
noticeMsg command verbosity colourOutput h msg =
    message defaultLayoutOptions verbosity colourOutput h
        $ noticeDoc (command <> Pretty.colon <+> msg) <> Pretty.line

debugMsg
    :: (forall ann. Pretty.Doc ann)
    -- ^ Command that is being executed.  It should either be @TOOLSET@ or
    -- @TOOLSET SUBCOMMAND@.
    -> Verbosity
    -> ColourOutput
    -> Handle
    -> (forall ann. Pretty.Doc ann)
    -- ^ Debug message.
    -> IO ()
debugMsg command verbosity colourOutput h msg =
    message defaultLayoutOptions verbosity colourOutput h
        $ debugDoc (command <> Pretty.colon <+> "Debug:" <+> msg) <> Pretty.line

hSetSgrCode :: Handle -> Terminal.SGR -> IO ()
hSetSgrCode h code = hPutStr h $ Terminal.setSGRCode [code]

hReset :: Handle -> IO ()
hReset h = hSetSgrCode h Terminal.Reset

-- | Die with exit code @127@ which was choosen based on
-- <http://www.tldp.org/LDP/abs/html/exitcodes.html>
dieUnableToFindSubcommandExecutable
    :: (forall ann. Pretty.Doc ann)
    -- ^ Name under which Command Wrapper was executed.
    -> Verbosity
    -> ColourOutput
    -> Handle
    -> Text
    -- ^ Subcommand name for which we tried to find an executable.
    -> IO a
dieUnableToFindSubcommandExecutable name verbosity colour h subcommand = do
    errorMsg name verbosity colour h
        ( pretty subcommand
        <> Pretty.colon
        <+> "Unable to find suitable executable for this subcommand."
        )
    exitWith (ExitFailure 127)

-- | Die with exit code @126@ which was choosen based on
-- <http://www.tldp.org/LDP/abs/html/exitcodes.html>
dieUnableToExecuteSubcommand
    :: (forall ann. Pretty.Doc ann)
    -- ^ Name under which Command Wrapper was executed.
    -> Verbosity
    -> ColourOutput
    -> Handle
    -> Text
    -- ^ Subcommand name for which we tried to execute the executable.
    -> Text
    -- ^ Executable that we tried to execute.
    -> IO a
dieUnableToExecuteSubcommand name verbosity colour h subcommand executable = do
    errorMsg name verbosity colour h
        ( Pretty.dquotes (pretty subcommand) <> Pretty.colon
        <+> "Unable to execute external subcommand executable:"
        <+> Pretty.dquotes (pretty executable) <> "."
        )
    exitWith (ExitFailure 126)

dieTooManyArguments
    :: (forall ann. Pretty.Doc ann)
    -- ^ Name under which Command Wrapper was executed.
    -> (forall ann. Pretty.Doc ann)
    -- ^ Subcommand name.
    -> Verbosity
    -> ColourOutput
    -> Handle
    -> (forall ann. Pretty.Doc ann)
    -- ^ Argument responsible for the error.
    -> IO a
dieTooManyArguments name subcommand verbosity colourOutput h argument = do
    errorMsg (name <+> subcommand) verbosity colourOutput h
        $ Pretty.dquotes argument <> Pretty.colon <+> "Too many arguments."
    exitWith (ExitFailure 1)

dieSubcommandNotYetImplemented
    :: (forall ann. Pretty.Doc ann)
    -- ^ Name under which Command Wrapper was executed.
    -> Verbosity
    -> ColourOutput
    -> Handle
    -> (forall ann. Pretty.Doc ann)
    -- ^ Name of the subcommand that is not implemented at the moment.
    -> IO a
dieSubcommandNotYetImplemented name verbosity colourOutput h subcommand = do
    errorMsg name verbosity colourOutput h
        ( Pretty.dquotes subcommand <> Pretty.colon
        <+> "Subcommand not yet implemented."
        )
    exitWith (ExitFailure 126)

dieFailedToParseEnvironment
    :: (forall ann. Pretty.Doc ann)
    -- ^ Name under which Command Wrapper was executed.
    -> Verbosity
    -> ColourOutput
    -> Handle
    -> ParseEnvError
    -> IO a
dieFailedToParseEnvironment name verbosity colourOutput h err = do
    errorMsg name verbosity colourOutput h
        $ "Failed to parse environment:" <+> Pretty.viaShow err
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
        withColour useColours vividRed $ \h' -> do
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

    withColour :: Bool -> Terminal.SGR -> (Handle -> IO ()) -> IO ()
    withColour useColours colourSgr action =
        if useColours
            then hSetSgrCode h colourSgr `bracket_` hReset h $ action h
            else action h

    vividRed = Terminal.SetColor
        Terminal.Foreground
        Terminal.Vivid
        Terminal.Red

-- | Provide access to controlling terminal.  This is useful in cases when
-- @stdout@/@stderr@ is redirected or used for something else then
-- communicating with the user.
--
-- TODO: Move to "CommandWrapper.Prelude" module.
withTerminal :: (Handle -> IO a) -> IO a
withTerminal = withFile "/dev/tty" WriteMode

-- {{{ Pretty Messages --------------------------------------------------------

class MessageType ann where
    -- | Minimal 'Verbosity' level when the message should be printed.
    messageMinVerbosity :: proxy ann -> Verbosity

    messageStyle :: ann -> Pretty.AnsiStyle

    default messageStyle
        :: Coercible ann Pretty.AnsiStyle
        => ann
        -> Pretty.AnsiStyle
    messageStyle = coerce

-- | Application result\/output.  It doesn't matter that verbosity is set to
-- 'Silent', we have to print it.
newtype Result ann = Result ann

instance MessageType (Result Pretty.AnsiStyle) where
    messageMinVerbosity _ = Silent

newtype (Error ann) = Error ann

instance MessageType (Error Pretty.AnsiStyle) where
    messageMinVerbosity _ = Normal

errorDoc :: (forall ann. Pretty.Doc ann) -> Pretty.Doc (Error Pretty.AnsiStyle)
errorDoc = Pretty.annotate $ Error (Pretty.color Pretty.Red)

newtype (Warning ann) = Warning ann

instance MessageType (Warning Pretty.AnsiStyle) where
    messageMinVerbosity _ = Normal

warningDoc
    :: (forall ann. Pretty.Doc ann)
    -> Pretty.Doc (Warning Pretty.AnsiStyle)
warningDoc = Pretty.annotate $ Warning (Pretty.color Pretty.Yellow)

newtype (Notice ann) = Notice ann

noticeDoc
    :: (forall ann. Pretty.Doc ann)
    -> Pretty.Doc (Notice Pretty.AnsiStyle)
noticeDoc = Pretty.annotate $ Notice (Pretty.color Pretty.White)

instance MessageType (Notice Pretty.AnsiStyle) where
    messageMinVerbosity _ = Normal

newtype (Info ann) = Info ann

instance MessageType (Info Pretty.AnsiStyle) where
    messageMinVerbosity _ = Verbose

infoDoc :: (forall ann. Pretty.Doc ann) -> Pretty.Doc (Info Pretty.AnsiStyle)
infoDoc = Pretty.annotate $ Info (Pretty.color Pretty.White)

newtype (Debug ann) = Debug ann

instance MessageType (Debug Pretty.AnsiStyle) where
    messageMinVerbosity _ = Annoying

debugDoc :: (forall ann. Pretty.Doc ann) -> Pretty.Doc (Debug Pretty.AnsiStyle)
debugDoc = Pretty.annotate $ Debug (Pretty.color Pretty.White)

-- | Print a message when in appropriate 'Verbosity' level.  Colours are used
-- based on user preferences, and layout is based on terminal size, if using
-- terminal.
message
    :: forall ann
    .  MessageType ann
    => (Maybe (Terminal.Window Int) -> Pretty.LayoutOptions)
    -> Verbosity
    -> ColourOutput
    -> Handle
    -> Pretty.Doc ann
    -> IO ()
message mkLayoutOptions verbosity colourOutput h doc =
    when (verbosity >= messageMinVerbosity (Proxy @ann)) $ do
        useColours <- shouldUseColours h colourOutput
        layoutOptions <- mkLayoutOptions <$> Terminal.hSize h
        putDoc useColours layoutOptions doc
  where
    putDoc :: Bool -> Pretty.LayoutOptions -> Pretty.Doc ann -> IO ()
    putDoc useColours opts =
        Pretty.renderIO h . reAnnotate useColours . Pretty.layoutPretty opts

    reAnnotate
        :: Bool
        -> Pretty.SimpleDocStream ann
        -> Pretty.SimpleDocStream Pretty.AnsiStyle
    reAnnotate useColours =
        if useColours
            then Pretty.reAnnotateS messageStyle
            else Pretty.unAnnotateS

defaultLayoutOptions :: Maybe (Terminal.Window Int) -> Pretty.LayoutOptions
defaultLayoutOptions = Pretty.LayoutOptions . \case
    Nothing -> Pretty.Unbounded
    Just Terminal.Window{width} -> Pretty.AvailablePerLine width 1.0

-- }}} Pretty Messages --------------------------------------------------------
