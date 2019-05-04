{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module:      CommandWrapper.Prelude
-- Description: Give subcommands everything they need to seamlessly integrate.
-- Copyright:   (c) 2018-2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- Give subcommands everything they need to seamlessly integrate.
module CommandWrapper.Prelude
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

    -- ** Completion Info
    , HaveCompletionInfo(..)
    , completionInfoOptionFields
    , completionInfoFlag
    , printOptparseCompletionInfoExpression

    -- * Error Handling
    --
    -- | These functions respect 'verbosity' and 'colour'.
    , errorMsg
    , warningMsg
    , dieWith

    -- ** IO
    --
    -- | Some of these are just reexported from "System.IO" from `base` package.
    , stderr
    , stdout
    , Message.withTerminal
    )
  where

import Data.Bool ((||))
import Data.Function (id)
import Data.Int (Int)
import Data.Monoid ((<>))
import Data.Ord ((<=), (>))
import System.IO (Handle, IO, stderr, stdout)
import System.Environment (getProgName)
import System.Exit (ExitCode(ExitFailure), exitWith)

import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty(pretty), (<+>))
import qualified Data.Text.Prettyprint.Doc as Pretty
import Data.Verbosity (Verbosity(..))
import qualified Dhall.Core as Dhall (Expr)
import qualified Dhall.Parser as Dhall (Src)
import qualified Dhall.Pretty as Dhall (CharacterSet(Unicode))
import qualified Dhall.TH (staticDhallExpression)
import qualified Dhall.TypeCheck as Dhall (X)
import qualified Options.Applicative as Options
    ( HasName
    , Mod
    , Parser
    , flag
    , internal
    , long
    )

import qualified CommandWrapper.Internal.Dhall as Dhall (hPutExpr)
import CommandWrapper.Environment.Params (Params(..), askParams)
import CommandWrapper.Environment.Parser (parseEnvIO)
import qualified CommandWrapper.Message as Message
    ( errorMsg
    , warningMsg
    , withTerminal
    )
import CommandWrapper.Options.ColourOutput (ColourOutput(..), shouldUseColours)


-- | Specialised version of @'parseEnvIO' onError 'askParams'@ that handles
-- edge cases correctly.
subcommandParams :: IO Params
subcommandParams = parseEnvIO protocolError askParams
  where
    protocolError err = do
        -- This is not the best thing we can use, but it's close enough to the
        -- `TOOLSET SUBCOMMAND@ format that it could be useful to the users.
        name <- getProgName
        Message.errorMsg (pretty name) Normal Auto stderr (message err)

        -- See `command-wrapper-subcommand-protocol(1)` manual page section
        -- /EXIT STATUS/.
        exitWith (ExitFailure 1)

    message err =
        "This command must be executed inside Command Wrapper environment:"
        <+> "Failed to parse environment variables:"
        <+> Pretty.viaShow err <> Pretty.colon
        <+> "See command-wrapper-subcommand-protocol(1) manual page for more"
        <+> "details."

errorMsg :: Params -> Handle -> Text -> IO ()
errorMsg Params{colour, name, subcommand, verbosity} h msg =
    let cmd = name <> " " <> subcommand
    in Message.errorMsg (pretty cmd) verbosity colour h (pretty msg)

warningMsg :: Params -> Handle -> Text -> IO ()
warningMsg Params{colour, name, subcommand, verbosity} h msg =
    let cmd = name <> " " <> subcommand
    in Message.warningMsg (pretty cmd) verbosity colour h (pretty msg)

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
    Options.flag id completionInfoMode completionInfoOptionFields

completionInfoOptionFields :: Options.HasName f => Options.Mod f a
completionInfoOptionFields =
    Options.long "completion-info" <> Options.internal

printOptparseCompletionInfoExpression
    :: Handle
    -- ^ Output handle.
    -> IO ()
printOptparseCompletionInfoExpression outHandle =
    let completionInfo :: Dhall.Expr Dhall.Src Dhall.X =
            $(Dhall.TH.staticDhallExpression
                "./dhall/optparse-completion-info.dhall"
            )

     in Dhall.hPutExpr Never Dhall.Unicode outHandle completionInfo

-- TODO:
--
-- Open controlling terminal and use it to print a message.  This is useful in
-- case when @stdout@\/@stderr@ is redirected or being used for other purposes.
--
--withTerminal :: (Handle -> Text -> IO a) -> Text -> IO a
