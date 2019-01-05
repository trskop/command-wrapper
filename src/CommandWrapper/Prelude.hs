{-# LANGUAGE NoImplicitPrelude #-}
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
    )
  where

import Data.Bool ((||))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Monoid ((<>))
import Data.Ord ((<=), (>))
import Data.String (fromString)
import System.IO (Handle, IO, stderr, stdout)
import System.Environment (getProgName)
import System.Exit (ExitCode(ExitFailure), exitWith)
import Text.Show (show)

import Data.Text (Text)
import Data.Verbosity (Verbosity(..))

import CommandWrapper.Environment.Params(Params(..), askParams)
import CommandWrapper.Environment.Parser(parseEnvIO)
import qualified CommandWrapper.Message as Message (errorMsg)
import CommandWrapper.Options.ColourOutput (ColourOutput(..), shouldUseColours)


-- | Specialised version of @'parseEnvIO' onError 'askParams'@ that handles
-- edge cases correctly.
subcommandParams :: IO Params
subcommandParams = parseEnvIO protocolError askParams
  where
    protocolError err = do
        -- This is not the best thing we can use, but it's close enough to the
        -- `TOOLSET SUBCOMMAND@ format that it could be useful to the users.
        name <- fromString <$> getProgName
        Message.errorMsg name Normal Auto stderr (message err)

        -- See `command-wrapper-subcommand-protocol(1)` manual page section
        -- /EXIT STATUS/.
        exitWith (ExitFailure 1)

    message err =
        "This command must be executed inside Command Wrapper environment:"
        <> " Failed to parse environment variables: "
        <> fromString (show err)
        <> ": See command-wrapper-subcommand-protocol(1) manual page for more"
        <> " details."

errorMsg :: Params -> Handle -> Text -> IO ()
errorMsg Params{colour, name, subcommand, verbosity} =
    let cmd = fromString name <> " " <> fromString subcommand
    in Message.errorMsg cmd verbosity colour

warningMsg :: Params -> Handle -> Text -> IO ()
warningMsg Params{colour, name, subcommand, verbosity} =
    let cmd = fromString name <> " " <> fromString subcommand
    in Message.errorMsg cmd verbosity colour

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

-- TODO:
--
-- Open controlling terminal and use it to print a message.  This is useful in
-- case when @stdout@\/@stderr@ is redirected or being used for other purposes.
--
--withTerminal :: (Handle -> Text -> IO a) -> Text -> IO a
