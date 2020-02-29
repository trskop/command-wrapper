{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      $Header$
-- Description: Utilities used by internal subcommands.
-- Copyright:   (c) 2018-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Utilities used by internal subcommands.
module CommandWrapper.Toolset.InternalSubcommand.Utils
    ( runMain

    -- * Options
    , dhallFlag
    , executableOption
    , helpFlag
    , toolsetOption
    )
  where

import Control.Applicative (pure)
import Data.Either (Either(Left, Right))
import Data.Function (($), (.))
import Data.Functor (Functor)
import Data.Maybe (Maybe, fromMaybe)
import Data.Monoid (Monoid, Endo, (<>), mconcat, mempty)
import Data.String (IsString, String, fromString)
import System.IO (IO)

import qualified Mainplate (noConfigToRead, runAppWith)
import qualified Options.Applicative as Options
    ( Parser
    , ReadM
    , eitherReader
    , flag
    , long
    , metavar
    , option
    , short
    )


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

-- | Parse `--help`, `-h` flags.
helpFlag :: Monoid a => a -> Options.Parser a
helpFlag whenPresent = Options.flag mempty whenPresent $ mconcat
    [ Options.short 'h'
    , Options.long "help"
    ]

-- | Parse `--dhall` flags.
dhallFlag :: Monoid a => a -> Options.Parser a
dhallFlag whenPresent = Options.flag mempty whenPresent (Options.long "dhall")

-- | Parse `--executable=PATH` option, where the name of the metavariable
-- `PATH` is configurable.
executableOption
    :: forall s
    .  IsString s
    => Maybe String
    -- ^ Metavariable name, as in `--executable=METAVAR`.  If set to `Nothing`
    -- then `"PATH"` is used, i.e. `--executable=PATH`.
    -> Options.Parser s
executableOption metavar = Options.option parse $ mconcat
    [ Options.long "executable"
    , Options.metavar metavar'
    ]
  where
    metavar' :: String
    metavar' = fromMaybe "PATH" metavar

    parse :: Options.ReadM s
    parse = Options.eitherReader \case
        "" -> Left ("Expected " <> metavar' <> ", but got empty path instead")
        s  -> Right (fromString s)

-- | Parse `--toolset=NAME` option, where the name of the metavariable `NAME`
-- is configurable.
toolsetOption
    :: forall s
    .  IsString s
    => Maybe String
    -- ^ Metavariable name, as in `--toolset=METAVAR`.  If set to `Nothing`
    -- then `"NAME"` is used, i.e. `--toolset=NAME`.
    -> Options.Parser s
toolsetOption metavar = Options.option parse $ mconcat
    [ Options.long "toolset"
    , Options.metavar metavar'
    ]
  where
    metavar' :: String
    metavar' = fromMaybe "TOOLSET_NAME" metavar

    parse :: Options.ReadM s
    parse = Options.eitherReader \case
        "" -> Left ("Expected " <> metavar' <> ", but got empty string instead")
        s  -> Right (fromString s)
