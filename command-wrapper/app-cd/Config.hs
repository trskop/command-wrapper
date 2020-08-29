-- |
-- Module:      Config
-- Description: Representation and parsing of configuration file for cd
--              subcommand
-- Copyright:   (c) 2018-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Representation and parsing of configuration file for @cd@ subcommand.
module Config
    ( Config(..)
    , defConfig
    , readConfig

    , ShellCommand(..)
    , shellCommand
    )
  where

import Data.Functor ((<$>))
import Data.Maybe (Maybe(Nothing), fromMaybe)
import GHC.Generics (Generic)
import System.IO (IO)

import Data.Text (Text)
import Dhall (FromDhall, ToDhall)
import qualified Dhall (auto)

import CommandWrapper.Subcommand.Prelude (Params, inputConfig)
import CommandWrapper.Toolset.Config.Command (SimpleCommand)


-- | Representation of @cd@ subcommand configuration file.
data Config = Config
    { directories :: [Text]
    -- ^ List of directories for the user to choose from.
    , menuTool :: Maybe (Maybe Text -> SimpleCommand)
    -- ^ Command to use when requesting user to select a directory from a list.
    --
    -- If 'Nothing' then very basic TUI implementation is used. Tools that can
    -- easily be used for this purpose are @fzf@ and @fzy@
    , shell :: Maybe Text
    -- ^ Shell to use when spawning a new shell. If 'Nothing' then the value
    -- from @SHELL@ environment variable is used.
    , terminalEmulator :: Maybe (Text -> Maybe ShellCommand -> SimpleCommand)
    -- ^ Terminal emulator to run when such action is requested.
    --
    -- If 'Nothing' then we don'd assume anything and die with appropriate
    -- error message.
    }
  deriving stock (Generic)
  deriving anyclass (Dhall.FromDhall)

-- | Empty 'Config' used when there is no configuration file available.
defConfig :: Config
defConfig = Config
    { directories = []
    , menuTool = Nothing
    , shell = Nothing
    , terminalEmulator = Nothing
    }

-- | Representation of command that can be passed to terminal emulator to be
-- executed.
data ShellCommand = ShellCommand
    { command :: Text
    , arguments :: [Text]
    }
  deriving stock (Generic)
  deriving anyclass (Dhall.ToDhall)

-- | Smart constructor for 'ShellCommand'.
shellCommand :: Text -> ShellCommand
shellCommand shell = ShellCommand shell []

-- | Read and parse configuration file.
readConfig :: Params -> IO Config
readConfig params = fromMaybe defConfig <$> inputConfig Dhall.auto params
