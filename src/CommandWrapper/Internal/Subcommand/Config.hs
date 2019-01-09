{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.Internal
-- Description: Implementation of internal command named config
-- Copyright:   (c) 2018-2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Implementation of internal command named @config@.
module CommandWrapper.Internal.Subcommand.Config
    ( ConfigMode(..)
    , config
    , configSubcommandHelp
    )
  where

import Control.Applicative (pure)
import Data.Function (($))
import Data.Functor (Functor)
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo, (<>))
import Data.String (String, fromString)
import GHC.Generics (Generic)
import System.IO (IO, stderr, stdout)
import Text.Show (Show)

import Data.Text.Prettyprint.Doc ((<+>))
import qualified Data.Text.Prettyprint.Doc as Pretty (Doc, squotes, vsep)
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty (AnsiStyle)
import qualified Data.Text.Prettyprint.Doc.Util as Pretty (reflow)
import qualified Mainplate (applySimpleDefaults)

import qualified CommandWrapper.Config.Global as Global (Config(..))
import CommandWrapper.Environment (AppNames(AppNames, usedName))
import CommandWrapper.Internal.Subcommand.Help
    ( globalOptionsHelp
    , helpOptions
    , metavar
    , optionDescription
    , optionalMetavar
    , section
    , toolsetCommand
    , usageSection
    )
import CommandWrapper.Internal.Utils (runMain)
import CommandWrapper.Message
    ( Result
    , defaultLayoutOptions
    , dieSubcommandNotYetImplemented
    , message
    )
import qualified CommandWrapper.Options.ColourOutput as ColourOutput
    ( ColourOutput(Auto)
    )


data ConfigMode a
    = InitConfig a
    | ConfigLib a
    | Dhall a
    | ConfigHelp a
  deriving (Functor, Generic, Show)

config :: AppNames -> [String] -> Global.Config -> IO ()
config appNames@AppNames{usedName} _options globalConfig =
    runMain parseOptions defaults $ \case
        InitConfig _ -> pure ()
        ConfigLib _ -> pure ()

        ConfigHelp _ ->
            let Global.Config{colourOutput, verbosity} = globalConfig
                colour = fromMaybe ColourOutput.Auto colourOutput
            in message defaultLayoutOptions verbosity colour stdout
                (configSubcommandHelp appNames)

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
    parseOptions =
        let Global.Config
                { Global.verbosity
                , Global.colourOutput = possiblyColourOutput
                } = globalConfig

        in dieSubcommandNotYetImplemented (fromString usedName) verbosity
            (fromMaybe ColourOutput.Auto possiblyColourOutput) stderr "config"

configSubcommandHelp :: AppNames -> Pretty.Doc (Result Pretty.AnsiStyle)
configSubcommandHelp AppNames{usedName} = Pretty.vsep
    [ usageSection usedName
        [ "config" <+> optionalMetavar "EXPRESSION"
        , "config" <+> helpOptions
        , "help config"
        ]

    , section "Options:"
        [ optionDescription ["EPRESSION"]
            [ "Dhall", metavar "EXPRESSION"
            , Pretty.reflow "that will be applied to configuration."
            ]

        , optionDescription ["--help", "-h"]
            [ Pretty.reflow "Print this help and exit. Same as"
            , Pretty.squotes (toolsetCommand usedName "help config") <> "."
            ]

        , globalOptionsHelp usedName
        ]

    , ""
    ]
