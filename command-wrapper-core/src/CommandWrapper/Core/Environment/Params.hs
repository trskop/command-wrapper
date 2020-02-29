-- |
-- Module:      $Header$
-- Description: Subcommand parameters as defined by Subcommand Protocol.
-- Copyright:   (c) 2018-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Subcommand parameters as defined by Subcommand Protocol.  See
-- @command-wrapper-subcommand-protocol(7)@ manual page for its definition.
module CommandWrapper.Core.Environment.Params
    (
      Params(..)

    , subcommandProtocolVersion

    -- * Environment Builder
    , mkEnvVars
    , commandWrapperEnv

    -- * Environment Parser
    , askParams
    )
  where

import Prelude (maxBound, minBound)

import Control.Applicative ((<*>), pure)
import Control.Monad ((>>=))
import Data.Bifunctor (bimap, first)
import qualified Data.Char as Char (toLower)
import Data.Eq ((/=))
import Data.Foldable (foldMap)
import Data.Function (($), (.))
import Data.Functor ((<$>), fmap)
import qualified Data.List as List (dropWhile, filter)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Monoid (Endo(Endo, appEndo))
import Data.Semigroup (Semigroup((<>)))
import Data.String (String, fromString)
import Data.Tuple (snd, uncurry)
import Data.Version (Version, makeVersion, parseVersion, showVersion)
import GHC.Generics (Generic)
import System.IO (FilePath)
import Text.Show (Show, show)
import Text.ParserCombinators.ReadP (readP_to_S)

import Control.Monad.Except (throwError)
import qualified Data.CaseInsensitive as CaseInsensitive (mk)
import qualified Data.HashMap.Strict as HashMap (delete, fromList, toList)
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import Data.Text.Prettyprint.Doc (Doc, Pretty(pretty), (<+>))
import qualified Data.Text.Prettyprint.Doc as Pretty (dquotes, viaShow, vsep)

import CommandWrapper.Core.Config.ColourOutput (ColourOutput)
import qualified CommandWrapper.Core.Config.ColourOutput as ColourOutput (parse)
import CommandWrapper.Core.Config.Verbosity (Verbosity)
import qualified CommandWrapper.Core.Config.Verbosity as Verbosity (parse)
import CommandWrapper.Core.Environment.Builder (EnvBuilder(EnvBuilder))
import CommandWrapper.Core.Environment.Parser
    ( ParseEnv
    , ParseEnvError(ParseEnvError)
    , commandWrapperVar
    , commandWrapperVar'
    )
import CommandWrapper.Core.Environment.AppNames
    ( AppNames(AppNames, commandWrapperPrefix)
    )
import CommandWrapper.Core.Environment.Variable
    ( CommandWrapperPrefix
    , CommandWrapperVarName(..)
    , EnvVarName
    , EnvVarValue
    , getCommandWrapperToolsetVarName
    , getCommandWrapperVarName
    , isVariableRemovedBeforeInvokingExternalCommand
    )

-- | Subcommand parameters.  They are used to populate environment variables
-- used in Subcommand Protocol.
--
-- See also 'mkEnvVars' and @command-wrapper-subcommand-protocol(7)@ manual
-- page for more details.
data Params = Params
    { exePath :: FilePath
    -- ^ Full path to Command Wrapper's executable, symlinks are resolved.
    --
    -- For example, default installation path of Command Wrapper for user
    -- @\"neo\"@ is:
    --
    -- @
    -- 'exePath' = \"\/home\/neo\/.local\/lib\/command-wrapper\/command-wrapper\"
    -- @
    --
    -- This value is passed via @COMMAND_WRAPPER_EXE@ environment variable.
    -- See @command-wrapper-subcommand-protocol(7)@ manual page for details.

    , name :: String
    -- ^ Name under which the 'exePath' executable was executed.
    --
    -- For example if user invoked command @yx SOMETHING@, which was resolved
    -- by shell into a symlink:
    --
    -- > "/home/neo/bin/yx" -> "/home/neo/.local/lib/command-wrapper/command-wrapper"
    --
    -- Then:
    --
    -- @
    -- 'name' = \"yx\"
    -- @
    --
    -- This value is passed via @COMMAND_WRAPPER_NAME@ environment variable.
    -- See @command-wrapper-subcommand-protocol(7)@ manual page for details.

    , subcommand :: String
    -- ^ Name of the subcommand that is being executed from the perspective
    -- of Command Wrapper.  This is not a file path.
    --
    -- For example if user invoked command @yx SOMETHING@ then 'subcommand'
    -- will contain @SOMETHING@.
    --
    -- This value is passed via @COMMAND_WRAPPER_SUBCOMMAND@ environment
    -- variable.  See @command-wrapper-subcommand-protocol(7)@ manual page for
    -- details.

    , config :: Text
    -- ^ Contains a Dhall expression that represents subcommand configuration.
    --
    -- This value is passed via @COMMAND_WRAPPER_CONFIG@ environment variable.
    -- See @command-wrapper-subcommand-protocol(7)@ manual page for details.

    , verbosity :: Verbosity
    -- ^ Verbosity settings passed down from Command Wrapper.
    --
    -- This value is passed via @COMMAND_WRAPPER_VERBOSITY@ environment
    -- variable.  See @command-wrapper-subcommand-protocol(7)@ manual page for
    -- details.

    , colour :: ColourOutput
    -- ^ Colour output settings passed down from Command Wrapper.
    --
    -- This value is passed via @COMMAND_WRAPPER_COLOUR@ environment variable.
    -- See @command-wrapper-subcommand-protocol(7)@ manual page for details.

    , version :: Version
    -- ^ Version of subcommand protocol that Command Wrapper expects the
    -- subcommand to respect.
    --
    -- This value is passed via @COMMAND_WRAPPER_VERSION@ environment variable.
    -- See @command-wrapper-subcommand-protocol(7)@ manual page for details.
    }
  deriving stock (Generic, Show)

instance Pretty Params where
    pretty :: Params -> Doc ann
    pretty Params{..} = Pretty.vsep
        [ "exePath" <+> Pretty.dquotes (pretty exePath)
        , "name" <+> Pretty.dquotes (pretty name)
        , "subcommand" <+> Pretty.dquotes (pretty subcommand)
        , "config" <+> Pretty.dquotes (pretty config)
        , "verbosity" <+> Pretty.viaShow verbosity
        , "colour" <+> Pretty.viaShow colour
        , "version" <+> Pretty.dquotes (pretty (showVersion version))
        ]

-- | Subcommand protocol is versioned separately from Command Wrapper library
-- and Command Wrapper tool.
subcommandProtocolVersion :: Version
subcommandProtocolVersion = makeVersion [1, 0, 0]

-- | Build environment as defined by Subcommand Protocol.  This has to be used
-- when calling a Command Wrapper subcommand.
--
-- See also 'Params' and @command-wrapper-subcommand-protocol(7)@ manual page
-- for more details.
mkEnvVars :: Params -> EnvBuilder CommandWrapperPrefix
mkEnvVars Params{..} = EnvBuilder $ \prefix ->
    HashMap.fromList $ fmap (first $ getCommandWrapperVarName prefix)
        [ (CommandWrapperExe, fromString exePath)
        , (CommandWrapperName, fromString name)
        , (CommandWrapperSubcommand, fromString subcommand)
        , (CommandWrapperConfig, config)
        , (CommandWrapperVerbosity, fromString $ Char.toLower <$> show verbosity)
        , (CommandWrapperColour, fromString $ Char.toLower <$> show colour)
        , (CommandWrapperVersion, fromString $ showVersion version)
        ]

-- | Evaluate environment builder intended for executing a command.  It removes
-- environment variables defined in 'CommandWrapperToolsetVarName' to avoid
-- potential leak of abstraction.  Especially if executed command is another
-- Command Wrapper instance.
commandWrapperEnv
    :: AppNames
    -> EnvBuilder CommandWrapperPrefix
    -> [(String, String)]
commandWrapperEnv AppNames{commandWrapperPrefix} (EnvBuilder mkEnv) =
    fromHashMap (removeToolsetVars `appEndo` mkEnv commandWrapperPrefix)
  where
    fromHashMap = fmap (bimap Text.unpack Text.unpack) . HashMap.toList

    removeToolsetVars = foldMap
        ( Endo
            . HashMap.delete
            . getCommandWrapperToolsetVarName commandWrapperPrefix
        )
        ( List.filter isVariableRemovedBeforeInvokingExternalCommand
            [minBound .. maxBound]
        )

-- | Parse Command Wrapper environment variables that are part of subcommand
-- protocol.
--
-- See also 'Params' and @command-wrapper-subcommand-protocol(7)@ manual page
-- for more details.
askParams :: ParseEnv CommandWrapperPrefix Params
askParams = Params
    <$> var CommandWrapperExe
    <*> var CommandWrapperName
    <*> var CommandWrapperSubcommand
    <*> commandWrapperVar CommandWrapperConfig
    <*> verbosityVar CommandWrapperVerbosity
    <*> colourOutputVar CommandWrapperColour
    <*> versionVar CommandWrapperVersion
  where
    var = fmap Text.unpack . commandWrapperVar

    verbosityVar name =
        commandWrapperVar' name >>= uncurry parseAsVerbosity

    colourOutputVar name =
        commandWrapperVar' name >>= uncurry parseAsColourOutput

    versionVar name =
        commandWrapperVar' name >>= uncurry parseAsVersion

    parseAsVerbosity
        :: EnvVarName
        -> EnvVarValue
        -> ParseEnv CommandWrapperPrefix Verbosity
    parseAsVerbosity name value =
        maybe (unableToParseVerbosity name value) pure
        . Verbosity.parse
        $ CaseInsensitive.mk value

    unableToParseVerbosity
        :: forall a
        .  EnvVarName
        -> EnvVarValue
        -> ParseEnv CommandWrapperPrefix a
    unableToParseVerbosity = throwParseEnvError "verbosity"

    parseAsColourOutput
        :: EnvVarName
        -> EnvVarValue
        -> ParseEnv CommandWrapperPrefix ColourOutput
    parseAsColourOutput name value =
        maybe (unableToParseColourOutput name value) pure
        . ColourOutput.parse
        $ CaseInsensitive.mk value

    unableToParseColourOutput
        :: forall a
        .  EnvVarName
        -> EnvVarValue
        -> ParseEnv CommandWrapperPrefix a
    unableToParseColourOutput = throwParseEnvError "colour output settings"

    parseAsVersion
        :: EnvVarName
        -> EnvVarValue
        -> ParseEnv CommandWrapperPrefix Version
    parseAsVersion name value = maybe (unableToParseVersion name value) pure
        $ case parseVersion' value of
            (v, "") : _ -> Just v
            _           -> Nothing
      where
        parseVersion' =
            List.dropWhile ((/= "") . snd)
            . readP_to_S parseVersion
            . Text.unpack

    unableToParseVersion
        :: forall a
        .  EnvVarName
        -> EnvVarValue
        -> ParseEnv CommandWrapperPrefix a
    unableToParseVersion = throwParseEnvError "version"

    throwParseEnvError
        :: forall a
        .  String
        -> EnvVarName
        -> EnvVarValue
        -> ParseEnv CommandWrapperPrefix a
    throwParseEnvError what name s = throwError (ParseEnvError name msg)
      where
        msg = "'" <> Text.unpack s <> "': Unable to parse " <> what <> "."
