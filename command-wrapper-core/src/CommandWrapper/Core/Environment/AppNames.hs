-- |
-- Module:      $Header$
-- Description: Representation of all the names and paths under wich Command
--              Wrapper toolset is known.
-- Copyright:   (c) 2018-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Representation of all the names and paths under wich Command Wrapper toolset
-- is known.  These are used to search for subcommands, populate environment
-- variables for subcommands, in error messages, etc.
--
-- See also @command-wrapper(1)@ and @command-wrapper-subcommand-protocol(7)@
-- manual pages.
module CommandWrapper.Core.Environment.AppNames
    (
    -- * Application Names
      AppNames(..)
    , AppNameError(..)
    , getAppNames

    -- * Executable Path
    , ExecutablePath(..)
    , getExecutablePath
    )
  where

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Eq (Eq, (==))
import Data.Function ((.))
import Data.Functor ((<$>), (<&>), fmap)
import Data.List.NonEmpty (NonEmpty((:|)), toList)
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Version (Version, showVersion)
import GHC.Generics (Generic)
import System.Environment (getProgName, lookupEnv)
import System.IO (FilePath, IO)
import Text.Show (Show)

import qualified Data.Text as Text (unpack)
import Data.Text.Prettyprint.Doc (Doc, Pretty(pretty), (<+>))
import qualified Data.Text.Prettyprint.Doc as Pretty
    ( comma
    , dquote
    , dquotes
    , encloseSep
    , vsep
    )
import System.Directory
    ( doesFileExist
    , executable
    , getPermissions
    , makeAbsolute
    )
import qualified System.Environment.Executable as Executable
    ( ScriptPath(..)
    , getScriptPath
    )
import System.FilePath (takeFileName)

import CommandWrapper.Core.Environment.Variable
    ( CommandWrapperPrefix
    , CommandWrapperToolsetVarName
        ( CommandWrapperFacade
        , CommandWrapperInvokeAs
        )
    , getCommandWrapperToolsetVarName
    )


data AppNames = AppNames
    { exePath :: FilePath
    -- ^ Full path to the executable, symlinks are resolved.  If environment
    -- variable @\<prefix\>_FACADE@ is set then that value is used instead.
    -- See 'CommandWrapperFacade' data constructor for more information.
    --
    -- For example, default installation path of Command Wrapper for user
    -- @\"neo\"@ is:
    --
    -- @
    -- 'exePath' = \"\/home\/neo\/.local\/lib\/command-wrapper\/command-wrapper\"
    -- @

    , exeName :: String
    -- ^ Executable file name
    --
    -- For example, if
    --
    -- @
    -- 'exePath' = \"\/home\/neo\/.local\/lib\/command-wrapper\/command-wrapper\"
    -- @
    --
    -- Then
    --
    -- @
    -- 'exeName' = \"command-wrapper\"
    -- @

    , exeVersion :: Version
    -- ^ Version number of the executable.  How this information is gathered
    -- is intentionally left vague.

    , usedName :: String
    -- ^ Name under which the executable was executed.
    --
    -- For example if user invoked command @yx SOMETHING@, which was resolved
    -- by shell into a symlink:
    --
    -- > "/home/neo/bin/yx" -> "/home/neo/.local/lib/command-wrapper/command-wrapper"
    --
    -- Then:
    --
    -- @
    -- 'usedName' = \"yx\"
    -- @
    --
    -- This value can be overriden by an environment variable
    -- @<prefix>_INVOKE_AS=<used-name>@. See 'CommandWrapperPrefix',
    -- 'CommandWrapperInvokeAs', and @command-wrapper(1)@ manual page section
    -- /ENVIRONMENT VARIABLES/.

    , names :: NonEmpty String
    -- ^ List of names under which the Command Wrapper executable is known.
    -- Order is significant when searching for a subcommand.  First name has
    -- precedence.
    --
    -- For example if @'exeName' = \"command-wrapper\"@, and
    -- @'usedName' = \"yx\"@ then:
    --
    -- @
    -- 'names' = \"yx\" :| [\"command-wrapper\"]
    -- @

    , commandWrapperPrefix :: CommandWrapperPrefix
    -- ^ Prefix shared by all Command Wrapper environment variables.
    }
  deriving stock (Generic, Show)

instance Pretty AppNames where
    pretty :: AppNames -> Doc ann
    pretty AppNames{..} = Pretty.vsep
        [ "exePath" <+> Pretty.dquotes (pretty exePath)
        , "exeName" <+> Pretty.dquotes (pretty exeName)
        , "exeVersion" <+> Pretty.dquotes (pretty (showVersion exeVersion))
        , "usedName" <+> Pretty.dquotes (pretty usedName)

        , "names"
            <+> Pretty.encloseSep Pretty.dquote Pretty.dquote
                    (Pretty.dquote <> Pretty.comma <+> Pretty.dquote)
                    (pretty <$> toList names)

        , "commandWrapperPrefix"
            <+> Pretty.dquotes (pretty commandWrapperPrefix)
        ]

-- | Application name resolution error.
data AppNameError
    = RunningInInteractiveInterpreterError
    -- ^ We are unable to figure out executable paths when running in
    -- interactive mode.  Use @\<prefix\>_FACADE@ environment variable to
    -- override the resolution algorithm.

    | FacadeDoesNotExistOrIsNotAFileError String
    -- ^ Path passed via @\<prefix\>_FACADE@ environment variable is not
    -- pointing to a file.

    | FacadeIsNotExecutableError String
    -- ^ Executable path passed via @\<prefix\>_FACADE@ environment variable
    -- is not pointing to an executable.
  deriving stock (Eq, Generic, Show)

-- | Smart constructor for 'AppNames'.
--
-- Lets say that we have a following installation:
--
-- > ~/
-- > ├── bin/
-- > │   ├── yx -> ../.local/lib/command-wrapper/command-wrapper
-- > │   └── ...
-- > ├── .local/lib/command-wrapper/
-- > │   ├── command-wrapper
-- > │   └── ...
-- > └── ...
--
-- If user @\"neo\"@ invokes @yx SOMETHING@ command and @\"$HOME\/bin\"@ is in
-- their @\"$PATH\"@ then return value of this function would be:
--
-- @
-- 'AppNames'
--     { 'exePath' = \"/home\/neo\/.local\/lib\/command-wrapper\/command-wrapper\"
--     , 'exeName' = \"command-wrapper\"
--     , 'exeVersion' = ... -- Whatever the second argument of 'getAppNames' returned.
--     , 'usedName' = \"yx\"
--     , 'names' = \"yx\" :| [\"command-wrapper\"]
--     }
-- @
--
-- See also 'AppNames', 'CommandWrapperInvokeAs' and 'CommandWrapperFacade' for
-- more information on how this works and why it's done.
getAppNames
    :: CommandWrapperPrefix
    -- ^ Prefix used by Command Wrapper's environment variables.  Usually
    -- 'CommandWrapper.Environment.Variable.defaultCommandWrapperPrefix'.
    -> IO Version
    -> IO (Either AppNameError AppNames)
getAppNames prefix getVersion = do
    usedName <- getUsedName
    version <- getVersion
    fmap (appNamesWithExePath usedName version) <$> getExecutablePath prefix
  where
    appNamesWithExePath :: String -> Version -> ExecutablePath -> AppNames
    appNamesWithExePath usedName exeVersion exePath' =
        let exePath =  case exePath' of
                Executable e -> e
                Facade     e -> e

            exeName = takeFileName exePath

        in AppNames
            { exePath
            , exeName
            , exeVersion
            , usedName

            -- More specific name has priority, i.e. user defined toolset has
            -- preference from generic 'command-wrapper' commands.
            , names = usedName :| if exeName == usedName then [] else [exeName]
            , commandWrapperPrefix = prefix
            }

    getUsedName :: IO String
    getUsedName = do
        -- Environment variable @<prefix>_INVOKE_AS=<used-name>@ overrides the
        -- default value of 'usedName'. This is useful for testing, and when
        -- subcommands call other subcommands.  With this there is no need to
        -- pass around path to toolset binary/symlink.
        let invokeAsVarName =
                getCommandWrapperToolsetVarName prefix CommandWrapperInvokeAs

        possiblyOverrideProgName <- lookupEnv (Text.unpack invokeAsVarName)
        maybe getProgName pure possiblyOverrideProgName

-- | Resolved path to Command Wrapper executable.
data ExecutablePath
    = Executable FilePath
    -- ^ Resolved absolute file path to Command Wrapper executable.

    | Facade FilePath
    -- ^ Environment variable @\<prefix\>_FACADE@ was supplied, providing it's
    -- value instead.
  deriving (Eq, Generic, Show)

-- | Figure out absolute path to Command Wrapper executable.
--
-- See also 'ExecutablePath', 'AppNames' (especially 'exePath'), and
-- 'CommandWrapperFacade' for more information on how this works and why it's
-- done.
getExecutablePath
    :: CommandWrapperPrefix
    -> IO (Either AppNameError ExecutablePath)
getExecutablePath prefix = lookupEnv (Text.unpack facadeVarName) >>= \case
    Just exePath -> do
        facadeExists <- doesFileExist exePath
        if facadeExists
            then do
                facadeIsExecutable <- isExecutable exePath
                if facadeIsExecutable
                    then Right . Facade <$> makeAbsolute exePath
                    else pure (Left (FacadeIsNotExecutableError exePath))
            else
                pure (Left (FacadeDoesNotExistOrIsNotAFileError exePath))
    Nothing ->
        Executable.getScriptPath <&> \case
            Executable.Executable exePath ->
                Right (Executable  exePath)

            Executable.RunGHC exePath ->
                Right (Executable exePath)

            Executable.Interactive ->
                Left RunningInInteractiveInterpreterError
  where
    facadeVarName = getCommandWrapperToolsetVarName prefix CommandWrapperFacade

    isExecutable fp = executable <$> getPermissions fp
