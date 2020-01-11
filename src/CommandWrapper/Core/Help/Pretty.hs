{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.Core.Help.Pretty
-- Description: Combinators and helper functions for constructing help message
--              using prettyprint library.
-- Copyright:   (c) 2018-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- Combinators and helper functions for constructing help message using
-- prettyprint library.
module CommandWrapper.Core.Help.Pretty
    ( (<++>)
    , command
    , dullGreen
    , globalOptions
    , globalOptionsHelp
    , helpOptions
    , longOption
    , longOptionWithArgument
    , metavar
    , optionDescription
    , optionalMetavar
    , optionalSubcommand
    , section
    , shortOption
    , subcommand
    , toolsetCommand
    , usageSection
    , value
    )
  where

import Prelude (fromIntegral)

import Control.Applicative ((<*>), (<|>), optional, pure)
import Control.Monad ((>>=))
import Data.Bool (Bool(False, True), otherwise)
import Data.Char (Char)
import qualified Data.Char as Char (toLower)
import Data.Eq ((==))
import Data.Foldable (any, null, traverse_)
import Data.Function (($), (.), on)
import Data.Functor (Functor, (<$>), (<&>), fmap)
import qualified Data.List as List
    ( deleteBy
    , elem
    , filter
    , intercalate
    , isPrefixOf
    , nub
    , take
    )
import qualified Data.List.NonEmpty as NonEmpty (toList)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, listToMaybe)
import Data.Monoid (Endo(Endo), mempty)
import Data.Semigroup ((<>))
import Data.String (String, fromString)
import Data.Tuple (fst)
import Data.Word (Word)
import GHC.Generics (Generic)
import System.Environment (getEnvironment)
import System.IO (IO, putStrLn, stderr, stdout)
import Text.Show (Show, show)

import Data.Monoid.Endo.Fold (foldEndo)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc, Pretty(pretty), (<+>))
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty
    ( AnsiStyle
    , Color(Green, Magenta, White)
    , color
    , colorDull
    )
import qualified Data.Text.Prettyprint.Doc.Util as Pretty (reflow)
import qualified Mainplate (applySimpleDefaults)
import qualified Options.Applicative as Options
    ( Parser
    , defaultPrefs
    , flag
    , flag'
    , help
    , info
    , long
    , metavar
    , short
    , strArgument
    )
import Safe (atMay, headMay, lastMay)
import System.Directory (findExecutablesInDirectories)
import System.Posix.Process (executeFile)

import CommandWrapper.Config.Global (Config(..), getAliases)
import CommandWrapper.Environment (AppNames(AppNames, usedName, names))
import qualified CommandWrapper.External as External
    ( executeCommand
    , findSubcommands
    , getSearchPath
    )
import CommandWrapper.Internal.Utils (runMain)
import CommandWrapper.Message
    ( Result(..)
    , debugMsg
    , defaultLayoutOptions
    , hPutDoc
    , out
    )
import CommandWrapper.Options.Alias
    ( Alias(Alias, alias, description)
    , applyAlias
    )
import qualified CommandWrapper.Options.Optparse as Options
    ( internalSubcommandParse
    )
import qualified CommandWrapper.Options.Shell as Options (Shell)


helpOptions :: Pretty.Doc (Result Pretty.AnsiStyle)
helpOptions = Pretty.braces (longOption "help" <> "|" <> shortOption 'h')

subcommand :: Pretty.Doc (Result Pretty.AnsiStyle)
subcommand = metavar "SUBCOMMAND"

optionalSubcommand :: Pretty.Doc (Result Pretty.AnsiStyle)
optionalSubcommand = Pretty.brackets subcommand

globalOptions :: Pretty.Doc (Result Pretty.AnsiStyle)
globalOptions = Pretty.brackets (Pretty.annotate dullGreen "GLOBAL_OPTIONS")

dullGreen, magenta :: Result Pretty.AnsiStyle
dullGreen = Result (Pretty.colorDull Pretty.Green)
magenta = Result (Pretty.color Pretty.Magenta)

usageSection
    :: Pretty command
    => command
    -> [Pretty.Doc (Result Pretty.AnsiStyle)]
    -> Pretty.Doc (Result Pretty.AnsiStyle)
usageSection commandName ds =
    section "Usage:"
        $   ( ds <&> \rest ->
                pretty commandName <+> globalOptions <+> rest
            )
        <>  [""]

globalOptionsHelp
    :: Pretty toolset
    => toolset
    -> Pretty.Doc (Result Pretty.AnsiStyle)
globalOptionsHelp toolset =
    optionDescription ["GLOBAL_OPTIONS"]
        [ Pretty.reflow "See output of" <++> Pretty.squotes callHelp <> "."
        ]
  where
    callHelp = toolsetCommand toolset "help"

section :: Pretty.Doc ann -> [Pretty.Doc ann] -> Pretty.Doc ann
section d ds = Pretty.nest 2 $ Pretty.vsep (d : "" : ds)

optionDescription
    :: [Text]
    -> [Pretty.Doc (Result Pretty.AnsiStyle)]
    -> Pretty.Doc (Result Pretty.AnsiStyle)
optionDescription opts ds = Pretty.vsep
    [ prettyOpts opts
    , Pretty.indent 4 $ Pretty.fillSep ds
    , ""
    ]
  where
    prettyOpts =
        Pretty.hsep
        . Pretty.punctuate Pretty.comma
        . fmap (Pretty.annotate dullGreen . pretty)

shortOption :: Char -> Pretty.Doc (Result Pretty.AnsiStyle)
shortOption o = Pretty.annotate dullGreen ("-" <> pretty o)

longOption :: Text -> Pretty.Doc (Result Pretty.AnsiStyle)
longOption o = Pretty.annotate dullGreen ("--" <> pretty o)

longOptionWithArgument :: Text -> Text -> Pretty.Doc (Result Pretty.AnsiStyle)
longOptionWithArgument o a = longOption o <> "=" <> metavar a

metavar :: Text -> Pretty.Doc (Result Pretty.AnsiStyle)
metavar = Pretty.annotate dullGreen . pretty

value :: Text -> Pretty.Doc (Result Pretty.AnsiStyle)
value = Pretty.annotate dullGreen . pretty

optionalMetavar :: Text -> Pretty.Doc (Result Pretty.AnsiStyle)
optionalMetavar = Pretty.brackets . metavar

command
    :: Pretty.Doc (Result Pretty.AnsiStyle)
    -> Pretty.Doc (Result Pretty.AnsiStyle)
command = Pretty.annotate magenta

toolsetCommand
    :: Pretty toolset
    => toolset
    -> Pretty.Doc (Result Pretty.AnsiStyle)
    -> Pretty.Doc (Result Pretty.AnsiStyle)
toolsetCommand toolset doc = Pretty.annotate magenta (pretty toolset <+> doc)

(<++>) :: Pretty.Doc ann -> Pretty.Doc ann -> Pretty.Doc ann
x <++> y = x <> Pretty.softline <> y
