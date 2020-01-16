{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.Core.Help.Message
-- Description: Experiment to make help messages easier to create.
-- Copyright:   (c) 2019-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Experiment to make help messages easier to create.  Highly experimental!
module CommandWrapper.Core.Help.Message
    ( Annotation(..)
    , Annotated(..)
    , HelpMessage(..)
    , Paragraph
    , Usage

    -- * Combinators
    , longOption
    , shortOption
    , metavar
    , command
    , value

    , (<+>)

    , alternatives
    , optionalAlternatives
    , requiredAlternatives
    , braces
    , brackets
    , squotes
    , dquotes

    , reflow

    , subcommand
    , optionalSubcommand
    , optionalMetavar
    , helpOptions

    -- * Rendering
    , renderAnnotated
    , renderParagraph
    , render
    , defaultStyle

    -- ** IO
    , hPutHelp

    , helpMessage -- TODO: TMP
    )
  where

import Data.Bool (otherwise)
import Data.Char (Char)
import Data.Eq (Eq, (==))
import Data.Foldable (foldMap)
import Data.Function (($), (.))
import Data.Functor (Functor, (<$>), fmap)
import Data.Int (Int)
import qualified Data.List as List (intercalate, intersperse)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Monoid (mconcat, mempty)
import Data.Semigroup (Semigroup, (<>))
import Data.String (IsString(fromString))
import GHC.Generics (Generic)
import System.IO (Handle, IO)
import Text.Show (Show)

import Data.Text (Text)
import qualified Data.Text as Text (null, uncons, unsnoc, words)
import Data.Text.Prettyprint.Doc (Doc, Pretty(pretty))
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty
import qualified System.Console.Terminal.Size as Terminal (Window)

import CommandWrapper.Core.Config.ColourOutput (ColourOutput)
import CommandWrapper.Core.Message (hPutDoc)


data Annotation
    = Plain
    | Option
    | Metavar
    | Value
    | Command
  deriving stock (Eq, Generic, Show)

-- TODO: At the moment @Annotated Plain \"\"@ and @Annotated Plain \" \"@ have
-- a special meaning.  Better encoding would be desirable, however, that will
-- complicate the implementation at the moment.
data Annotated a = Annotated
    { annotation :: Annotation
    , content :: a
    }
  deriving stock (Eq, Functor, Generic, Show)

-- | @\\s -> 'Annotated'{'annotation' = 'Plain', 'content' = 'fromString' s}@
instance IsString s => IsString (Annotated s) where
    fromString = Annotated Plain . fromString

type Paragraph a = [Annotated a]

type Usage a = [Annotated a]

data Section a = Section
    { name :: Paragraph a
    , content :: [Paragraph a]
    }
  deriving stock (Eq, Functor, Generic, Show)

data HelpMessage a = HelpMessage
    { description :: Paragraph a
    , usage :: [Usage a]
    , sections :: [Section a]
    , message :: [Paragraph a]
    }
  deriving stock (Eq, Functor, Generic, Show)

-- {{{ Combinators ------------------------------------------------------------

-- |
-- >>> longOption "foo" :: Annotated Text
-- Annotated{annotation = Option, content = "--foo"}
longOption :: (IsString s, Semigroup s) => s -> Annotated s
longOption content = Annotated
    { annotation = Option
    , content = "--" <> content
    }

-- |
-- >>> shortOption 'f' :: Annotated Text
-- Annotated{annotation = Option, content = "-f"}
shortOption :: IsString s => Char -> Annotated s
shortOption c = Annotated
    { annotation = Option
    , content = fromString ('-' : [c])
    }

metavar :: s -> Annotated s
metavar = Annotated Metavar

command :: s -> Annotated s
command = Annotated Command

value :: s -> Annotated s
value = Annotated Command

(<+>) :: IsString s => [Annotated s] -> [Annotated s] -> [Annotated s]
l <+> r = l <> [""] <> r

brackets :: IsString s => [Annotated s] -> [Annotated s]
brackets c = "[" : c <> ["]"]

braces :: IsString s => [Annotated s] -> [Annotated s]
braces c = "{" : c <> ["}"]

squotes :: IsString s => [Annotated s] -> [Annotated s]
squotes c = "'" : c <> ["'"]

dquotes :: IsString s => [Annotated s] -> [Annotated s]
dquotes c = "\"" : c <> ["\""]

alternatives :: IsString s => [[Annotated s]] -> [Annotated s]
alternatives = List.intercalate ["|"]

optionalAlternatives :: IsString s => [[Annotated s]] -> [Annotated s]
optionalAlternatives = brackets . alternatives

requiredAlternatives :: IsString s => [[Annotated s]] -> [Annotated s]
requiredAlternatives = braces . alternatives

reflow :: Paragraph Text -> Paragraph Text
reflow = foldMap words
  where
    words :: Annotated Text -> [Annotated Text]
    words a@Annotated{..} = case annotation of
        Plain
          | Text.null content -> [a]
          | content == " " -> [a]
          | otherwise -> mconcat
                [ preserveSpaceAtTheBeginning content
                , Annotated Plain <$> List.intersperse "" (Text.words content)
                , preserveSpaceAtTheEnd content
                ]

        Option -> [a]
        Metavar -> [a]
        Value -> [a]
        Command -> [a]
      where
        preserveSpaceAtTheBeginning t = case Text.uncons t of
            Nothing -> []
            Just (' ', _) -> [Annotated Plain ""]
            _ -> []

        preserveSpaceAtTheEnd t = case Text.unsnoc t of
            Nothing -> []
            Just (_, ' ') -> [Annotated Plain ""]
            _ -> []

-- | @['metavar' \"SUBCOMMAND\"]@
subcommand :: IsString s => [Annotated s]
subcommand = [metavar "SUBCOMMAND"]

-- | @'metavar' \"SUBCOMMAND\"@
optionalSubcommand :: IsString s => [Annotated s]
optionalSubcommand = brackets subcommand

optionalMetavar :: IsString s => s -> [Annotated s]
optionalMetavar s = brackets [metavar s]

helpOptions :: (IsString s, Semigroup s) => [Annotated s]
helpOptions = alternatives
    [ [longOption "help"]
    , [shortOption 'h']
    ]

-- }}} Combinators ------------------------------------------------------------
-- {{{ Rendering --------------------------------------------------------------

renderAnnotated :: (Eq a, IsString a, Pretty a) => Annotated a -> Doc Annotation
renderAnnotated Annotated{..} = case annotation of
    Plain
      | content == "" ->
            Pretty.softline

      | content == " " ->
            " "

      | otherwise ->
            Pretty.annotate annotation (pretty content)

    _ ->
        Pretty.annotate annotation (pretty content)

renderParagraph :: Paragraph Text -> Doc Annotation
renderParagraph = foldMap renderAnnotated . reflow

render :: HelpMessage Text -> Doc Annotation
render HelpMessage{..} = Pretty.vsep
    [ renderParagraph description
    , ""
    , renderSection "Usage:" (usage)
    , Pretty.vsep (renderSections sections)
    , Pretty.vsep (renderParagraph <$> message)
    ]
  where
    renderSection :: Doc Annotation -> [Paragraph Text] -> Doc Annotation
    renderSection d ds =
        Pretty.nest 2 $ Pretty.vsep $ d : "" : (fmap renderParagraph ds <> [""])

    renderSections :: [Section Text] -> [Doc Annotation]
    renderSections = fmap \Section{..} ->
        renderSection (renderParagraph name) content

defaultStyle :: Annotation -> Pretty.AnsiStyle
defaultStyle = \case
    Plain -> mempty
    Option -> Pretty.colorDull Pretty.Green
    Metavar -> Pretty.colorDull Pretty.Green
    Value -> Pretty.colorDull Pretty.Green
    Command -> Pretty.color Pretty.Magenta

-- {{{ I/O --------------------------------------------------------------------

hPutHelp
    :: (Maybe (Terminal.Window Int) -> Pretty.LayoutOptions)
    -> (Annotation -> Pretty.AnsiStyle)
    -> ColourOutput
    -> Handle
    -> HelpMessage Text
    -> IO ()
hPutHelp mkLayoutOptions style colourOutput h =
    hPutDoc mkLayoutOptions style colourOutput h . render

-- }}} I/O --------------------------------------------------------------------
-- }}} Rendering --------------------------------------------------------------

helpMessage :: Text -> Maybe Text -> HelpMessage Text
helpMessage usedName description = HelpMessage
    { description = [maybe defaultDescription (Annotated Plain) description]
    , usage =
        [ [Annotated Plain usedName] <+> subcommand <+> subcommandArguments

        , [Annotated Plain usedName] <+> ["help"]
            <+> optionalMetavar "HELP_OPTIONS"
            <+> optionalSubcommand

        , [Annotated Plain usedName] <+> ["config"]
            <+> optionalMetavar "CONFIG_OPTIONS"
            <+> optionalSubcommand

        , [Annotated Plain usedName] <+> ["version"]
            <+> optionalMetavar "VERSION_OPTIONS"

        , [Annotated Plain usedName] <+> ["completion"]
            <+> optionalMetavar "COMPLETION_OPTIONS"

        , [Annotated Plain usedName] <+> requiredAlternatives
            [ [longOption "version"]
            , [shortOption 'V']
            ]

        , [Annotated Plain usedName] <+> braces helpOptions
        ]
    , sections = []
    , message = []
    }

--  , Help.section (Pretty.annotate Help.dullGreen "GLOBAL_OPTIONS" <> ":")
--      [ Help.optionDescription ["-v"]
--          [ Pretty.reflow "Increment verbosity by one level. Can be used"
--          , Pretty.reflow "multiple times."
--          ]

--      , Help.optionDescription ["--verbosity=VERBOSITY"]
--          [ Pretty.reflow "Set verbosity level to"
--          , Help.metavar "VERBOSITY" <> "."
--          , Pretty.reflow "Possible values of"
--          , Help.metavar "VERBOSITY", "are"
--          , Pretty.squotes (Help.value "silent") <> ","
--          , Pretty.squotes (Help.value "normal") <> ","
--          , Pretty.squotes (Help.value "verbose") <> ","
--          , "and"
--          , Pretty.squotes (Help.value "annoying") <> "."
--          ]

--      , Help.optionDescription ["--silent", "-s"]
--          (silentDescription "quiet")

--      , Help.optionDescription ["--quiet", "-q"]
--          (silentDescription "silent")

--      , Help.optionDescription ["--colo[u]r=WHEN"]
--          [ "Set", Help.metavar "WHEN"
--          , Pretty.reflow "colourised output should be produced. Possible"
--          , Pretty.reflow "values of"
--          , Help.metavar "WHEN", "are"
--          , Pretty.squotes (Help.value "always") <> ","
--          , Pretty.squotes (Help.value "auto") <> ","
--          , "and", Pretty.squotes (Help.value "never") <> "."
--          ]

--      , Help.optionDescription ["--no-colo[u]r"]
--          [ Pretty.reflow "Same as"
--          , Pretty.squotes (Help.longOption "colour=never") <> "."
--          ]

--      , Help.optionDescription ["--[no-]aliases"]
--          [ "Apply or ignore", Help.metavar "SUBCOMMAND", "aliases."
--          , Pretty.reflow  "This is useful when used from e.g. scripts to\
--              \ avoid issues with user defined aliases interfering with how\
--              \ the script behaves."
--          ]

--      , Help.optionDescription ["--version", "-V"]
--          [ Pretty.reflow "Print version information to stdout and exit."
--          ]

--      , Help.optionDescription ["--help", "-h"]
--          [ Pretty.reflow "Print this help and exit."
--          ]
--      ]

--  , Help.section (Help.metavar "SUBCOMMAND" <> ":")
--      [ Pretty.reflow "Name of either internal or external subcommand."
--      ]
--  , ""
--  ]
  where
--  silentDescription altOpt =
--      [ "Silent mode. Suppress normal diagnostic or result output. Same as "
--      , Pretty.squotes (Help.longOption altOpt) <> ",", "and"
--      , Pretty.squotes (Help.longOption "verbosity=silent") <> "."
--      ]

    defaultDescription :: Annotated Text
    defaultDescription = "Toolset of commands for working developer."

    subcommandArguments =
        brackets [metavar "SUBCOMMAND_ARGUMENTS"]
