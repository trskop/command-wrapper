-- |
-- Module:      $Header$
-- Description: Generate Dhall configuration file with XDG Directories.
-- Copyright:   (c) 2018-2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Generate Dhall configuration file with XDG Directories.
module CommandWrapper.Internal.Subcommand.Config.Dhall
    ( Options(..)
    , Output(..)
    , dhall
    )
  where

import Control.Exception (Exception, throwIO)
import Data.Foldable (asum, traverse_)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.String (fromString)
import Data.Maybe (fromMaybe)
import Data.Traversable (for)
import Numeric.Natural (Natural)
import System.IO (stdout)

import Control.Monad.State.Strict (evalStateT)
import Control.Monad.Except (liftEither, runExcept, withExcept)
import Data.Text (Text)
import qualified Data.Text.IO as Text (putStrLn, readFile)
import qualified Dhall
import qualified Dhall.Core as Dhall
    ( Binding
        ( Binding
        , annotation
        , value
        , variable
        )
    , Expr
        ( Annot
        , App
        , Integer
        , Let
        , List
        , Natural
        , Optional
        , Record
        , RecordLit
        , Text
        )
    , Import
    , normalize
    )
import qualified Dhall.Import as Dhall (emptyStatus, loadWith)
import qualified Dhall.Map (fromList)
import qualified Dhall.Parser as Dhall (Src, exprFromText)
import qualified Dhall.Pretty as Dhall (CharacterSet)
import qualified Dhall.TypeCheck as Dhall (TypeError, X, typeOf)
import System.FilePath (takeDirectory)
import System.Exit (die)

import CommandWrapper.Internal.Dhall as Dhall (hPutExpr)
import CommandWrapper.Internal.Subcommand.Config.Paths (Paths)
import CommandWrapper.Options.ColourOutput (ColourOutput)

data Output = Plain | DhallExpression | DhallType

data Options = Options
    { output :: Output
    , colour :: ColourOutput
    , charset :: Dhall.CharacterSet
    }

dhall :: Options -> Paths -> Maybe FilePath -> Text -> IO ()
dhall opts paths possiblyConfigFile expressionText = do
    config <- for possiblyConfigFile Text.readFile
    expression <- parseDhallExpression Nothing expressionText
    configExpression <- for config (parseDhallExpression possiblyConfigFile)
    case output opts of
        Plain ->
            printPlain paths expression configExpression

        DhallExpression ->
            let Options{colour, charset} = opts
            in printDhall colour charset paths expression configExpression fst

        DhallType ->
            let Options{colour, charset} = opts
            in printDhall colour charset paths expression configExpression snd

printDhall
    :: ColourOutput
    -> Dhall.CharacterSet
    -> Paths
    -> Dhall.Expr Dhall.Src Dhall.X
    -> Maybe (Dhall.Expr Dhall.Src Dhall.X)
    ->  ( (Dhall.Expr Dhall.Src Dhall.X, Dhall.Expr Dhall.Src Dhall.X)
        -> Dhall.Expr Dhall.Src Dhall.X
        )
    -> IO ()
printDhall colour charset paths expression possiblyConfigExpression selector =
    throwLeft mkExpressionAndTypeCheck'
        >>= Dhall.hPutExpr colour charset stdout . selector
  where
    mkExpressionAndTypeCheck' =
        mkExpressionAndTypeCheck Nothing expression configExpression paths

    configExpression = fromMaybe emptyRecord possiblyConfigExpression

printPlain
    :: Paths
    -> Dhall.Expr Dhall.Src Dhall.X
    -> Maybe (Dhall.Expr Dhall.Src Dhall.X)
    -> IO ()
printPlain paths expression possiblyConfigExpression =
    either dieWithNotPlainType id result
  where
    result :: Either [Dhall.TypeError Dhall.Src Dhall.X] (IO ())
    result = runExcept $ asum
        [ printText <$> mkExpression Dhall.Text
        , printOptionalText <$> mkExpression (optional Dhall.Text)
        , printNatural <$> mkExpression Dhall.Natural
        , printInteger <$> mkExpression Dhall.Integer
        , printTextList <$> mkExpression (listOf Dhall.Text)
        , printNaturalList <$> mkExpression (listOf Dhall.Natural)
        , printIntegerList <$> mkExpression (listOf Dhall.Integer)
        ]

    listOf = (Dhall.List `Dhall.App`)
    optional = (Dhall.Optional `Dhall.App`)

    configExpression = fromMaybe emptyRecord possiblyConfigExpression

    mkExpression t = withExcept pure . liftEither
        $ fst <$> mkExpressionAndTypeCheck (Just t) expression configExpression paths

    dieWithNotPlainType _ = die
        "Error: Expected Text, Optional Text, Natural, Integer, or List of\
        \ them as result."

    dieWithUnexpectedType t =
        die ("Error: Expected " <> t <> " as result.")

    Dhall.Type{extract = extractText} = Dhall.auto @Text
    Dhall.Type{extract = extractMaybeText} = Dhall.auto @(Maybe Text)
    Dhall.Type{extract = extractNatural} = Dhall.auto @Natural
    Dhall.Type{extract = extractInteger} = Dhall.auto @Integer
    Dhall.Type{extract = extractTextList} = Dhall.auto @[Text]
    Dhall.Type{extract = extractNaturalList} = Dhall.auto @[Natural]
    Dhall.Type{extract = extractIntegerList} = Dhall.auto @[Integer]

    printText, printOptionalText, printNatural, printInteger
        :: Dhall.Expr Dhall.Src Dhall.X
        -> IO ()

    printText =
        maybe (dieWithUnexpectedType "Text") Text.putStrLn . extractText

    printOptionalText =
        maybe (dieWithUnexpectedType "Optional Text") (traverse_ Text.putStrLn)
        . extractMaybeText

    printNatural =
        maybe (dieWithUnexpectedType "Natural") (Text.putStrLn . showing)
        . extractNatural

    printInteger =
        maybe (dieWithUnexpectedType "Integer") (Text.putStrLn . showing)
        . extractInteger

    printTextList =
        maybe (dieWithUnexpectedType "List Text") (mapM_ Text.putStrLn)
        . extractTextList

    printNaturalList =
        maybe (dieWithUnexpectedType "List Natural")
            (mapM_ $ Text.putStrLn . showing)
        . extractNaturalList

    printIntegerList =
        maybe (dieWithUnexpectedType "List Integer")
            (mapM_ $ Text.putStrLn . showing)
        . extractIntegerList

    showing :: Show a => a -> Text
    showing = fromString . show

-- | Parse Dhall expression and resolve imports.
--
-- TODO:
--
-- * Support parsing a file
-- * Allow disabling imports
-- * Pass root directory for imports resolution.
parseDhallExpression
    :: Maybe FilePath
    -> Text
    -> IO (Dhall.Expr Dhall.Src Dhall.X)
parseDhallExpression possiblySourcePath =
    either throwIO resolveImports . Dhall.exprFromText sourcePath
  where
    sourcePath = fromMaybe "(command-line)" possiblySourcePath
    sourceDir = maybe "." takeDirectory possiblySourcePath

    resolveImports
        :: Dhall.Expr Dhall.Src Dhall.Import
        -> IO (Dhall.Expr Dhall.Src Dhall.X)
    resolveImports expr =
        evalStateT (Dhall.loadWith expr) (Dhall.emptyStatus sourceDir)

-- | Takes a Dhall @EXPRESSION@ and returns a following expression:
--
-- @
-- let
--     -- Type of 'Paths' data type so that it can be referred to in
--     -- EXPRESSION.
--     Paths = ...
--
-- in let
--     -- Type of 'Paths' data type so that it can be referred to in
--     -- EXPRESSION.
--     Config = ...
--
-- in
--     ( λ(data : {paths : Paths, config : Config}) → EXPRESSION : TYPE
--     ) data
-- @
mkExpressionAndTypeCheck
    :: Maybe (Dhall.Expr Dhall.Src Dhall.X)
    -- ^ Expected type of the result.
    -> Dhall.Expr Dhall.Src Dhall.X
    -- ^ Expression from command line.
    -> Dhall.Expr Dhall.Src Dhall.X
    -- ^ Parsed configuration file.
    -> Paths
    -> Either
        (Dhall.TypeError Dhall.Src Dhall.X)
        (Dhall.Expr Dhall.Src Dhall.X, Dhall.Expr Dhall.Src Dhall.X)
mkExpressionAndTypeCheck possiblyResultType bodyExpression configExpression paths = do
    expression <- mkExpression <$> Dhall.typeOf configExpression
    Dhall.typeOf expression <&> (Dhall.normalize expression, )
  where
    Dhall.InputType{..} = Dhall.inject @Paths

    bodyExpression' =
        maybe bodyExpression (Dhall.Annot bodyExpression) possiblyResultType

    mkExpression configExpressionType =
        Dhall.Let
            ( Dhall.Binding
                { Dhall.variable = "Paths"
                , Dhall.annotation = Nothing
                , Dhall.value = declared
                }
            :|  [ Dhall.Binding
                    { Dhall.variable = "Config"
                    , Dhall.annotation = Nothing
                    , Dhall.value = configExpressionType
                    }
                , Dhall.Binding
                    { Dhall.variable = "config"
                    , Dhall.annotation = Just configExpressionType
                    , Dhall.value = configExpression
                    }
                , Dhall.Binding
                    { Dhall.variable = "paths"
                    , Dhall.annotation = Just declared
                    , Dhall.value = embed paths
                    }
                , Dhall.Binding
                    { Dhall.variable = "data"
                    , Dhall.annotation = Just
                        ( recordType
                            [ ("paths", declared)
                            , ("config", configExpressionType)
                            ]
                        )
                    , Dhall.value = record
                        [ ("paths", embed paths)
                        , ("config", configExpression)
                        ]
                    }
                ]
            )
            bodyExpression'

throwLeft :: Exception e => Either e r -> IO r
throwLeft = either throwIO pure

emptyRecord :: Dhall.Expr s a
emptyRecord = Dhall.RecordLit mempty

record :: [(Text, Dhall.Expr s a)] -> Dhall.Expr s a
record = Dhall.RecordLit . Dhall.Map.fromList

recordType :: [(Text, Dhall.Expr s a)] -> Dhall.Expr s a
recordType = Dhall.Record . Dhall.Map.fromList