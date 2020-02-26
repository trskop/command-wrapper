-- |
-- Module:      $Header$
-- Description: Generate Dhall configuration file with XDG Directories.
-- Copyright:   (c) 2018-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Generate Dhall configuration file with XDG Directories.
module CommandWrapper.Internal.Subcommand.Config.UserConfig
    ( Options(..)
    , Output(..)
    , dhall
    )
  where

import Control.Exception (Exception, throwIO)
import Data.Foldable (asum, traverse_)
import Data.Functor ((<&>))
import Data.String (fromString)
import Data.Maybe (fromMaybe)
import Data.Traversable (for)
import Data.Void (Void)
import Numeric.Natural (Natural)
import System.IO (stdout)

import Control.Monad.State.Strict (evalStateT)
import Control.Monad.Except (liftEither, runExcept, withExcept)
import Data.Either.Validation (validationToEither)
import Data.Text (Text)
import qualified Data.Text.IO as Text (putStrLn, readFile)
import qualified Dhall
import qualified Dhall.Core as Dhall
    ( Binding
        ( Binding
        , annotation
        , bindingSrc0
        , bindingSrc1
        , bindingSrc2
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
import qualified Dhall.TypeCheck as Dhall (TypeError, typeOf)
import System.FilePath (takeDirectory)
import System.Exit (die)

import CommandWrapper.Core.Config.ColourOutput (ColourOutput)
import CommandWrapper.Core.Dhall as Dhall (hPutExpr)
import CommandWrapper.Internal.Subcommand.Config.Paths (Paths)

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
    -> Dhall.Expr Dhall.Src Void
    -> Maybe (Dhall.Expr Dhall.Src Void)
    ->  ( (Dhall.Expr Dhall.Src Void, Dhall.Expr Dhall.Src Void)
        -> Dhall.Expr Dhall.Src Void
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
    -> Dhall.Expr Dhall.Src Void
    -> Maybe (Dhall.Expr Dhall.Src Void)
    -> IO ()
printPlain paths expression possiblyConfigExpression =
    either dieWithNotPlainType id result
  where
    result :: Either [Dhall.TypeError Dhall.Src Void] (IO ())
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

    extractText = validationToEither . Dhall.extract (Dhall.auto @Text)

    extractMaybeText =
        validationToEither . Dhall.extract (Dhall.auto @(Maybe Text))

    extractNatural =
        validationToEither . Dhall.extract (Dhall.auto @Natural)

    extractInteger =
        validationToEither . Dhall.extract (Dhall.auto @Integer)

    extractTextList =
        validationToEither . Dhall.extract (Dhall.auto @[Text])

    extractNaturalList =
        validationToEither . Dhall.extract (Dhall.auto @[Natural])

    extractIntegerList =
        validationToEither . Dhall.extract (Dhall.auto @[Integer])

    printText, printOptionalText, printNatural, printInteger
        :: Dhall.Expr Dhall.Src Void
        -> IO ()

    printText =
        either (die . show) Text.putStrLn . extractText

    printOptionalText =
        either (die . show) (traverse_ Text.putStrLn) . extractMaybeText

    printNatural =
        either (die . show) (Text.putStrLn . showing) . extractNatural

    printInteger =
        either (die . show) (Text.putStrLn . showing) . extractInteger

    printTextList =
        either (die . show) (mapM_ Text.putStrLn) . extractTextList

    printNaturalList =
        either (die . show) (mapM_ $ Text.putStrLn . showing)
        . extractNaturalList

    printIntegerList =
        either (die . show) (mapM_ $ Text.putStrLn . showing)
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
    -> IO (Dhall.Expr Dhall.Src Void)
parseDhallExpression possiblySourcePath =
    either throwIO resolveImports . Dhall.exprFromText sourcePath
  where
    sourcePath = fromMaybe "(command-line)" possiblySourcePath
    sourceDir = maybe "." takeDirectory possiblySourcePath

    resolveImports
        :: Dhall.Expr Dhall.Src Dhall.Import
        -> IO (Dhall.Expr Dhall.Src Void)
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
    :: Maybe (Dhall.Expr Dhall.Src Void)
    -- ^ Expected type of the result.
    -> Dhall.Expr Dhall.Src Void
    -- ^ Expression from command line.
    -> Dhall.Expr Dhall.Src Void
    -- ^ Parsed configuration file.
    -> Paths
    -> Either
        (Dhall.TypeError Dhall.Src Void)
        (Dhall.Expr Dhall.Src Void, Dhall.Expr Dhall.Src Void)
mkExpressionAndTypeCheck possiblyResultType bodyExpression configExpression paths = do
    expression <- mkExpression <$> Dhall.typeOf configExpression
    Dhall.typeOf expression <&> (Dhall.normalize expression, )
  where
    Dhall.Encoder{..} = Dhall.inject @Paths

    bodyExpression' =
        maybe bodyExpression (Dhall.Annot bodyExpression) possiblyResultType

    mkExpression configExpressionType =
        Dhall.Let
            ( Dhall.Binding
                { Dhall.variable = "Paths"
                , Dhall.annotation = Nothing
                , Dhall.value = declared
                , Dhall.bindingSrc0 = Nothing
                , Dhall.bindingSrc1 = Nothing
                , Dhall.bindingSrc2 = Nothing
                }
            )
            ( Dhall.Let
                ( Dhall.Binding
                    { Dhall.variable = "Config"
                    , Dhall.annotation = Nothing
                    , Dhall.value = configExpressionType
                    , Dhall.bindingSrc0 = Nothing
                    , Dhall.bindingSrc1 = Nothing
                    , Dhall.bindingSrc2 = Nothing
                    }
                )
                ( Dhall.Let
                    ( Dhall.Binding
                        { Dhall.variable = "config"
                        , Dhall.annotation = Just (Nothing, configExpressionType)
                        , Dhall.value = configExpression
                        , Dhall.bindingSrc0 = Nothing
                        , Dhall.bindingSrc1 = Nothing
                        , Dhall.bindingSrc2 = Nothing
                        }
                    )
                    ( Dhall.Let
                        ( Dhall.Binding
                            { Dhall.variable = "paths"
                            , Dhall.annotation = Just (Nothing, declared)
                            , Dhall.value = embed paths
                            , Dhall.bindingSrc0 = Nothing
                            , Dhall.bindingSrc1 = Nothing
                            , Dhall.bindingSrc2 = Nothing
                            }
                        )
                        ( Dhall.Let
                            ( Dhall.Binding
                                { Dhall.variable = "data"
                                , Dhall.annotation = Just
                                    ( Nothing
                                    , recordType
                                        [ ("paths", declared)
                                        , ("config", configExpressionType)
                                        ]
                                    )
                                , Dhall.value = record
                                    [ ("paths", embed paths)
                                    , ("config", configExpression)
                                    ]
                                , Dhall.bindingSrc0 = Nothing
                                , Dhall.bindingSrc1 = Nothing
                                , Dhall.bindingSrc2 = Nothing
                                }
                            )
                            bodyExpression'
                        )
                    )
                )
            )

throwLeft :: Exception e => Either e r -> IO r
throwLeft = either throwIO pure

emptyRecord :: Dhall.Expr s a
emptyRecord = Dhall.RecordLit mempty

record :: [(Text, Dhall.Expr s a)] -> Dhall.Expr s a
record = Dhall.RecordLit . Dhall.Map.fromList

recordType :: [(Text, Dhall.Expr s a)] -> Dhall.Expr s a
recordType = Dhall.Record . Dhall.Map.fromList
