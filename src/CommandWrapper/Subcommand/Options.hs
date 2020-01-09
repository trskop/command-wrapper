{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module:      CommandWrapper.Subcommand.Options
-- Description: Command line option parser for Command Wrapper subcommand.
-- Copyright:   (c) 2018-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Command line option parser for Command Wrapper subcommand.
module CommandWrapper.Subcommand.Options
    (
    -- * Parse Subcommand Options
      SubcommandProps(..)
    , noPreprocessing
    , runSubcommand

    -- ** Help
    , helpFlag
    , helpFlagFields

    -- ** Completion
    , completionInfoFlag
    , completionInfoFlagFields
    , completionInfoHashFlag
    , completionInfoHashFlagFields
    , completionOptions
    , printCommandWrapperStyleCompletionInfoExpression
    , printCommandWrapperStyleCompletionInfoExpressionHash
    , printOptparseCompletionInfoExpression
    , printOptparseCompletionInfoExpressionHash

    -- * Internals
    , Mode(..)
    )
  where

import Control.Applicative ((<*>), many)
import Data.Foldable (asum)
import Data.Function (($), (.), const)
import Data.Functor ((<$>), fmap)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Monoid (Endo(Endo, appEndo), Monoid, mempty)
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Void (Void)
import Data.Word (Word)
import System.IO (Handle, IO, stdout)

import qualified Data.CaseInsensitive as CI (mk)
import Data.Text (Text)
import qualified Data.Text.IO as Text (hPutStr)
import qualified Dhall.Core as Dhall (Expr, denote)
import qualified Dhall.Import as Dhall (hashExpressionToCode)
import qualified Dhall.Parser as Dhall (Src)
import qualified Dhall.Pretty as Dhall (CharacterSet(Unicode))
import qualified Dhall.TH (staticDhallExpression)
import Data.Generics.Product.Typed (HasType, getTyped)
import Data.Monoid.Endo (mapEndo)
import Data.Monoid.Endo.Fold (dualFoldEndo)
import qualified Data.Text.Prettyprint.Doc as Pretty (Doc)
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty (AnsiStyle)
import qualified Options.Applicative as Options
    ( HasName
    , Mod
    , Parser
    , ParserInfo
    , auto
    , defaultPrefs
    , flag
    , flag'
    , strArgument
    , info
    , internal
    , long
    , maybeReader
    , metavar
    , option
    , short
    )

import CommandWrapper.Environment (Params(Params, colour, verbosity))
import qualified CommandWrapper.Internal.Dhall as Dhall (hPutExpr)
import CommandWrapper.Message (Result, defaultLayoutOptions, outWith)
import CommandWrapper.Options.ColourOutput (ColourOutput(Never))
import CommandWrapper.Options.Optparse (subcommandParse)
import qualified CommandWrapper.Options.Shell as Options (Shell)
import qualified CommandWrapper.Options.Shell as Options.Shell (parse)


data Mode action
    = Action action
    | CompletionInfo
    | CompletionInfoHash
    | Completion Word Options.Shell [String]
    | Help

-- | Parameters of 'runSubcommand' function.
data SubcommandProps params action = SubcommandProps
    { preprocess :: params -> [String] -> (params, [String])
    -- ^ Pre-process inputs before doing anything.  This is useful to e.g.
    -- split\/filter arguments.
    , doCompletion :: params -> Word -> Options.Shell -> [String] -> IO ()
    -- ^ Perform command line completion using this action.
    , helpMsg :: params -> Pretty.Doc (Result Pretty.AnsiStyle)
    -- ^ Help message to be printed.
    , actionOptions :: Options.Parser (Endo (Maybe action))
    -- ^ Options parser.
    , defaultAction :: Maybe action
    , params :: params
    -- ^ Subcommand parameters, usually just parsed environment variables.  It
    -- has to contain a field of type 'Params'.  Those are values passed by
    -- Command Wrapper via Subcommand Protocol.
    , arguments :: [String]
    -- ^ Subcommand command line arguments.
    }

-- | Identity to use when there is no need to do anything as part of
-- 'preprocess' stage.
noPreprocessing :: params -> [String] -> (params, [String])
noPreprocessing = (,)

-- | Parse subcommand options and run subcommand main action.
runSubcommand
    :: forall params action
    .  HasType Params params
    => SubcommandProps params action
    -> (params -> action -> IO ())
    -> IO ()
runSubcommand SubcommandProps{..} doAction = do
    let (params', arguments') = preprocess params arguments
        protoParams = getTyped @Params params'

    updateMode
        <- subcommandParse protoParams Options.defaultPrefs info arguments'
    case updateMode `appEndo` maybe Help Action defaultAction of
        Action action ->
            doAction params' action

        CompletionInfo ->
            printCommandWrapperStyleCompletionInfoExpression stdout

        CompletionInfoHash ->
            printCommandWrapperStyleCompletionInfoExpressionHash stdout

        Completion index shell words ->
            doCompletion params' index shell words

        Help -> do
            let Params{colour, verbosity} = protoParams
            outWith defaultLayoutOptions verbosity colour stdout
                (helpMsg params')
  where
    info :: Options.ParserInfo (Endo (Mode action))
    info = (`Options.info` mempty) $ asum
        [ dualFoldEndo
            <$> helpFlag (constEndo Help)
            <*> fmap (mapEndo mapActionMode) actionOptions
        , completionOptions (\i s as -> constEndo (Completion i s as))
        , completionInfoFlag (constEndo CompletionInfo)
        , completionInfoHashFlag (constEndo CompletionInfoHash)
        ]

    constEndo :: a -> Endo a
    constEndo a = Endo (const a)

    mapActionMode
        :: (Maybe action -> Maybe action)
        -> Mode action -> Mode action
    mapActionMode f = \case
        Action action ->
            maybe Help Action (f (Just action))
        mode ->
            maybe mode Action (f Nothing)

-- {{{ Help Options -----------------------------------------------------------

helpFlag :: Monoid mode => mode -> Options.Parser mode
helpFlag helpMode = Options.flag mempty helpMode helpFlagFields

helpFlagFields :: Options.HasName f => Options.Mod f a
helpFlagFields = Options.long "help" <> Options.short 'h'

-- }}} Help Options -----------------------------------------------------------

-- {{{ Completion Options -----------------------------------------------------

completionOptions
    :: (Word -> Options.Shell -> [String] -> mode)
    -> Options.Parser mode
completionOptions completionMode =
    Options.flag' completionMode
        (Options.long "completion" <> Options.internal)
    <*> Options.option Options.auto (Options.long "index" <> Options.internal)
    <*> Options.option (Options.maybeReader $ Options.Shell.parse . CI.mk)
            (Options.long "shell" <> Options.internal)
    <*> many (Options.strArgument (Options.metavar "WORD" <> Options.internal))

completionInfoFlag :: mode -> Options.Parser mode
completionInfoFlag completionInfoMode =
    Options.flag' completionInfoMode completionInfoFlagFields

completionInfoFlagFields :: Options.HasName f => Options.Mod f a
completionInfoFlagFields =
    Options.long "completion-info" <> Options.internal

completionInfoHashFlag :: mode -> Options.Parser mode
completionInfoHashFlag completionInfoHashMode =
    Options.flag' completionInfoHashMode completionInfoHashFlagFields

completionInfoHashFlagFields :: Options.HasName f => Options.Mod f a
completionInfoHashFlagFields =
    Options.long "completion-info-hash" <> Options.internal

-- {{{ Optparse-applicative-style  Completion Expression ----------------------

-- | Style of calling a subcommand completion functionality that is used by
-- [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative)
-- package.
--
-- > --bash-completion-index=NUM [--bash-completion-word=WORD ...]
printOptparseCompletionInfoExpression
    :: Handle
    -- ^ Output handle.
    -> IO ()
printOptparseCompletionInfoExpression outHandle =
    let completionInfo :: Dhall.Expr Dhall.Src Void =
            $(Dhall.TH.staticDhallExpression
                "./dhall/optparse-completion-info.dhall"
            )

     in Dhall.hPutExpr Never Dhall.Unicode outHandle completionInfo

printOptparseCompletionInfoExpressionHash :: Handle -> IO ()
printOptparseCompletionInfoExpressionHash h =
    Text.hPutStr h optparseCompletionInfoExpressionHash

optparseCompletionInfoExpression :: Dhall.Expr Dhall.Src Void
optparseCompletionInfoExpression =
    $(Dhall.TH.staticDhallExpression
        "./dhall/optparse-completion-info.dhall"
    )
{-# INLINE optparseCompletionInfoExpression #-}

optparseCompletionInfoExpressionHash :: Text
optparseCompletionInfoExpressionHash =
    Dhall.hashExpressionToCode (Dhall.denote optparseCompletionInfoExpression)
{-# INLINE optparseCompletionInfoExpressionHash #-}

-- }}} Optparse-applicative-style  Completion Expression ----------------------

-- {{{ Command Wrapper-style Completion Expression ----------------------------

-- | Style of calling a subcommand completion functionality that is similar to
-- how Command Wrapper's own @completion@ subcommand works.
--
-- > --completion --index=NUM --shell=SHELL -- [WORD ...]
printCommandWrapperStyleCompletionInfoExpression
    :: Handle
    -- ^ Output handle.
    -> IO ()
printCommandWrapperStyleCompletionInfoExpression outHandle =
    Dhall.hPutExpr Never Dhall.Unicode outHandle
        commandWrapperStyleCompletionInfoExpression

printCommandWrapperStyleCompletionInfoExpressionHash :: Handle -> IO ()
printCommandWrapperStyleCompletionInfoExpressionHash h =
    Text.hPutStr h commandWrapperStyleCompletionInfoExpressionHash

commandWrapperStyleCompletionInfoExpression :: Dhall.Expr Dhall.Src Void
commandWrapperStyleCompletionInfoExpression =
    $(Dhall.TH.staticDhallExpression
        "./dhall/command-wrapper-style-completion-info.dhall"
    )
{-# INLINE commandWrapperStyleCompletionInfoExpression #-}

commandWrapperStyleCompletionInfoExpressionHash :: Text
commandWrapperStyleCompletionInfoExpressionHash = Dhall.hashExpressionToCode
    (Dhall.denote commandWrapperStyleCompletionInfoExpression)
{-# INLINE commandWrapperStyleCompletionInfoExpressionHash #-}

-- }}} Command Wrapper-style Completion Expression ----------------------------
