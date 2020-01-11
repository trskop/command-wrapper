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
    , helpFlag'
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
import Data.Functor (Functor, (<$>), fmap)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Monoid (Endo(Endo, appEndo), Monoid, mempty)
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Void (Void)
import Data.Word (Word)
import GHC.Generics (Generic)
import System.IO (Handle, IO, stdout)
import Text.Show (Show)

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


-- | Mode of operation of a subcommand.  It can be either 'Action', which is
-- the functionality of the subcommand or one of the modes expected by the
-- Subcommand Protocol (see @command-wrapper-subcommand-protocol(7)@ manual
-- page for more information).
data Mode action
    = Action action
    -- ^ Subcommand @action@, i.e. the functionality provided by the subcommand
    -- itself.
    | DefaultAction
    -- ^ Invoke a default action based on what subcommand declared as one.
    | CompletionInfo
    -- ^ Mode initiated by @--completion-info@ command line option that is
    -- mandated by Subcommand Protocol.
    --
    -- In this mode we display a Dhall expressions that tells Command Wrapper
    -- how to call command line completion on the subcommand.  This way
    -- subcommand is free to implement completion as it wants.
    | CompletionInfoHash
    -- ^ Mode initiated by @--completion-info-hash@ that prints semantic hash
    -- of Dhall expressions printed by 'CompletionInfo' mode.
    | Completion Word Options.Shell [String]
    -- ^ Command line completion mode initiated when subcommand is called using
    -- convention described by 'CompletionInfo' mode.
    | Help
    -- ^ Print help message mode initiated by @--help|-h@ options.  This mode
    -- is also mandated by Subcommand Protocol.
  deriving stock (Functor, Generic, Show)

-- | Parameters of 'runSubcommand' function.
data SubcommandProps params action = SubcommandProps
    { preprocess :: params -> [String] -> (params, [String])
    -- ^ Pre-process inputs before doing anything.  This is useful to e.g.
    -- split\/filter arguments.
    , doCompletion :: params -> Word -> Options.Shell -> [String] -> IO ()
    -- ^ Perform command line completion using this action.
    , helpMsg :: params -> Pretty.Doc (Result Pretty.AnsiStyle)
    -- ^ Help message to be printed if @--help|-h@ is invoked.
    , actionOptions :: Options.Parser (Endo (Maybe action))
    -- ^ Subcommand options parser.  It will be integrated into bigger parser
    -- that handles options mandated by Subcommand Protocol as well.
    , defaultAction :: Maybe action
    -- ^ Default action to be used when `actionOptions` is either not invoked
    -- or returns `Nothing`.
    --
    -- If this value is set to `Nothing` and `actionOptions` doesn't change it
    -- we'll print `Help` information.  This is usually the case when there
    -- were no options passed to our application.  If you want to provide
    -- custom error message for this then define an action for this purpose and
    -- set it as 'defaultAction'.
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
    case updateMode `appEndo` DefaultAction of
        Action action ->
            doAction params' action

        DefaultAction ->
            case defaultAction of
                Nothing ->
                    doHelpMsg params' protoParams

                Just action ->
                    doAction params' action

        CompletionInfo ->
            printCommandWrapperStyleCompletionInfoExpression stdout

        CompletionInfoHash ->
            printCommandWrapperStyleCompletionInfoExpressionHash stdout

        Completion index shell words ->
            doCompletion params' index shell words

        Help ->
            doHelpMsg params' protoParams
  where
    info :: Options.ParserInfo (Endo (Mode action))
    info = (`Options.info` mempty) $ asum
        [ helpFlag' (constEndo Help)
        , dualFoldEndo
            <$> fmap (mapEndo mapActionMode) actionOptions
            <*> helpFlag (constEndo Help)
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
            maybe DefaultAction Action (f (Just action))

        DefaultAction ->
            maybe DefaultAction Action (f defaultAction)

        mode ->
            mode

    doHelpMsg params' protoParams = do
        let Params{colour, verbosity} = protoParams
        outWith defaultLayoutOptions verbosity colour stdout (helpMsg params')

-- {{{ Help Options -----------------------------------------------------------

helpFlag :: Monoid mode => mode -> Options.Parser mode
helpFlag helpMode = Options.flag mempty helpMode helpFlagFields

helpFlag' :: mode -> Options.Parser mode
helpFlag' helpMode = Options.flag' helpMode helpFlagFields

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
