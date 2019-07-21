{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
-- |
-- Module:      CommandWrapper.Internal.Subcommand.Completion.Compgen
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module CommandWrapper.Internal.Subcommand.Completion.Compgen
--  (
--  )
  where

import Prelude hiding (filter, words)

import Control.Exception (throwIO)
import GHC.Generics (Generic)
import qualified GHC.IO.Encoding as IO (setLocaleEncoding)
import Numeric.Natural (Natural)
import qualified System.IO as IO (utf8)

import qualified Control.Monad.Trans.State.Strict as State
import Data.Monoid.Endo (mapEndo)
import Data.Text (Text)
import qualified Data.Text.IO as Text (getContents, putStrLn, readFile)
import Data.Generics.Product.Fields (field')
import Data.Output
    ( HasOutput
--  , OutputFile(OutputFile)
--  , OutputHandle(OutputHandle, OutputNotHandle)
    , OutputStdoutOrFile
    , pattern OutputStdoutOnly
    )
import qualified Data.Output (HasOutput(..))
import qualified Dhall
import qualified Dhall.Import
import qualified Dhall.Core as Dhall (Expr(..), Import, normalize, throws)
import Dhall.Parser (Src)
import qualified Dhall.Parser (exprFromText)
import Dhall.TypeCheck (X)

import CommandWrapper.Config.Global (Config)
import CommandWrapper.Environment (AppNames)
import qualified CommandWrapper.Internal.Subcommand.Config.Dhall as Config.Dhall
import qualified CommandWrapper.Options.Shell as Options


data Input
    = InputStdin
    | InputFile FilePath
    | InputExpression Text
  deriving stock (Generic, Show)

data CompgenOptions = CompgenOptions
    { index :: Natural
    , words :: [Text]
    , shell :: Options.Shell
    , allowImports :: Bool
    , input :: Input
    , output :: OutputStdoutOrFile
    }
  deriving stock (Generic, Show)

instance HasOutput CompgenOptions where
    type Output CompgenOptions = OutputStdoutOrFile
    output = field' @"output"

instance Options.HasShell CompgenOptions where
    updateShell = mapEndo \f opts@CompgenOptions{shell} ->
        (opts :: CompgenOptions){shell = f shell}

defCompgenOptions :: CompgenOptions
defCompgenOptions = CompgenOptions
    { index = 0
    , words = []
    , shell = Options.Bash
    , allowImports = True
    , input = InputStdin
    , output = OutputStdoutOnly
    }

data Action
    = Directory -- TODO: Options
    -- ^ Complete directory name.

    | File -- TODO: Options
    -- ^ Complete file name.

    | FromList -- TODO: Options
    -- ^ Complete using a predefined list of completions.

    | Exec -- TODO: Options
    -- ^ Execute a command for completion.

    | None
    -- ^ Do nothing.  We may want to reconsider this and have @Optional ACtion@
    -- instead.
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.Interpret) -- TODO: Will need custom instance or combinator.

compgen :: AppNames -> Config -> CompgenOptions -> IO ()
compgen appNames config CompgenOptions{..} =
    Config.Dhall.handleExceptions appNames config do
        IO.setLocaleEncoding IO.utf8
        (expressionWithImports, status) <- readExpression input

        stepExpression <- if allowImports
            then
                State.evalStateT (Dhall.Import.loadWith expressionWithImports)
                    status
            else
                Dhall.Import.assertNoImports expressionWithImports

        -- TODO: Type check 'stepExpression'.
        expression <- buildExpression stepExpression index words
        {- TODO: Type check 'expression'.
        _ <- Dhall.Core.throws (Dhall.TypeCheck.typeWith (view startingContext settings) annot)
        -}
        let annotatedExpression = Dhall.Annot expression expected
        case extract (Dhall.normalize annotatedExpression) of
            Just action ->
                runAction appNames config index words action
                    >>= mapM_ Text.putStrLn . filter

            Nothing ->
                throwIO (Dhall.InvalidType expected annotatedExpression)

  where
    Dhall.Type{expected, extract} = Dhall.auto @Action

-- TODO:
-- * The resulting expression should be of type @List Text -> Action@
buildExpression
    :: Dhall.Expr Src X
    -> Natural
    -> [Text]
    -> IO (Dhall.Expr Src X)
buildExpression e _ _ = pure e -- TODO Implement.

runAction :: AppNames -> Config -> Natural -> [Text] -> Action -> IO [Text]
runAction _ _ _ _ = \case
    Directory -> pure []
    File -> pure []
    FromList -> pure []
    Exec -> pure []
    None -> pure []

filter :: {- MatchingAlgorithm -> -} [Text] -> [Text]
filter x = x -- TODO

readExpression
    :: Input
    -> IO (Dhall.Expr Src Dhall.Import, Dhall.Import.Status IO)
readExpression = \case
    InputStdin ->
        Text.getContents >>= parseExpr "(stdin)" "."

    InputFile file ->
        Text.readFile file >>= parseExpr file file

    InputExpression expr ->
        parseExpr "(expression)" "." expr
  where
    parseExpr f c txt =
        (,) <$> Dhall.throws (Dhall.Parser.exprFromText f txt)
            <*> pure (Dhall.Import.emptyStatus c)
