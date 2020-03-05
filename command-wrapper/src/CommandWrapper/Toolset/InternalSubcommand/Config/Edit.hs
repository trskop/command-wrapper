{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      $Header$
-- Description: Invoke editor.
-- Copyright:   (c) 2019-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- Invoke editor.
module CommandWrapper.Toolset.InternalSubcommand.Config.Edit
    ( EditOptions(..)
    , WhatToEdit(..)
    , defEditOptions
    , edit
    )
  where

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Bool (Bool(False))
import Data.Functor ((<$>), (<&>), fmap)
import Data.Int (Int)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid ((<>))
import Data.String (String, fromString)
import GHC.Generics (Generic)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (FilePath, IO, stderr)
import Text.Show (Show)

import Data.Text.Prettyprint.Doc (pretty)
import qualified Data.Text.Prettyprint.Doc as Pretty (Doc)
import System.Editor
    ( File(..)
    , editor
    , execEditorCommand
    , getEditorCommand
    , simpleEditorCommand
    , stdEditorLookupStrategy
    )

import CommandWrapper.Core.Message (errorMsg)
import CommandWrapper.Core.Environment (AppNames(AppNames, usedName))
import CommandWrapper.Toolset.Config.Global
    ( Config(Config, colourOutput, verbosity)
    )
import CommandWrapper.Toolset.ExternalSubcommand (getSubcommandConfigPathToEdit)


data EditOptions = EditOptions
    { defaultEditor :: String
    , what :: Maybe WhatToEdit
    -- ^ When 'Nothing' then just open editor.
    }
  deriving stock (Generic, Show)

data WhatToEdit
    = EditFile FilePath
    | EditSubcommandConfig String
  deriving stock (Generic, Show)

defEditOptions :: EditOptions
defEditOptions = EditOptions
    { defaultEditor = "vi"
    , what = Nothing
    }

edit :: AppNames -> Config -> EditOptions -> IO ()
edit appNames@AppNames{..} config@Config{..} EditOptions{..} = do
    -- TODO: What should be Command Wrapper's lookup strategy?
    findEditor >>= \case
        Nothing -> dieWith 126 "Unable to find suitable editor."
        Just editorCommand -> do
            file <- getFilePath appNames config what <&> fmap \file ->
                File{file = fromString file, line = 0}

            execEditorCommand (editor editorCommand) file
  where
    findEditor =
        getEditorCommand Nothing
            (stdEditorLookupStrategy (fromString defaultEditor))
            simpleEditorCommand (pure False)

    dieWith :: Int -> (forall ann. Pretty.Doc ann) -> IO a
    dieWith exitCode msg = do
        let subcommand :: forall ann. Pretty.Doc ann
            subcommand = pretty (usedName <> " config")

        errorMsg subcommand verbosity colourOutput stderr msg
        exitWith (ExitFailure exitCode)

getFilePath :: AppNames -> Config -> Maybe WhatToEdit -> IO (Maybe FilePath)
getFilePath appNames config = \case
    Just (EditFile fp) ->
        pure (Just fp)

    Just (EditSubcommandConfig subcommand) ->
        Just <$> getSubcommandConfigPathToEdit appNames config subcommand

    Nothing ->
        pure Nothing
