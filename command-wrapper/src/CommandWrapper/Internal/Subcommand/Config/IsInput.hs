{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.Internal.Subcommand.Config.IsInput
-- Description: Class of data types that can represent FilePath.
-- Copyright:   (c) 2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- Class of data types that can represent 'FilePath'.
module CommandWrapper.Internal.Subcommand.Config.IsInput
    ( IsInput(..)
    )
  where

import Data.Either (Either)
import Data.String (String)
import System.IO (FilePath)

import System.FilePath.Parse (parseFilePath)


class IsInput a where
    parseInput :: String -> Either String a

instance IsInput FilePath where
    parseInput = parseFilePath
