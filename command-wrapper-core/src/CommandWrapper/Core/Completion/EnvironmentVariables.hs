{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.Core.Completion.EnvironmentVariables
-- Description: Completion of environment variables.
-- Copyright:   (c) 2019-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Completion of environment variables.
module CommandWrapper.Core.Completion.EnvironmentVariables
    ( EnvironmentVariablesOptions(..)
    , defEnvironmentVariablesOptions
    , environmentVariablesCompleter
    , queryEnvironmentVariables
    )
  where

import Control.Monad ((>>=), mapM_)
import Data.Functor ((<$>), fmap)
import Data.Function ((.))
import qualified Data.List as List
    ( filter
    , isPrefixOf
    , unlines
    )
import Data.Monoid ((<>))
import Data.String (String)
import Data.Tuple (fst)
import GHC.Generics (Generic)
import System.Environment (getEnvironment)
import System.IO (IO, putStrLn)
import Text.Show (Show)

import Data.Generics.Product.Typed (typed)
import Data.Output
    ( HasOutput(Output)
    , OutputFile(OutputFile)
    , OutputHandle(OutputHandle, OutputNotHandle)
    , OutputStdoutOrFile
    , pattern OutputStdoutOnly
    )
import qualified Data.Output (HasOutput(output))
import System.AtomicWrite.Writer.String (atomicWriteFile)


data EnvironmentVariablesOptions = EnvironmentVariablesOptions
    { word :: String
    , prefix :: String
    , suffix :: String

--  , expandContents :: Bool

    , output :: OutputStdoutOrFile

--  , matchingAlgorithm :: MatchingAlgorithm
    }
  deriving stock (Generic, Show)

instance HasOutput EnvironmentVariablesOptions where
    type Output EnvironmentVariablesOptions = OutputStdoutOrFile
    output = typed

defEnvironmentVariablesOptions :: EnvironmentVariablesOptions
defEnvironmentVariablesOptions = EnvironmentVariablesOptions
    { word = ""
    , prefix = ""
    , suffix = ""
    , output = OutputStdoutOnly
    }

-- | Complete environment variables.
environmentVariablesCompleter :: EnvironmentVariablesOptions -> IO [String]
environmentVariablesCompleter env =
    fmap (updateEntry env) <$> findEnvironmentEntries env
  where

queryEnvironmentVariables :: EnvironmentVariablesOptions -> IO ()
queryEnvironmentVariables env@EnvironmentVariablesOptions{output} =
    findEnvironmentEntries env >>= outputLines (updateEntry env) output

-- | List environment variables matching specified pattern.
findEnvironmentEntries :: EnvironmentVariablesOptions -> IO [(String, String)]
findEnvironmentEntries EnvironmentVariablesOptions{word} =
    List.filter ((word `List.isPrefixOf`) . fst) <$>  getEnvironment

updateEntry :: EnvironmentVariablesOptions -> (String, String) -> String
updateEntry EnvironmentVariablesOptions{prefix, suffix} = \(s, _) ->
    prefix <> s <> suffix

-- | Print completion output.
--
-- TODO: Move this into a separate module so that it can be shared when more
-- stuff will be moved out of "CommandWrapper.Internal.Subcommand.Completion"
-- module.
outputLines
    :: (a -> String)
    -- ^ Modify entry before printing it.
    -> OutputStdoutOrFile
    -> [a]
    -> IO ()
outputLines f = \case
    OutputHandle _ ->
        mapM_ (putStrLn . f)

    OutputNotHandle (OutputFile fn) ->
        atomicWriteFile fn . (List.unlines . fmap f)
