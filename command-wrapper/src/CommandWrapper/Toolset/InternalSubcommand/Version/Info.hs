-- |
-- Module:      $Header$
-- Description: Data type representing Command Wrapper version information.
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Data type representing Command Wrapper version information.
module CommandWrapper.Toolset.InternalSubcommand.Version.Info
    ( VersionInfo(..)
    , PrettyVersion(..)
    , VersionInfoField(..)
    , versionQQ
    )
  where

import Prelude (error, fromIntegral)

import Control.Monad (fail)
import Data.Bool (otherwise)
import Data.Eq (Eq, (/=), (==))
import Data.Function ((.), const)
import Data.Functor (fmap)
import qualified Data.List as List (dropWhile, takeWhile)
import Data.Int (Int)
import Data.Semigroup ((<>))
import Data.Ord (Ord)
import Data.Tuple (snd)
import Data.Version
    ( Version(versionBranch)
    , makeVersion
    , parseVersion
    , showVersion
    )
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Text.Show (Show, show)
import Text.ParserCombinators.ReadP (readP_to_S)

import Data.Text.Prettyprint.Doc (Pretty(pretty))
import Dhall (ToDhall)
import qualified Dhall
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (liftData)


data VersionInfo = VersionInfo
    { commandWrapper :: PrettyVersion
    , subcommandProtocol :: PrettyVersion
    , dhallLibrary :: PrettyVersion
    , dhallStandard :: PrettyVersion
    }
  deriving stock (Generic, Show)
  deriving anyclass (ToDhall)

newtype PrettyVersion = PrettyVersion {rawVersion :: Version}
  deriving stock (Generic, Show)

instance Pretty PrettyVersion where
    pretty (PrettyVersion v)
      | v == makeVersion [] = "N/A"
      | otherwise           = pretty (showVersion v)

instance ToDhall PrettyVersion where
    injectWith opts = Dhall.Encoder
        { embed = embedList . toNaturals . versionBranch . rawVersion
        , declared
        }
      where
        Dhall.Encoder{embed = embedList, declared} = Dhall.injectWith opts

        toNaturals :: [Int] -> [Natural] = fmap fromIntegral

data VersionInfoField
    = CommandWrapperField
    | SubcommandProtocolField
    | DhallLibrary
    | DhallStandard
  deriving stock (Eq, Generic, Ord, Show)

-- | Parse CPP version definition (usually provided by Cabal) into a 'Version'.
-- Using 'QuasiQuoter' allows us to fail during compilation.
versionQQ :: QuasiQuoter
versionQQ = QuasiQuoter
    { quoteExp = \s ->
        case parseVersion' s of
            (v, "") : _ -> liftData v
            _           -> fail (show s <> ": Unable to parse as a Version.")

    , quotePat = undef "Cannot use 'version' QuasiQuoter as a pattern."
    , quoteType = undef "Cannot use 'version' QuasiQuoter as a type."
    , quoteDec = undef "Cannot use 'version' QuasiQuoter as a  declaration."
    }
  where
    undef = const . error

    -- Our input is e.g. "\"1.20.1\"".  This is due to the fact that we are
    -- using CPP definition as an input.
    parseVersion' =
        List.dropWhile ((/= "") . snd) . readP_to_S parseVersion
        . List.takeWhile (/= '"') . List.dropWhile (== '"')
