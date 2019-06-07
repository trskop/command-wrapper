-- |
-- Module:      CommandWrapper.HelpMessage
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module CommandWrapper.HelpMessage
    ( Annotation(..)
    , Annotated(..)
    , HelpMessage(..)
    , Paragraph
    , Usage
    )
  where

import GHC.Generics (Generic)

import Data.Text (Text)


data Annotation
    = Plain
    | Option
    | Metavar
    | Value
    | Command
  deriving stock (Eq, Generic, Show)

data Annotated a = Annotated
    { annotation :: Annotation
    , content :: a
    }
  deriving stock (Eq, Generic, Show)

type Paragraph a = [Annotated a]

type Usage a = [Annotated a]

data Section a = Section
    { name :: Paragraph a
    , content :: [Paragraph a]
    }
  deriving stock (Eq, Generic, Show)

data HelpMessage a = HelpMessage
    { description :: Paragraph a
    , usage :: [Usage a]
    , sections :: [Section a]
    , message :: [Paragraph a]
    }
  deriving stock (Eq, Generic, Show)

-- Basically smart words function.
--smartReflow :: [Annotated Text] -> [Annotation Text]
--smartReflow =

--render :: HelpMessage Text -> Doc Annotation
--render =
