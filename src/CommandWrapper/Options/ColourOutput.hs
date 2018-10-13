{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:      CommandWrapper.Options.Colour
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module CommandWrapper.Options.ColourOutput
    ( ColourOutput(..)
    , parse

    -- * Environment
    , noColorEnvVar

    -- * Terminal
    , terminalSupportsColours

    -- * Options
    , options
    , colourOption
    , colorOption
    , noColourFlag
    , noColorFlag
    )
  where

import Control.Applicative (optional)
import Data.Maybe (isJust)
import Data.Monoid (Last(Last, getLast), mconcat)
import Data.String (IsString)
import GHC.Generics (Generic)

import qualified Data.CaseInsensitive as CI (mk)
import qualified Dhall (Inject, Interpret)
import qualified Options.Applicative as Options
import System.Console.Terminfo (Terminal, getCapability, termColors)

import CommandWrapper.Environment.Parser (ParseEnv, askOptionalVar)


data ColourOutput
    = Always
    -- ^ Always use colourised output, even if output is not a terminal. This
    -- can be useful when, for example, piping output to a pager.
    | Auto
    -- ^ Use colourised output when the output is a terminal that supports it.
    | Never
    -- ^ Never produce colourised output.
  deriving (Generic, Show)

instance Dhall.Inject ColourOutput
instance Dhall.Interpret ColourOutput

-- | Check for presence of @NO_COLOR@ environment variable. If present, then
-- set `ColourOutput` value to `Never`, otherwise keep it as it was.
--
-- @NO_COLOR@ einvironment variable is an informal standard which is available
-- online at <https://no-color.org>.
--
-- > Accepting the futility of trying to reverse this trend, an informal
-- > standard is hereby proposed:
-- >
-- > > All command-line software which outputs text with ANSI color added
-- > > should check for the presence of a NO_COLOR environment variable that,
-- > > when present (regardless of its value), prevents the addition of ANSI
-- > > color.
noColorEnvVar :: ParseEnv (Maybe ColourOutput)
noColorEnvVar = fmap (const Never) <$> askOptionalVar "NO_COLOR"

-- | Does specified terminal support colours?
terminalSupportsColours :: Terminal -> Bool
terminalSupportsColours term = isJust (getCapability term termColors)

options :: Options.Parser (Maybe ColourOutput)
options = go
    <$> noColourFlag
    <*> noColorFlag
    <*> optional colourOption
    <*> optional colorOption
  where
    go a b c d = getLast . mconcat $ map Last [a, b, c, d]

noColourFlag :: Options.Parser (Maybe ColourOutput)
noColourFlag = noColourFlag' "no-colour"

noColorFlag :: Options.Parser (Maybe ColourOutput)
noColorFlag = noColourFlag' "no-color"

noColourFlag' :: String ->  Options.Parser (Maybe ColourOutput)
noColourFlag' name = Options.flag Nothing (Just Never) $ mconcat
    [ Options.long name
    , Options.help "Never use colourised output. Same as '--colour=never'."
    ]

colourOption :: Options.Parser ColourOutput
colourOption = colourOption' "colour"

colorOption :: Options.Parser ColourOutput
colorOption = colourOption' "color"

colourOption' :: String -> Options.Parser ColourOutput
colourOption' name = Options.option parse' $ mconcat
    [ Options.long name
    , Options.help "Specify if and when colourised output should be used.\
        \Possible values are 'always', 'auto', and 'never'."
    , Options.metavar "WHEN"
    ]
  where
    parse' = Options.maybeReader (parse . CI.mk)

parse :: (IsString s, Eq s) => s -> Maybe ColourOutput
parse = \case
    "always" -> Just Always
    "auto" -> Just Auto
    "never" -> Just Never
    _ -> Nothing
