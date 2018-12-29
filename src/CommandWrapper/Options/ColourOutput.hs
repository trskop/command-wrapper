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
    , shouldUseColours

    -- * Options
    , options
    , colourOption
    , colorOption
    , noColourFlag
    , noColorFlag
    )
  where

import Control.Applicative (optional)
import Data.Monoid (Last(Last, getLast), mconcat)
import System.IO (Handle, hIsTerminalDevice)

import qualified Data.CaseInsensitive as CI (mk)
import Data.Output.Colour
    ( ColourOutput(..)
    , noColorEnvVar
    , parse
    , terminalSupportsColours
    , useColoursWhen
    )
import qualified Options.Applicative as Options
import System.Console.Terminfo (setupTermFromEnv)


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

-- | Check if we should use colours for the specified output handle.
shouldUseColours :: Handle -> ColourOutput -> IO Bool
shouldUseColours handle = useColoursWhen $ do
    otputIsTerminal <- hIsTerminalDevice handle
    if otputIsTerminal
        then terminalSupportsColours <$> setupTermFromEnv
        else pure False
