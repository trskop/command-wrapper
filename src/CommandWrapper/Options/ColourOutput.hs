-- |
-- Module:      $Header$
-- Description: Colourised output preferences
-- Copyright:   (c) 2018-2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Colourised output preferences.
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


-- |
-- > [--[no-]colo[u]r --colo[u]r=WHEN]
options :: Options.Parser (Maybe ColourOutput)
options = go
    <$> noColourFlag
    <*> noColorFlag
    <*> optional colourOption
    <*> optional colorOption
  where
    go a b c d = getLast . mconcat $ map Last [a, b, c, d]

-- |
-- > [--no-colo[u]r]
noColourFlag :: Options.Parser (Maybe ColourOutput)
noColourFlag = noColourFlag' "no-colour"

-- |
-- > [--no-color]
noColorFlag :: Options.Parser (Maybe ColourOutput)
noColorFlag = noColourFlag' "no-color"

noColourFlag' :: String ->  Options.Parser (Maybe ColourOutput)
noColourFlag' name = Options.flag Nothing (Just Never) $ mconcat
    [ Options.long name
    , Options.help "Never use colourised output. Same as '--colour=never'."
    ]

-- |
-- > --colour=WHEN
colourOption :: Options.Parser ColourOutput
colourOption = colourOption' "colour"

-- |
-- > --color=WHEN
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

-- | Check if we should use colours for the specified output handle.  This
-- function doesn't look for @NO_COLOR@ environment variable.  That has to be
-- done manually before calling this function.  It would be impossible to
-- override @NO_COLOR@ by options\/configuration otherwise.
--
-- See also 'noColorEnvVar' for more details.
--
-- Simple usage example:
--
-- @
-- printMessage :: Config -> 'Handle' -> Doc Style -> IO ()
-- printMessage Config{colourOutput} handle msg = do
--     useColours <- 'shouldUseColours' handle colourOutput
--     if useColours
--         then printColourisedMessage handle msg
--         else printPlainMessage handle msg
-- @
shouldUseColours
    :: Handle
    -- ^ A handle to which we want to print colourised output.  In case of
    -- 'Auto' we are checking if this is connected to a terminal or not, and
    -- if it is then we are checking that the terminal supports colours.
    -> ColourOutput
    -- ^ User preferences.  Usually combination of configuration, environment
    -- ('noColorEnvVar'), and command line options that culminate in this value.
    -> IO Bool
    -- ^ Return values:
    --
    -- * 'False' - Don't use colours when printing to specified 'Handle'.
    -- * 'True' - You are free to use colours when printing to specified
    --   'Handle'.
shouldUseColours handle = useColoursWhen $ do
    otputIsTerminal <- hIsTerminalDevice handle
    if otputIsTerminal
        then terminalSupportsColours <$> setupTermFromEnv
        else pure False
