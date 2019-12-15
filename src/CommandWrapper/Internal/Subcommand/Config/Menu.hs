-- |
-- Module:      CommandWrapper.Internal.Subcommand.Config.Menu
-- Description: Simple terminal selection tool.
-- Copyright:   (c) 2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- Simple terminal selection tool.
module CommandWrapper.Internal.Subcommand.Config.Menu
    ( MenuOptions(..)
    , defMenuOptions
    , menu

    -- * Input
    , Input(..)
    , setInput
    )
  where

import Control.Exception (bracket)
import Data.Functor ((<&>))
import Data.Word (Word8)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (stdout)
import GHC.Generics (Generic)

import Graphics.Vty (Vty, mkVty)
import qualified Graphics.Vty as Vty
    ( Config(inputFd, outputFd)
    , Image
    , Vty(..)
    , Event(EvKey)
    , Key(KChar, KDown, KEnter, KEsc, KPageDown, KPageUp, KUp)
    , black
    , currentAttr
    , displayBounds
    , picForImage
    , standardIOConfig
    , string
    , update
    , vertCat
    , white
    , withBackColor
    , withForeColor
    )
import System.Posix (OpenMode(ReadWrite), closeFd, defaultFileFlags, openFd)

import CommandWrapper.Config.Global (Config(Config, colourOutput))
import CommandWrapper.Environment (AppNames(AppNames))
import CommandWrapper.Internal.Subcommand.Config.IsInput (IsInput(..))
import CommandWrapper.Options.ColourOutput (shouldUseColours)


data MenuOptions = MenuOptions
    { input :: Input
    , delimiter :: Char
    , selectedPrefix :: String
    , interruptedExitCode :: Word8
    , noSelectionExitCode :: Word8
    }
  deriving stock (Generic, Show)

defMenuOptions :: MenuOptions
defMenuOptions = MenuOptions
    { input = InputStdin
    , delimiter = '\n'
    , selectedPrefix = "> "
    , interruptedExitCode = 130 -- Same value as `fzf` returns.
    , noSelectionExitCode = 1
    }

menu :: AppNames -> Config -> MenuOptions -> IO ()
menu AppNames{} Config{colourOutput} opts@MenuOptions{delimiter, input} = do
    colours <- shouldUseColours stdout colourOutput
    items <- readInput delimiter input
    withVty (renderMenu items opts colours) >>= \case
        Selection s -> putStrLn s
        NoSelection -> exitFailure (noSelectionExitCode opts)
        Interrupted -> exitFailure (interruptedExitCode opts)
  where
    exitFailure :: Word8 -> IO a
    exitFailure = exitWith . ExitFailure . fromIntegral

renderItem :: MenuOptions -> Bool -> Bool -> String -> Vty.Image
renderItem MenuOptions{selectedPrefix} colours selected item =
    Vty.string (addBackground (addForeground Vty.currentAttr)) (prefix <> item)
  where
    prefix
      | selected  = selectedPrefix
      | otherwise = ' ' <$ selectedPrefix

    addForeground attr
      | not colours = attr
      | selected    = Vty.withForeColor attr Vty.black
      | otherwise   = Vty.withForeColor attr Vty.white

    addBackground attr
      | not colours = attr
      | selected    = Vty.withBackColor attr Vty.white
      | otherwise   = Vty.withBackColor attr Vty.black

data Result
    = Selection String
    | NoSelection
    | Interrupted

renderMenu :: [String] -> MenuOptions -> Bool -> Vty.Vty -> IO Result
renderMenu items opts colours vty = render 0 0 >>= loop
  where
    menuItems :: [(Int, String)]
    menuItems = zip [0..] items

    maxLine :: Int
    maxLine
      | null items = 0
      | otherwise  = length items - 1

    render :: Int -> Int -> IO (Int, Int, Int)
    render currentLine offset = do
        (_, rowsOnScreen) <- Vty.displayBounds (Vty.outputIface vty)
        let
            newOffset = adjustOffset (currentLine + offset) rowsOnScreen offset

            renderedItems =
                -- Dropping negative value is the same thing as 'drop 0'.
                drop (0 - newOffset) menuItems <&> \(lineNumber, content) ->
                    renderItem opts colours (lineNumber == currentLine) content

        Vty.update vty (Vty.picForImage (Vty.vertCat renderedItems))
        pure (currentLine, rowsOnScreen, newOffset)

    adjustOffset :: Int -> Int -> Int -> Int
    adjustOffset screenPosition rowsOnScreen offset
      | screenPosition >= rowsOnScreen =
            offset - (screenPosition - rowsOnScreen + 1)
      | screenPosition < 0 =
            offset - screenPosition
      | otherwise =
            offset

    loop :: (Int, Int, Int) -> IO Result
    loop (currentLine, pageLines, offset) =
        getAction vty maxLine pageLines currentLine >>= \case
            Quit ->
                pure Interrupted

            SelectedLine n
              | null items ->
                    pure NoSelection
              | otherwise ->
                    -- Function 'getAction' made sure that the following is
                    -- safe:
                    pure $ Selection (items !! n)

            LineChanged newLine ->
                render newLine offset >>= loop

            None ->
                loop (currentLine, pageLines, offset)

data Action
    = Quit
    | SelectedLine Int
    | LineChanged Int
    | None

getAction :: Vty.Vty -> Int -> Int -> Int -> IO Action
getAction vty maxLine pageLines currentLine = Vty.nextEvent vty <&> \case
    Vty.EvKey key _ -> case key of
        Vty.KUp ->
            LineChanged lineUp
        Vty.KDown ->
            LineChanged lineDown
        Vty.KEnter ->
            SelectedLine currentLine
        Vty.KEsc ->
            Quit
        Vty.KPageUp ->
            LineChanged pageUp
        Vty.KPageDown ->
            LineChanged pageDown
        Vty.KChar ch -> case ch of
            'k' ->
                LineChanged lineUp
            'j' ->
                LineChanged lineDown
            'q' ->
                Quit
            _ ->
                None
        _ ->
            None
    _ ->
        None
  where
    lineUp
      | currentLine <= 0 = 0
      | otherwise        = currentLine - 1

    lineDown
      | currentLine >= maxLine = maxLine
      | otherwise              = currentLine + 1

    pageUp
      | currentLine >= pageLines = currentLine - pageLines
      | otherwise                = 0

    pageDown =
        let newLine = currentLine + pageLines
        in  if newLine > maxLine then maxLine else newLine

withVty :: (Vty -> IO a) -> IO a
withVty f = (acquire `bracket` release) \(vty, _) -> f vty
  where
    acquire = do
        config <- Vty.standardIOConfig

        -- TODO: Gracefully handle case when "/dev/tty" cannot be opened.  It
        -- happens when there is no controlling terminal.
        ttyFd <- openFd "/dev/tty" ReadWrite Nothing defaultFileFlags

        (, ttyFd) <$> mkVty config
            { Vty.inputFd = Just ttyFd
            , Vty.outputFd = Just ttyFd
            }

    release (vty, ttyFd) = do
        Vty.shutdown vty
        closeFd ttyFd

-- {{{ Input ------------------------------------------------------------------

data Input
    = InputStdin
    | InputFile FilePath
    | InputItems [String]
  deriving stock (Show)

instance IsInput Input where
    parseInput s = InputFile <$> parseInput s

setInput :: Input -> MenuOptions -> MenuOptions
setInput input opts = opts{input}

readInput :: Char -> Input -> IO [String]
readInput delimiter = \case
    InputStdin ->
        parse <$> getContents

    InputFile file ->
        parse <$> readFile file

    InputItems items ->
        pure items
  where
    parse :: String -> [String]
    parse "" = []
    parse s  =
        uncurry (:) case break (== delimiter) s of
            (x, s') ->
                ( x
                , case s' of
                    [] -> []
                    _ : s'' -> parse s''
                )

-- }}} Input ------------------------------------------------------------------