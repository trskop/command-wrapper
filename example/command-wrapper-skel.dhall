  λ(description : Text)
→ λ(wrapper : Text)
→ λ(subcommand : Text)
→ { targetFile = "${env:HOME as Text}/.local/lib/${wrapper}/${subcommand}"
  , templates =
      { haskell =
          ''
          #!/usr/bin/env stack
          {- stack script
              --resolver lts-12.5
              --package dhall
              --package turtle
              --
          -}
          
          {-# LANGUAGE DeriveGeneric #-}
          {-# LANGUAGE LambdaCase #-}
          {-# LANGUAGE OverloadedStrings #-}
          {-# LANGUAGE ScopedTypeVariables #-}
          module Main (main)
            where
          
          import Control.Applicative ((<|>))
          import Data.Functor (void)
          import GHC.Generics (Generic)
          import System.Exit (die)
          
          import qualified Dhall
          import qualified Turtle
          -- TODO: Get rid of this if it's interpreted:
          import CommandWrapper.Environment (askVar, parseEnvIO)
          
          
          data Config = Config
            deriving (Generic, Show)
          
          instance Dhall.Interpret Config
          
          data Mode = Mode
            deriving (Generic, Show)
          
          main :: IO ()
          main = do
              (configFile, _wrapperName) <- parseEnvIO (die . show) $ do
                  void (askVar "COMMAND_WRAPPER_EXE")
                      <|> fail "This command must be executed as part of some command-wrapper environment"
          
                  (,) <$> askVar "COMMAND_WRAPPER_CONFIG"
                      <*> askVar "COMMAND_WRAPPER_NAME"
          
              config :: Config <- Dhall.inputFile Dhall.auto configFile
          
              Turtle.options "TODO: Describe me!" parseOptions >>= realMain config
          
          parseOptions :: Turtle.Parser Mode
          parseOptions = pure Mode
          
          realMain :: Config -> Mode -> IO ()
          realMain _config = \case
              Mode -> die "Error: TODO: Implement me!"
          
          -- ${"vim:ft=haskell"}
          ''
      , bash = ""
      }
  }

-- vim:expandtab
