  λ(description : Text)
→ λ(wrapper : Text)
→ λ(subcommand : Text)
→ { targetFile = "${env:HOME as Text}/.local/lib/${wrapper}/${subcommand}"
  , templates =
      { haskell =
          { targetFile = [] : Optional Text
          , executable = True
          , template =
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

                  mode <- Turtle.options "TODO: Describe me!" parseOptions
                  config <- Dhall.inputFile Dhall.auto configFile
                  realMain config mode

              parseOptions :: Turtle.Parser Mode
              parseOptions = pure Mode

              realMain :: Config -> Mode -> IO ()
              realMain _config = \case
                  Mode -> die "Error: TODO: Implement me!"

              -- ${"vim:ft=haskell"}
              ''
          }
      , bash =
          { targetFile = [] : Optional Text
          , executable = True
          , template =
              ''
              #!/usr/bin/env bash

              set -e

              function declareCfg() {
                  local -r configFile="$1"; shift
                  local -r name="$1"; shift

                  dhall-to-bash --declare "''${name}" < "''${configFile}"
              }

              function main() {
                  local arg
                  while (( $# )); do
                      arg="$1"; shift
                      case "''${arg}" in
                          -h|--help)
                              printHelp
                              exit 0
                              ;;
                          -*)
                              error 1 "'%s': %s" "''${arg}" 'Unknown option.'
                              ;;
                          *)
                              error 1 "'%s': %s" "''${arg}" 'Too many arguments.'
                              ;;
                      esac
                  done

                  if  [[ -n "''${COMMAND_WRAPPER_EXE}" ]]; then
                      error 1 'Error: COMMAND_WRAPPER_EXE: %s: %s' \
                          'Missing environment variable' \
                          'This command must be executed inside command-wrapper environment.'
                  fi

                  if  [[ -n "''${COMMAND_WRAPPER_NAME}" ]]; then
                      error 1 'Error: COMMAND_WRAPPER_NAME: %s: %s' \
                          'Missing environment variable' \
                          'This command must be executed inside command-wrapper environment.'
                  fi

                  if  [[ -n "''${COMMAND_WRAPPER_CONFIG}" ]]; then
                      error 1 'Error: COMMAND_WRAPPER_CONFIG: %s: %s' \
                          'Missing environment variable' \
                          'This command must be executed inside command-wrapper environment.'
                  fi

                  eval "$(declareCfg "''${COMMAND_WRAPPER_CONFIG}" 'config')"

                  # TODO: Implement me!
              }

              main "$@"
              ''
          }
      }
  }

-- vim:expandtab
