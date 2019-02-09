let foldVerbosity =
      https://raw.githubusercontent.com/trskop/verbosity/e6c643d9e39fac2f512d4c76594807ba14339b4a/dhall/Verbosity/fold
      sha256:d75266e224b698e607681b50e9f49bc35f95b749493d239b6a8758a26ad7a3c9

in  { verbosity =
        { fold = foldVerbosity
        }
    , colourOutput = ./colour-output.dhall
    , mkDefaultConfig = ./default.dhall
    , mkCdConfig = ./cd.dhall
    , command = ./command.dhall
    , terminalEmulator = ./terminal-emulator.dhall
    , toolsetConfig = ./toolset-config.dhall
    }
