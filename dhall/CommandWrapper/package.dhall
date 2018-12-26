let commandWrapper =
      { verbosity = ./verbosity.dhall
      , colourOutput = ./colour-output.dhall
      , mkDefaultConfig = ./default.dhall
      , mkCdConfig = ./cd.dhall
      , command = ./command.dhall
      , terminalEmulator = ./terminal-emulator.dhall
      , toolsetConfig = ./toolset-config.dhall
      }
in  commandWrapper
