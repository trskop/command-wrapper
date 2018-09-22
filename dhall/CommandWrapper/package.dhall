let commandWrapper =
      { verbosity = ./verbosity.dhall
      , mkDefaultConfig = ./default.dhall
      , mkCdConfig = ./cd.dhall
      , mkSkelConfig = ./skel.dhall
      , command = ./command.dhall
      , terminalEmulator = ./terminal-emulator.dhall
      , toolsetConfig = ./toolset-config.dhall
      }
in  commandWrapper
