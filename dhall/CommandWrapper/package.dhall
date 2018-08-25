let commandWrapper =
      { verbosity = https://raw.githubusercontent.com/trskop/verbosity/master/dhall/Verbosity/package.dhall
      , mkDefaultConfig = ./default.dhall
      , mkCdConfig = ./cd.dhall
      , mkSkelConfig = ./skel.dhall
      , command = ./command.dhall
      , terminalEmulator = ./terminal-emulator.dhall
      }
in  commandWrapper
