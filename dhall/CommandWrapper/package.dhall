let Verbosity/fold =
      https://raw.githubusercontent.com/trskop/verbosity/9dd482bfbd767c2a79e5bdec8e3d0bd4812b75c0/dhall/Verbosity/fold
      sha256:4dac2c264a2531d569ad0e5f712a1cd2d17b51ecdc502cc72f19937bf4733b1e

in  { verbosity =
        { fold = Verbosity/fold
        }
    , colourOutput = ./colour-output.dhall
    , mkDefaultConfig = ./default.dhall
    , mkCdConfig = ./cd.dhall
    , command = ./command.dhall
    , terminalEmulator = ./terminal-emulator.dhall
    , toolsetConfig = ./toolset-config.dhall
    }
