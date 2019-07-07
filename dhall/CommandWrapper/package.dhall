{ colourOutput = ./colour-output

, command =
    { emptyArguments = ./command/emptyArguments
    , emptyEnvironment = ./command/emptyEnvironment
    , simple = ./command/simple
    , withExtraArguments = ./command/withExtraArguments
    }

-- Defaults and library of useful tools for toolset and external subcommands
-- bundled with Command Wrapper.
, config =
    { cd = ./cd-config

    , exec =
        { defaults = ./exec/defaults
        , emptyCommands = ./exec/emptyCommands
        , namedCommand = ./exec/namedCommand
        , namedCommandToAlias = ./exec/namedCommandToAlias
        , namedCommandsToAliases = ./exec/namedCommandsToAliases
        } 

    , skel =
        { defaults = ./skel/defaults
        , default-bash-skel = ./skel/default-bash-skel
        , default-dhall-skel = ./skel/default-dhall-skel
        , default-haskell-skel = ./skel/default-haskell-skel
        }

    , toolset = ./toolset-config
    }

, editor =
    { colon-separated = ./editor/colon-separated
    , some = ./editor/some
    , standard = ./editor/standard
    , visual-studio-code = ./editor/visual-studio-code
    }

, help =
    { command = ./help/command
    , metavar = ./help/metavar
    , option = ./help/option
    , plain = ./help/plain
    , value = ./help/value
    }

-- HTTP/HTTPS URL schemas
, schema =
    { fold = ./schema/fold
    , show = ./schema/show
    }

-- These are some known terminal emulators that support setting working
-- directory via command line option.
, terminalEmulator =
    { gnome-terminal = ./terminal-emulator/gnome-terminal
    , kitty = ./terminal-emulator/kitty
    , urxvt = ./terminal-emulator/urxvt
    }

, verbosity =
    { fold = ./verbosity/fold
    }

, utils =
    { `List` =
        { head-and-tail = ./List/head-and-tail
        , index = ./List/index
        }
    , `Optional` =
        { from = ./Optional/from
        }
    , url =
        { defaultPort = ./url/defaultPort
        , emptyPath = ./url/emptyPath
        , mk = ./url/mk
        , port = ./url/port
        , portToText = ./url/portToText
        }
    }
}
