{ colourOutput = ./colour-output

, command =
    { emptyArguments = ./command/emptyArguments
    , emptyEnvironment = ./command/emptyEnvironment
    , simple = ./command/simple
    , withExtraArguments = ./command/withExtraArguments
    }

-- HTTP/HTTPS URL schemas
, schema =
    { fold = ./schema/fold
    , show = ./schema/show
    }

-- These are some known terminal emulators that support setting working
-- directory via command line option.
, terminalEmulator =
    { urxvt = ./terminal-emulator/urxvt
    , kitty = ./terminal-emulator/kitty
    }

, verbosity =
    { fold = ./verbosity/fold
    }

-- Defaults and library of useful tools for toolset and external subcommands
-- bundled with Command Wrapper.
, config =
    { cd = ./cd-config

    , exec =
        { defaults = ./exec/defaults
        , emptyCommands = ./exec/emptyCommands
        , namedCommand = ./exec/namedCommand

        , bazel = ./exec/bazel
        , direnv = ./exec/direnv
        , docker = ./exec/docker
        , firefox = ./exec/firefox
        , pg_dump = ./exec/pg_dump
        , psql = ./exec/psql
        , run-mailcap = ./exec/run-mailcap
        , stack = ./exec/stack
        , tmux = ./exec/tmux
        , xdg-open = ./exec/xdg-open
        } 

    , skel =
        { default-bash-skel = ./skel/default-bash-skel
        , default-dhall-skel = ./skel/default-dhall-skel
        , default-haskell-skel = ./skel/default-haskell-skel
        }

    , toolset = ./toolset-config
    }

, utils =
    { `List` =
        { head-and-tail = ./List/head-and-tail
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
