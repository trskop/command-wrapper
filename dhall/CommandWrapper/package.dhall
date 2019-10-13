-- Defaults and library of useful tools for toolset and external subcommands
-- bundled with Command Wrapper.
--
-- vim: filetype=dhall

let -- HTTP/HTTPS URL schemas
    schema =
      { fold = ./schema/fold, show = ./schema/show }

let -- These are some known terminal emulators that support setting working
    -- directory via command line option.
    terminalEmulator =
      { gnome-terminal = ./terminal-emulator/gnome-terminal
      , kitty = ./terminal-emulator/kitty
      , urxvt = ./terminal-emulator/urxvt
      }

in  { colourOutput = ./colour-output
    , command =
        { emptyArguments = ./command/emptyArguments
        , emptyEnvironment = ./command/emptyEnvironment
        , simple = ./command/simple
        , withExtraArguments = ./command/withExtraArguments
        }
    , config =
        { cd =
            { defaults = ./cd/defaults
            , emptyDirectories = ./cd/emptyDirectories
            , systemShell = ./cd/systemShell
            , menuTools =
                { fzf = ./cd/menu-tools/fzf, fzy = ./cd/menu-tools/fzy }
            }
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
        , toolset =
            { defaults = ./toolset/defaults
            , emptyAliases = ./toolset/emptyAliases
            , emptySearchPath = ./toolset/emptySearchPath
            , emptyManPath = ./toolset/emptyManPath
            , addSubcommandAliases = ./toolset/addSubcommandAliases
            , defaultSearchPath = ./toolset/defaultSearchPath
            }
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
    , schema = schema
    , terminalEmulator = terminalEmulator
    , verbosity = { fold = ./verbosity/fold }
    , utils =
        { List = { head-and-tail = ./List/head-and-tail, index = ./List/index }
        , Optional = { from = ./Optional/from }
        , url =
            { defaultPort = ./url/defaultPort
            , emptyPath = ./url/emptyPath
            , mk = ./url/mk
            , port = ./url/port
            , portToText = ./url/portToText
            }
        }
    }
