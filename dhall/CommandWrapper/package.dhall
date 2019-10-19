-- vim: filetype=dhall
--
-- Defaults and library of useful tools for toolset and external subcommands
-- bundled with Command Wrapper.

{ colourOutput =
      ./ColourOutput/package.dhall sha256:ab063b4e29e5237c1bbb2c4ed07ed3cc80096c9cd2f9978d727d81ac1f1c8c68
    ? ./ColourOutput/package.dhall
, command =
      ./Command/package.dhall sha256:41261a285c80fdfbef49adb3ced0be160dd9dc37bc89e8a0561da4917561eccd
    ? ./Command/package.dhall
, config =
    { cd =
        { defaults = ./cd/defaults
        , emptyDirectories = ./cd/emptyDirectories
        , systemShell = ./cd/systemShell
        , menuTools = { fzf = ./cd/menu-tools/fzf, fzy = ./cd/menu-tools/fzy }
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
      ./Editor/package.dhall sha256:ea947ca2fa9dec681385344190f8373da2d4ced8556cce9096c354f9265992f5
    ? ./Editor/package.dhall
, help =
    { command = ./help/command
    , metavar = ./help/metavar
    , option = ./help/option
    , plain = ./help/plain
    , value = ./help/value
    }
, schema =
      ./Schema/package.dhall sha256:6d9bebdef83104bcecba867f13da86a5b4d064cda1b3e2938a6979f806d5cb1d
    ? ./Schema/package.dhall
, terminalEmulator =
      ./TerminalEmulator/package.dhall sha256:7af1af071f36f10b12d8f076016fad8f9d070e5da793f20e42a0f4ec074bd519
    ? ./TerminalEmulator/package.dhall
, verbosity =
      ./Verbosity/package.dhall sha256:488f95a5a27b82653c5a759b592b08e16940d1698dcf956fcbd9c153cb2547f2
    ? ./Verbosity/package.dhall
, utils =
    { List =
        { head-and-tail = ./List/head-and-tail, index = ./List/index }
    , Optional =
          ./Optional/package.dhall sha256:33857949625599d84fb64d5b77946655d38fa1e3120ed397f2166a106b09e5f7
        ? ./Optional/package.dhall
    , url =
        { defaultPort = ./url/defaultPort
        , emptyPath = ./url/emptyPath
        , mk = ./url/mk
        , port = ./url/port
        , portToText = ./url/portToText
        }
    }
}
