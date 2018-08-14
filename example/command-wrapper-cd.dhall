{ directories =
    [ "${env:HOME as Text}/.local/src/github.com/trskop/command-wrapper"
    , "${env:HOME as Text}/Devel"
    , "${env:HOME as Text}/Downloads"
    ] : List Text
, menuTool = "fzf"
, shell = "${env:SHELL as Text ? "bash"}"
}
