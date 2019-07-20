let nix-completion =
      https://raw.githubusercontent.com/hedning/nix-bash-completions/43dfe677f58a0443dde2493fdee36395f3b64865/_nix
      sha256:749f88ded2f8a66ce3d5dc4f60d77e8e9de5ad4c05152d51ab66f333cf7f9d7d
      as Text


in  ''
    #!/usr/bin/env bash

    # Cannot be used due to sourced 'bash_completion'.
    #set -euo pipefail

    # Required by nix-completion, fails to parse otherwise.
    shopt -s extglob

    function usage() {
        cat <<EOF
    Completion for Nix commands with Command Wrapper style UI.

    Usage:

      ''${0##*/} [--index=NUM] [--shell=SHELL] [-- [WORD ...]]
      ''${0##*/} {--help|-h}
    EOF
    }

    function main() {
        local -a words=()
        local index=0

        local arg
        while (( $# )); do
            arg="$1"; shift
            case "''${arg}" in
              --index=*)
                index="''${arg#*=}"
                ;;

              --shell=*)
                ;;

              --)
                words=("$@")
                break
                ;;

              -h|--help)
                usage
                exit 0
                ;;
            esac
        done

        if ! command -v 'docker-compose' &>/dev/null; then
            # TODO: Consider using default shell completion.
            exit 0
        fi

        if (( index >= ''${#words[@]} )); then
            words+=("")
        fi

        function concat() {
            # Reason for this contraption is to prevent interpretation of e.g. '-e'
            # as an option to 'echo'.
            IFS=' ' echo "$*"
        }

        local lineBefore
        lineBefore="$(concat "''${words[@]:0:''${index}+1}")"

        local COMP_LINE; COMP_LINE="$(concat "''${words[@]}")"
        local COMP_POINT=$((''${#lineBefore} + 1))
        local COMP_WORDBREAKS="'\"><=;|&(: "
        local -a COMP_WORDS=("''${words[@]}")
        local COMP_CWORD="''${index}"
        local -a COMPREPLY

        # Debugging:
        : index="''${index}" 'words=(' "''${words[@]}" ')'

        function complete() {
            :
        }

        function compgen() {
            command compgen "$@"
        }

        # shellcheck source=/dev/null
        source /usr/share/bash-completion/bash_completion

        # Note to self, send a PR that fixes these when bored. Ha!
        # shellcheck disable=SC1083,SC2027,SC2034,SC2046,SC2059,SC2071,SC2086,SC2100,SC2128,SC2140,SC2155,SC2178,SC2179,SC2190,SC2206,SC2207,SC2209,SC2221,SC2222
        {
            ${nix-completion}

            _nix_completion
        }

        for reply in "''${COMPREPLY[@]}"; do
            if [[ -n "''${reply}" ]]; then
                printf -- '%s\n' "''${reply}"
            fi
        done
    }

    main "$@"
    ''
