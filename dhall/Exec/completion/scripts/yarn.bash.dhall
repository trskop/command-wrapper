-- vim: filetype=dhall

let -- This version of yarn-completion.bash works for yarn 1.17.x
    yarn-completion =
      https://raw.githubusercontent.com/dsifford/yarn-completion/f208d3ae3c5d8e0de2789348aec9265ddc385dcc/yarn-completion.bash
      sha256:cd6e752db396b34a4f639c118e81fc73e390d1a9d6086de29699c0381eb5cd31
      as Text

in  ''
    #!/usr/bin/env bash

    # Copyright (c) 2019, Peter Trsko
    #
    # All rights reserved.
    #
    # Redistribution and use in source and binary forms, with or without
    # modification, are permitted provided that the following conditions are met:
    #
    #     * Redistributions of source code must retain the above copyright
    #       notice, this list of conditions and the following disclaimer.
    #
    #     * Redistributions in binary form must reproduce the above
    #       copyright notice, this list of conditions and the following
    #       disclaimer in the documentation and/or other materials provided
    #       with the distribution.
    #
    #     * Neither the name of Peter Trsko nor the names of other
    #       contributors may be used to endorse or promote products derived
    #       from this software without specific prior written permission.
    #
    # THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    # "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    # LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    # A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
    # OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
    # SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
    # LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    # DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    # THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    # (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    # OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    # shellcheck shell=bash disable=2207
    # ^ Due to inlined yarn-completion.bash script

    # Cannot be used due to sourced 'bash_completion'.
    #set -euo pipefail

    function usage() {
        cat <<EOF
    Completion for yarn with Command Wrapper style UI.

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

        ${yarn-completion}

        _yarn

        for reply in "''${COMPREPLY[@]}"; do
            printf -- '%s\n' "''${reply% }"
        done
    }

    main "$@"
    ''
