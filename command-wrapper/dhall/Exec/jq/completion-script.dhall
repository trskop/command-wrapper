-- vim: filetype=dhall

let completionFor = "jq"

let completion =
      https://raw.githubusercontent.com/perlpunk/shell-completions/6af9f7cd5db837680aef453ca6ded1a3dd219eae/bash/jq.bash sha256:9c8d6062196559e73ae82191f77e5424c5157951331d72516e24a197802109bb as Text

let entryPoint = "_jq"

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

    # Cannot be used due to sourced 'bash_completion'.
    #set -euo pipefail

    function usage() {
        cat <<EOF
    Completion for ${completionFor} commands with Command Wrapper style UI.

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
        # shellcheck disable=SC1083,SC2003,SC2004,SC2006,SC2027,SC2034,SC2046,SC2059
        # shellcheck disable=SC2068,SC2071,SC2086,SC2100,SC2119,SC2120,SC2128,SC2140
        # shellcheck disable=SC2155,SC2178,SC2179,SC2190,SC2206,SC2207,SC2209,SC2221
        # shellcheck disable=SC2222
        {
            ${completion}

            ${entryPoint}
        }

        for reply in "''${COMPREPLY[@]}"; do
            if [[ -n "''${reply}" ]]; then
                if [[ "''${reply}" =~ -*, ]]; then
                    # Some options seem to have trailing ',' character.
                    reply="''${reply%,}"
                fi
                printf -- '%s\n' "''${reply}"
            fi
        done
    }

    main "$@"
    ''
