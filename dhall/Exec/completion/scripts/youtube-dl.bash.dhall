-- vim: filetype=dhall
--
-- List of directories to look for are based on multiple sources of information.
-- This includes following:
--
-- * <https://salsa.debian.org/debian/youtube-dl/blob/master/Makefile>

let completionFor = "youtube-dl"

let -- Completion for version 2018.06.18
    completion =
      https://salsa.debian.org/debian/youtube-dl/raw/9d6a3ba754469c85b7f0f88714203f99ac69087a/youtube-dl.bash-completion sha256:6be47cff4e5059cbbaeb101947d4cd4e6cfadb4f666d5d2b687df142ab193b73 as Text

let entryPoint = "__youtube_dl"

in ''
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

    set -euo pipefail

    function findCompletion() {
        local cmdName="$1"; shift
        local cmd="$1"; shift

        local -a configDirectories=()
        configDirectories=(
            # E.g. /nix/store/''${hash}-''${cmdName}-''${version}/etc
            "$(dirname "$(command -v "''${cmd}")")/../etc"
            "''${HOME}/.nix-profile/etc"
            '/etc'
            '/usr/local/etc'
            '/usr/etc'
        )

        local prefix
        for configDir in "''${configDirectories[@]}"; do
            prefix="''${configDir}/bash_completion.d/''${cmdName}"
            : prefix="''${prefix}" # Debugging
            for file in "''${prefix}" "''${prefix}.bash" "''${prefix}.bash-completion"; do
                if file="$(readlink -e "''${file}")"; then
                    : file="''${file}" # Debugging
                    echo "''${file}"
                    return 0
                fi
            done
        done
    }

    function usage() {
        cat <<EOF
    Completion for ${completionFor} with Command Wrapper style UI.

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

        if ! command -v '${completionFor}' &>/dev/null; then
            # TODO: Consider using default shell completion.
            exit 0
        fi

        if (( index >= ''${#words[@]} )); then
            words+=(''')
        fi

        # Debugging:
        : index="''${index}" 'words=(' "''${words[@]}" ')'

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

        function complete() {
            :
        }

        function compgen() {
            command compgen "$@"
        }

        local completionFile='''
        completionFile="$(findCompletion '${completionFor}' '${completionFor}')"

        if [[ -n "''${completionFile}" ]]; then
            # shellcheck source=/dev/null
            source "''${completionFile}"
        else
        # shellcheck disable=SC2086,SC2207
        {
            ${completion}
        }
        fi

        ${entryPoint}

        for reply in "''${COMPREPLY[@]}"; do
            printf -- '%s\n' "''${reply% }"
        done
    }

    main "$@"
    ''
