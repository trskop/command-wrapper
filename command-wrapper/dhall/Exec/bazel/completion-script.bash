#!/usr/bin/env bash

# Copyright (c) 2019-2020, Peter Trsko
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

    local -a shareDirectories=()
    shareDirectories=(
        # E.g. /nix/store/${hash}-bazel-${version}/share
        "$(dirname "$(command -v "${cmd}")")/../share"
        "${HOME}/.nix-profile/share"
        '/usr/local/share'
        '/usr/share'
    )

    local completionFile
    for shareDir in "${shareDirectories[@]}"; do
        completionFile="${shareDir}/bash-completion/completions/${cmdName}"
        if completionFile="$(readlink -e "${completionFile}")"; then
            # Debugging:
            : completionFile="${completionFile}"
            cat "${completionFile}"
            return 0
        fi
    done

    echo 'return 1'
}

function usage() {
    cat <<EOF
Completion for Bazel with Command Wrapper style UI.

Usage:

  ${0##*/} [--index=NUM] [--shell=SHELL] [-- [WORD ...]]
  ${0##*/} {--help|-h}

Environment variables:

  BAZEL_COMPLETION_USE_QUERY
      Possible values are 'false' and 'true'. If set to 'true' then
      'bazel query' command will be used to provide completion, otherwise
      'grep'-based heuristic will be used. Be aware that 'bazel query' is much
      slower.

  BAZEL_COMPLETION_ALLOW_TESTS_FOR_RUN
      Possible values are 'false' and 'true'. If set to 'true' then 'bazel run'
      completion will include test targets. Useful when running them manually
      is part of your workflow.
EOF
}

function main() {
    local -a words=()
    local index=0

    local arg
    while (( $# )); do
        arg="$1"; shift
        case "${arg}" in
          --index=*)
            index="${arg#*=}"
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

    if ! command -v 'bazel' &>/dev/null; then
        # TODO: Consider using default shell completion.
        exit 0
    fi

    if (( index >= ${#words[@]} )); then
        words+=('')
    fi

    # Debugging:
    : index="${index}" 'words=(' "${words[@]}" ')'

    function concat() {
        # Reason for this contraption is to prevent interpretation of e.g. '-e'
        # as an option to 'echo'.
        IFS=' ' echo "$*"
    }

    local lineBefore
    lineBefore="$(concat "${words[@]:0:${index}+1}")"

    local COMP_LINE; COMP_LINE="$(concat "${words[@]}")"
    local COMP_POINT=$((${#lineBefore} + 1))
    local COMP_WORDBREAKS="'\"><=;|&(: "
    local -a COMP_WORDS=("${words[@]}")
    local COMP_CWORD="${index}"
    local -a COMPREPLY

    function complete() {
        :
    }

    function compgen() {
        command compgen "$@"
    }

    # These variables need to be set to not create a havoc when running
    # completion. The way they are defined allows them to be eoverriden by
    # setting the environment variables
    export BAZEL_COMPLETION_USE_QUERY="${BAZEL_COMPLETION_USE_QUERY:-false}"
    export BAZEL_COMPLETION_ALLOW_TESTS_FOR_RUN="${BAZEL_COMPLETION_ALLOW_TESTS_FOR_RUN:-false}"

    # shellcheck source=/dev/null
    source <(findCompletion 'bazel' 'bazel')

    _bazel__complete

    for reply in "${COMPREPLY[@]}"; do
        printf -- '%s\n' "${reply% }"
    done
}

main "$@"
