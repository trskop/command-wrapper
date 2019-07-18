#!/usr/bin/env bash

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
        echo "$@"
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

    # shellcheck source=/dev/null
    source <(findCompletion 'bazel' 'bazel')

    _bazel__complete

    for reply in "${COMPREPLY[@]}"; do
        echo "${reply% }"
    done
}

main "$@"
