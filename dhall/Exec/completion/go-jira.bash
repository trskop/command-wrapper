#!/usr/bin/env bash

# Cannot be used due to sourced go-jira completion.
#set -euo pipefail

function usage() {
    cat <<EOF
Completion for go-jira with Command Wrapper style UI.

Usage:

  ${0##*/} [--index=NUM] [--shell=SHELL] [-- [WORD ...]]
  ${0##*/} {--help|-h}
EOF
}

function main() {
    local -a words=()
    local index=0
    local goJiraCommand='jira'

    local arg
    while (( $# )); do
        arg="$1"; shift
        case "${arg}" in
          --index=*)
            index="${arg#*=}"
            ;;

          --shell=*)
            # Currently ignored.
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

    if (( ${#words[@]} )); then
        goJiraCommand="${words[0]}"
    fi

    if [[ "${goJiraCommand}" = /* ]]; then
        if [[ ! -x "${goJiraCommand}" ]]; then
            # TODO: Consider using default shell completion.
            exit 0
        fi
    elif ! command -v "${goJiraCommand}" &>/dev/null; then
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

    # shellcheck source=/dev/null
    source <("${goJiraCommand}" --completion-script-bash)

    _jira_bash_autocomplete

    for reply in "${COMPREPLY[@]}"; do
        printf -- '%s\n' "${reply% }"
    done
}

main "$@"
