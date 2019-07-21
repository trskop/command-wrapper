#!/usr/bin/env bash

set -euo pipefail

function usage() {
    cat <<EOF
Completion for direnv (version 2.20.1) with Command Wrapper style UI.

Usage:

  ${0##*/} [--hide-private-commands] [--index=NUM] [--shell=SHELL] [-- [WORD ...]]
  ${0##*/} {--help|-h}

Options:

  --hide-private-commands
      Don't complete 'direnv' subcommands that are considered private.  For
      more information see:

          direnv help show-private

  --index=INDEX
      INDEX of a WORD that we want to complete, starting from zero.  If there
      is no WORD with INDEX we assume empty string.  INDEX zero should be name
      of the command for which we are completing, i.e. 'direnv' in this case.

  --shell=SHELL
      SHELL for which we are doing command line completion.  At the moment this
      option is ignored.  In the future it may provide us with the ability to
      have nicer command line completion for specific shells.

  -- [WORD ...]
      Command line for which we are doing completion splitted into WORDs, i.e.
      tokenised.
EOF
}

function main() {
    local -a words=()
    local index=0
    local hidePrivateCommands=0

    local arg
    while (( $# )); do
        arg="$1"; shift
        case "${arg}" in
          --index=*)
            index="${arg#*=}"
            ;;

          --hide-private-commands)
            hidePrivateCommands=1
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

    if (( index >= ${#words[@]} )); then
        words+=('')
    fi

    # Debugging:
    : index="${index}" 'words=(' "${words[@]}" ')'

    # Bash complained about unbound variable if there was no WORD provided on
    # the command line, hence the ':-' operator.
    local -r current="${words[${index}]:-}"

    # Usage:
    #   compgenSubcommand WORD
    function compgenSubcommand() {
        if (( hidePrivateCommands )); then
            local -r -a privateSubcommands=()
        else
            local -r -a privateSubcommands=(
                'apply_dump'
                'show_dump'
                'dotenv'
                'dump'
                'expand_path'
                'export'
                'current'
            )
        fi
        local -r -a subcommands=(
            'allow'
            'deny'
            'edit'
            'exec'
            'help'
            'hook'
            'prune'
            'reload'
            'status'
            'stdlib'
            'version'
            'watch'
            "${privateSubcommands[@]}"
        )

        compgen -W "${subcommands[*]}" -- "$1"
    }

    # Usage:
    #   compgenShell WORD
    function compgenShell() {
        # https://github.com/direnv/direnv/blob/master/shell.go
        local -r supportedShells=(
            'bash'
            'elvish'
            'fish'
            'gzenv'
            'json'
            'tcsh'
            'vim'
            'zsh'
        )

        compgen -W "${supportedShells[*]}" -- "$1"
    }

    if (( index == 1 )); then
        compgenSubcommand "${current}"
    elif (( index > 1 )); then
        local -r subcommand="${words[1]}"

        case "${subcommand}" in
            allow)          # allow [PATH_TO_RC]
                if (( index == 2 )); then
                    compgen -A file -- "${current}"
                fi
                ;;
            deny)           # deny [PATH_TO_RC]
                if (( index == 2 )); then
                    compgen -A file -- "${current}"
                fi
                ;;
            edit)           # edit [PATH_TO_RC]
                if (( index == 2 )); then
                    compgen -A file -- "${current}"
                fi
                ;;
            exec)           # exec [DIR] COMMAND [...ARGS]
                # The fact that DIR is optional does make the UI cumbersome.
                if (( index == 2 )); then
                    if  [[ "${words[2]:-}" = '.'
                        || "${words[2]:-}" = '..'
                        || "${words[2]:-}" = '~'
                        || "${words[2]:-}" == ./*
                        || "${words[2]:-}" == ~/*
                        || "${words[2]:-}" == /*
                        ]]
                    then
                        compgen -A 'directory' -- "${current}"
                    else
                        compgen -A 'directory' -- "${current}" \
                        || compgen -A 'command' -- "${current}"
                    fi
                elif (( index == 3 )); then
                    if  [[ "${words[3]:-}" = '.'
                        || "${words[3]:-}" = '..'
                        || "${words[3]:-}" = '~'
                        || "${words[3]:-}" == ./*
                        || "${words[3]:-}" == ~/*
                        || "${words[3]:-}" == /*
                        ]]
                    then
                        compgen -A 'file' -- "${current}"
                    else
                        compgen -A 'command' -- "${current}" \
                        || compgen -A 'file' -- "${current}"
                    fi
                else
                    # Is this a good default?
                    compgen -A 'file' -- "${current}"
                fi
                ;;
            help)           # help [SHOW_PRIVATE]
                if (( index == 2 )); then
                    # The value of this argument is actually ignored, only its
                    # presence is detected by 'direnv'.
                    compgen -W 'show-private' -- "${current}"
                fi
                ;;
            hook)           # hook SHELL
                if (( index == 2 )); then
                    compgenShell "${current}"
                fi
                ;;
            watch)          # watch [SHELL] PATH
                if (( index == 2 )); then
                    compgenShell "${current}"
                else
                    compgen -A file -- "${current}"
                fi
                ;;

            # Private commands:
            apply_dump)     # apply_dump FILE
                compgen -A file -- "${current}"
                ;;
            show_dump)      # show_dump DUMP
                if (( index == 2 )); then
                    compgen -A file -- "${current}"
                fi
                ;;
            dotenv)         # dotenv [SHELL] [PATH_TO_DOTENV]
                if (( index == 2 )); then
                    compgenShell "${current}"
                else
                    compgen -A file -- "${current}"
                fi
                ;;
            dump)           # dump [SHELL]
                if (( index == 2 )); then
                    compgenShell "${current}"
                fi
                ;;
            expand_path)    # expand_path PATH [REL_TO]
                if (( index == 2 )); then
                    compgen -A file -- "${current}"
                elif (( index == 3 )); then
                    compgen -A directory -- "${current}"
                fi
                ;;
            export)         # export SHELL
                if (( index == 2 )); then
                    compgenShell "${current}"
                fi
                ;;
            current)        # current PATH
                if (( index == 2 )); then
                    compgen -A file -- "${current}"
                fi
                ;;

            # Either unknown command or a command that doesn't take any
            # arguments.  Soo wish that 'direnv' had better UI.
            *)
                # Nothing to complete.
                exit 1
                ;;
        esac
    fi
}

main "$@"
