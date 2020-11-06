-- vim: filetype=dhall
--
-- Takes standard Bash completion file in a script that provides Command
-- Wrapper-style UI on top.
--
-- This whole file is a templating function with following type signature:
--
-- ```Dhall
-- Options → Text
-- ```
--
-- Where `Options` are defined and documented in [Options/Type](./Options/Type).
--
-- Generated script has following calling convention:
--
-- ```
-- COMMAND [--index=NUM] [--shell=SHELL] [-- [WORD ...]]
-- ```

let Options =
        ./Options/Type.dhall sha256:9ff8f2f6d9cd3a786d7e9ff5d22f807e3f471287d83903b26a3324a4dc163a91
      ? ./Options/Type.dhall

let Prelude =
        ../../../Prelude/package.dhall sha256:2086c6a2e7ce8ee371858036ab60d24d045969defadb4bc9d87639921ec2e028
      ? ../../../Prelude/package.dhall

let findCompletionFunction =
      ''
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
      ''

let sourceLibrary =
      λ(lib : Optional Text) →
        Prelude.Optional.fold
          Text
          lib
          Text
          ( λ(_ : Text) →
              ''
                  # shellcheck source=/dev/null
                  source $'${_}'
              ''
          )
          ""

in  λ(opts : Options) →
      ''
      #!/usr/bin/env bash

      # This script was generated.

      ${if    opts.strictMode
        then  ''
              set -euo pipefail
              ''
        else  ""}
      ${if opts.searchForCompletionFile then findCompletionFunction else ""}

      function usage() {
          cat <<EOF
      Completion for ${opts.completionFor} with Command Wrapper style UI.

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

          if ! command -v '${opts.completionFor}' &>/dev/null; then
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
          ${if    opts.searchForCompletionFile
            then  ''
                      completionFile="$(
                          findCompletion '${opts.completionFor}' '${opts.completionFor}'
                      )"
                  ''
            else  ""}

          ${sourceLibrary opts.sourceLibrary}

          if [[ -n "''${completionFile}" ]]; then
              # shellcheck source=/dev/null
              source "''${completionFile}"
          else
          {
              ${Prelude.Optional.fold
                  Text
                  opts.completion
                  Text
                  (λ(_ : Text) → _)
                  "exit 1"}
          }
          fi

          ${opts.entryPoint}

          for reply in "''${COMPREPLY[@]}"; do
              printf -- '%s\n' "''${reply% }"
          done
      }

      main "$@"
      ''