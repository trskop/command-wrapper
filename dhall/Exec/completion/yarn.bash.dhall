let yarn-completion =
      -- This version of yarn-completion.bash works for yarn 1.17.x
      https://raw.githubusercontent.com/dsifford/yarn-completion/f208d3ae3c5d8e0de2789348aec9265ddc385dcc/yarn-completion.bash
      sha256:cd6e752db396b34a4f639c118e81fc73e390d1a9d6086de29699c0381eb5cd31
      as Text

in  ''
    #!/usr/bin/env bash
    
    # shellcheck shell=bash disable=2207
    # ^ Due to inlined yarn-completion.bash script
    
    # Cannot be used due to sourced 'bash_completion'.
    #set -euo pipefail
    
    function usage() {
        cat <<EOF
    Completion for docker-compose with Command Wrapper style UI.
    
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
            echo "$@"
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
            echo "''${reply% }"
        done
    }
    
    main "$@"
    ''
