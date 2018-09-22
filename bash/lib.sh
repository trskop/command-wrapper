# Library for writing CommandWrapper subcommands in Bash.

# Usage:
#
#   verbosityToNum VERBOSITY
function verbosityToNum() {
    case "$1" in
      'silent')   echo 0;;
      'normal')   echo 1;;
      'verbose')  echo 2;;
      'annoying') echo 3;;
      [0-3])      echo "$1";;
    esac
}

# Usage:
#
#   msgf VERBOSITY MESSAGE_TYPE FORMAT [ARGUMENTS]
function msgf() {
    local -r verbosity="$(verbosityToNum "$1")"; shift
    local -r type="$1"; shift
    local -r format="$1"; shift

    case "${type}" in
        'info')
            local -r messageType='Info'
            local -r -i typeNum=3
            ;;
        'notice')
            local -r messageType='Notice'
            local -r -i typeNum=2
            ;;
        'warning')
            local -r messageType='Warning'
            local -r -i typeNum=1
            ;;
        'error')
            local -r messageType='Error'
            local -r -i typeNum=1
            ;;
    esac

    if (( typeNum <= verbosity )); then
        printf "${messageType}: ${format}\n" "$@"
    fi
}

# Usage:
#
#   info FORMAT [ARGUMENTS]
function info() {
    msgf "${COMMAND_WRAPPER_VERBOSITY:-normal}" 'info' "$@"
}

# Usage:
#
#   notice FORMAT [ARGUMENTS]
function notice() {
    msgf "${COMMAND_WRAPPER_VERBOSITY:-normal}" 'notice' "$@"
}

# Usage:
#
#   warn FORMAT [ARGUMENTS]
function warn() {
    msgf "${COMMAND_WRAPPER_VERBOSITY:-normal}" 'warning' "$@"
}

# Usage:
#
#   error FORMAT [ARGUMENTS]
function error() {
    msgf "${COMMAND_WRAPPER_VERBOSITY:-normal}" 'error' "$@"
}

# Usage:
#
#   die EXIT_CODE FORMAT [ARGUMENTS]
function die() {
    local -r exitCode="$1"; shift

    error "$@"
    exit "${exitCode}"
}

# Usage:
#
#   dieIfExecutedOutsideOfCommandWrapperEnvironment
function dieIfExecutedOutsideOfCommandWrapperEnvironment() {
    if  [[ -z "${COMMAND_WRAPPER_EXE}" ]]; then
        die 2 'COMMAND_WRAPPER_EXE: %s: %s' \
            'Missing environment variable' \
            'This command must be executed inside command-wrapper environment.'
    fi

    if  [[ -z "${COMMAND_WRAPPER_NAME}" ]]; then
        die 2 'COMMAND_WRAPPER_NAME: %s: %s' \
            'Missing environment variable' \
            'This command must be executed inside command-wrapper environment.'
    fi

    if  [[ -z "${COMMAND_WRAPPER_CONFIG}" ]]; then
        die 2 'COMMAND_WRAPPER_CONFIG: %s: %s' \
            'Missing environment variable' \
            'This command must be executed inside command-wrapper environment.'
    fi

    if  [[ -z "${COMMAND_WRAPPER_VERBOSITY}" ]]; then
        die 2 'COMMAND_WRAPPER_VERBOSITY: %s: %s' \
            'Missing environment variable' \
            'This command must be executed inside command-wrapper environment.'
    fi
}

# List CommandWrapper environment variables.  Useful if we need to remove them
# before executing something.
#
# Usage:
#
#   commandWrapperEnvironmentVariables
function commandWrapperEnvironmentVariables() {
    env | sed -n 's/\(COMMAND_WRAPPER_[^=]*\)=.*/\1/;T;p'
}

# Usage:
#
#   exec [-cl] [-a NAME] [COMMAND [ARGUMENTS ...]] [REDIRECTION ...]
#
# See "help exec" for more details.
function exec_() {
    export -n $(commandWrapperEnvironmentVariables)

    exec "$@"
}
