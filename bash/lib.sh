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
      *)          echo 3;;    # There is probably a bug, let's be safe and
                              # print the information.
    esac
}

# Usage:
#
#   useColours WHEN_TO_USE_COLOURS FILE_DESCRIPTOR
#
# Arguments:
#
#   WHEN_TO_USE_COLOURS is one of 'always', 'auto', and 'never'.
#
# Return value / exit code:
#
#   0 -- Use colours on specified file descriptor
#   1 -- Don't use colours on specified file descriptor
function useColours() {
    local when="$1"; shift
    local fd="$1"; shift

    function terminalSupportsColours() {
        if [[ -t "${fd}" && "$(tput colors)" -gt 0 ]]; then
            return 0
        else
            return 1
        fi
    }

    case "${when}" in
      'always') return 0;;
      'auto')   terminalSupportsColours;;
      'never')  return 1;;
      *)        terminalSupportsColours;;
    esac
}

# Usage:
#
#   msgf VERBOSITY MESSAGE_TYPE FORMAT [ARGUMENTS]
function msgf() {
    local -r verbosity="$(verbosityToNum "$1")"; shift
    local -r type="$1"; shift
    local -r format="$1"; shift

    # This function MUST work even in faulty Command Wrapper environment.
    # Otherwise we wouldn't be able to print any error messages about a bad
    # environment.

    function useColours_() {
        useColours "${COMMAND_WRAPPER_COLOUR:-auto}" 0
    }

    local -r resetColour='\033[0m'
    case "${type}" in
        'info')
            local -r messageType='Info'
            local -r -i typeNum=3
            ;;
        'notice')
            if useColours_; then
                local -r colour='\033[0;37m'
            fi
            local -r messageType='Notice'
            local -r -i typeNum=2
            ;;
        'warning')
            if useColours_; then
                local -r colour='\033[0;33m'
            fi
            local -r messageType='Warning'
            local -r -i typeNum=1
            ;;
        'error')
            if useColours_; then
                local -r colour='\033[0;31m'
            fi
            local -r messageType='Error'
            local -r -i typeNum=1
            ;;
    esac

    if (( typeNum <= verbosity )); then
        local -r cmd="${COMMAND_WRAPPER_NAME:+"${COMMAND_WRAPPER_NAME} "}${COMMAND_WRAPPER_SUBCOMMAND:-${0##*/}}"
        local -r fullFormat="${cmd}: ${messageType}: ${format}"

        printf "${colour}${fullFormat}${colour:+${resetColour}}\n" "$@"
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
    msgf "${COMMAND_WRAPPER_VERBOSITY:-normal}" 'warning' "$@" 1>&2
}

# Usage:
#
#   error FORMAT [ARGUMENTS]
function error() {
    msgf "${COMMAND_WRAPPER_VERBOSITY:-normal}" 'error' "$@" 1>&2
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

    if  [[ -z "${COMMAND_WRAPPER_VERSION}" ]]; then
        die 2 'COMMAND_WRAPPER_VERSION: %s: %s' \
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

    if  [[ -z "${COMMAND_WRAPPER_COLOUR}" ]]; then
        die 2 'COMMAND_WRAPPER_COLOUR: %s: %s' \
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
