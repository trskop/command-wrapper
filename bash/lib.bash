# shellcheck shell=bash

# Library for writing CommandWrapper subcommands in Bash.
#
# Copyright (c) 2018-2019, Peter Trsko
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

# Convert VERBOSITY value into an integer where `silent = 0`, and
# `annoying = 3`.  Numeric values are much easier to compare, and order, in
# Bash scripts.
#
# Usage:
#
#   verbosityToNum VERBOSITY
#
# Arguments:
#
#   VERBOSITY is one of 'silent', 'normal', 'verbose', 'annoying', and a number
#     in between 0 (including) and 3 (including).  If this function is unable
#     to parse VERBOSITY then 'annoying' is assumed.  This is to make sure that
#     the user will see error messages in case of faulty environment/script.
#
# Return value:
#
#   0 - Always
#
# Stdout:
#
#   Number in between '0' (including) and '3' (including).
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

# Determine if colourised output should be produced.
#
# Usage:
#
#   useColours WHEN_TO_USE_COLOURS FILE_DESCRIPTOR
#
# Arguments:
#
#   WHEN_TO_USE_COLOURS is one of 'always', 'auto', and 'never'.
#
#   FILE_DESCRIPTOR is an open file descriptor that may or may not be attached
#     to a terminal.  Standard values are 1 for stdout, and 2 for stderr.
#
# Return value / exit code:
#
#   0 -- Use colours on specified file descriptor
#   1 -- Don't use colours on specified file descriptor
#
# Usage example:
#
#   useColours "${COMMAND_WRAPPER_COLOUR}" 1
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

# Low-level function for printing out messages.
#
# Usage:
#
#   msgf VERBOSITY MESSAGE_TYPE FORMAT [ARGUMENTS]
#
# Arguments:
#
#   VERBOSITY
#     Can be one of 'silent', 'normal', 'verbose', and 'annoying'.  Value
#     of ${COMMAND_WRAPPER_VERBOSITY} should be used when available.
#
#   MESSAGE_TYPE
#     Can be one of 'info', 'notice', 'output', 'warning', and 'error'.
function msgf() {
    local -r verbosity="$(verbosityToNum "$1")"; shift
    local -r type="$1"; shift
    local -r format="$1"; shift

    # This function MUST work even in faulty Command Wrapper environment.
    # Otherwise we wouldn't be able to print any error messages about a bad
    # environment.

    function useColours_() {
        useColours "${COMMAND_WRAPPER_COLOUR:-auto}" 1
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
        'output')
            local -r messageType=''
            local -r -i typeNum=1
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
        local -r fullFormat="${cmd}: ${messageType}${messageType:+: }${format}"

        # shellcheck disable=SC2059
        printf "${colour:-}${fullFormat}${colour:+${resetColour}}\n" "$@"
    fi
}

# Print info-level message.  Printed only if verbosity is set to 'annoying'.
#
# Usage:
#
#   info FORMAT [ARGUMENTS]
#
# Message is by default printed to stdout.
function info() {
    msgf "${COMMAND_WRAPPER_VERBOSITY:-normal}" 'info' "$@"
}

# Print notice-level message.  Printed only if verbosity is 'verbose' or higher.
#
# Usage:
#
#   notice FORMAT [ARGUMENTS]
#
# Message is by default printed to stdout.
function notice() {
    msgf "${COMMAND_WRAPPER_VERBOSITY:-normal}" 'notice' "$@"
}

# Print normal output message.  Supressed when verbosity is set to 'silent'.
#
# Usage:
#
#   out FORMAT [ARGUMENTS]
#
# Message is by default printed to stdout.
function out() {
    msgf "${COMMAND_WRAPPER_VERBOSITY:-normal}" 'output' "$@"
}

# Print warning message.  Printed only if verbosity is 'normal' or higher.
#
# Usage:
#
#   warn FORMAT [ARGUMENTS]
#
# Message is by default printed to stderr.
function warn() {
    msgf "${COMMAND_WRAPPER_VERBOSITY:-normal}" 'warning' "$@" 1>&2
}

# Print error message.  Printed only if verbosity is 'normal' or higher.
#
# Usage:
#
#   error FORMAT [ARGUMENTS]
#
# Message is by default printed to stderr.
function error() {
    msgf "${COMMAND_WRAPPER_VERBOSITY:-normal}" 'error' "$@" 1>&2
}

# Like `error` function, but calls `exit EXIT_CODE` afterwards.
#
# Usage:
#
#   die EXIT_CODE FORMAT [ARGUMENTS]
#
# Arguments:
#
#   EXIT_CODE, terminate script with this exit code after printing the error.
function die() {
    local -r exitCode="$1"; shift

    error "$@"
    exit "${exitCode}"
}

# Check that the Command Wrapper environment variables, as defined in
# Subcommand Protocol, were passed to us.  If any of them is missing then this
# this function will terminate the the subcommand with exit code 2, and
# appropriate error message.
#
# Usage:
#
#   dieIfExecutedOutsideOfCommandWrapperEnvironment
#
# Return value:
#
#   0 - Always, on failure it calls `exit 2`, i.e. terminates the script.
#
# See Also
#
#   Command Wrapper's Subcommand Protocol command-wrapper-subcommand-protocol(7)
#   for more details.
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

    if  [[ -z "${COMMAND_WRAPPER_SUBCOMMAND}" ]]; then
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

# Uses 'commandWrapperEnvironmentVariables' to list Command Wrapper environment
# variables and then unsets them.  Useful if we need to remove them before
# executing something.
#
# Usage:
#
#   removeCommandWrapperEnvironmentVariables
function removeCommandWrapperEnvironmentVariables() {
    local -a vars=()
    mapfile -t vars < <(commandWrapperEnvironmentVariables)
    export -n "${vars[@]}"
    unset vars
}

# Execute a specified command, but with Command Wrapper environment variables
# removed from its environment.
#
# Usage:
#
#   exec [-cl] [-a NAME] [COMMAND [ARGUMENTS ...]] [REDIRECTION ...]
#
# See also:
#
#   * Function `commandWrapperEnvironmentVariables`.
#   * Run "help exec" for more details about Bash's exec.
function exec_() {
    removeCommandWrapperEnvironmentVariables
    exec "$@"
}

# Run current toolset command.
#
# Usage
#
#   toolset [GLOBAL_OPTIONS] SUBCOMMAND [--] [SUBCOMMAND_ARGUMENTS]
#   toolset {--help|-h}
#   toolset {--version|-V}
#
# See `command-wrapper(1)` for details.
function toolset() {
    # Reason for using '--no-aliases' is to prevent aliases interfering with
    # what subcommand script expects.
    COMMAND_WRAPPER_INVOKE_AS="${COMMAND_WRAPPER_NAME}" \
        "${COMMAND_WRAPPER_EXE}" \
        --no-aliases "$@"
}

# Dhall interpreter.
#
# Usage:
#
#   dhall
#     [--[no-]allow-imports] [--[no-]cache]
#     [--[no-]annotate] [--[no-]alpha] [--[no-]type]
#     [--let=NAME=EXPRESSION]
#     [--expression=EXPRESSION|--expression EXPRESSION|--input=FILE|--input FILE|-i FILE]
#     [--output=FILE|--output FILE|-o FILE]
#
# See `TOOLSET help [--man] config` for more details.
function dhall() {
    # Reason for using '--no-aliases' is to prevent aliases interfering with
    # what subcommand script expects.
    #
    # We aren't passing `COMMAND_WRAPPER_INVOKE_AS` to avoid dependency on
    # specific toolset configuration.
    "${COMMAND_WRAPPER_EXE}" --no-aliases config --dhall "$@"
}

# Dhall interpreter that puts Dhall input expression into the scope of
# EXPRESSION as "input : Input".
#
# Usage:
#
#   dhall-filter
#     [--[no-]allow-imports] [--[no-]cache]
#     [--let=NAME=EXPRESSION]
#     [--expression=EXPRESSION|--expression EXPRESSION|--input=FILE|--input FILE|-i FILE]
#     [--output=FILE|--output FILE|-o FILE]
#     EXPRESSION
#
# See `TOOLSET help [--man] config` for more details.
function dhall-filter() {
    # Reason for using '--no-aliases' is to prevent aliases interfering with
    # what subcommand script expects.
    #
    # We aren't passing `COMMAND_WRAPPER_INVOKE_AS` to avoid dependency on
    # specific toolset configuration.
    "${COMMAND_WRAPPER_EXE}" --no-aliases config --dhall-filter "$@"
}

# Format Dhall expression.
#
# Usage:
#
#   dhall-format
#
# See `TOOLSET help [--man] config` for more details.
function dhall-format() {
    # Reason for using '--no-aliases' is to prevent aliases interfering with
    # what subcommand script expects.
    #
    # We aren't passing `COMMAND_WRAPPER_INVOKE_AS` to avoid dependency on
    # specific toolset configuration.
    "${COMMAND_WRAPPER_EXE}" --no-aliases config --dhall-format "$@"
}

# Add integrity checks to import statements of a Dhall expression.
#
# Usage:
#   dhall-freeze
#     [--[no-]remote-only]
#     [--for-security|--for-caching]
#     [--expression=EXPRESSION|--expression EXPRESSION|--input=FILE|--input FILE|-i FILE]
#     [--output=FILE|--output FILE|-o FILE]
#
# See `TOOLSET help [--man] config` for more details.
function dhall-freeze() {
    # Reason for using '--no-aliases' is to prevent aliases interfering with
    # what subcommand script expects.
    #
    # We aren't passing `COMMAND_WRAPPER_INVOKE_AS` to avoid dependency on
    # specific toolset configuration.
    "${COMMAND_WRAPPER_EXE}" --no-aliases config --dhall-freeze "$@"
}

# Compute semantic hashes for Dhall expressions.
#
# Usage:
#
#   dhall-hash
#     [--[no-]cache]
#     [--expression=EXPRESSION|--expression EXPRESSION|--input=FILE|--input FILE|-i FILE]
#     [--output=FILE|--output FILE|-o FILE]
#
# See `TOOLSET help [--man] config` for more details.
function dhall-hash() {
    # Reason for using '--no-aliases' is to prevent aliases interfering with
    # what subcommand script expects.
    #
    # We aren't passing `COMMAND_WRAPPER_INVOKE_AS` to avoid dependency on
    # specific toolset configuration.
    "${COMMAND_WRAPPER_EXE}" --no-aliases config --dhall-hash "$@"
}

# Compile Dhall expression into Bash expression or statement.
#
# Usage:
#
#   dhall-to-bash
#     [--[no-]allow-imports] [--[no-]cache]
#     [--declare=NAME]
#     [--expression=EXPRESSION|--expression EXPRESSION|--input=FILE|--input FILE|-i FILE]
#     [--output=FILE|--output FILE|-o FILE]
#
# See `TOOLSET help [--man] config` for more details.
function dhall-to-bash() {
    # Reason for using '--no-aliases' is to prevent aliases interfering with
    # what subcommand script expects.
    #
    # We aren't passing `COMMAND_WRAPPER_INVOKE_AS` to avoid dependency on
    # specific toolset configuration.
    "${COMMAND_WRAPPER_EXE}" --no-aliases config --dhall-bash "$@"
}

# Compile Dhall expression into Bash expression or statement.
#
# Usage:
#
#   dhall-to-text
#     [--[no-]allow-imports] [--[no-]cache]
#     [--list]
#     [--expression=EXPRESSION|--expression EXPRESSION|--input=FILE|--input FILE|-i FILE]
#     [--output=FILE|--output FILE|-o FILE]
#
# See `TOOLSET help [--man] config` for more details.
function dhall-to-text() {
    # Reason for using '--no-aliases' is to prevent aliases interfering with
    # what subcommand script expects.
    #
    # We aren't passing `COMMAND_WRAPPER_INVOKE_AS` to avoid dependency on
    # specific toolset configuration.
    "${COMMAND_WRAPPER_EXE}" --no-aliases config --dhall-text "$@"
}

# Render Dhall expression as Text and execute the result.
#
# Usage:
#
#   dhall-exec
#     [--expression=EXPRESSION|--expression EXPRESSION|--input=FILE|--input FILE|-i FILE]
#     [--interpreter=COMMAND [--interpreter-argument=ARGUMENT ...]]
#     [ARGUMENT ...]
#
# See `TOOLSET help [--man] config` for more details.
function dhall-exec() {
    # Reason for using '--no-aliases' is to prevent aliases interfering with
    # what subcommand script expects.
    #
    # We aren't passing `COMMAND_WRAPPER_INVOKE_AS` to avoid dependency on
    # specific toolset configuration.
    "${COMMAND_WRAPPER_EXE}" --no-aliases config --dhall-exec "$@"
}

# Print Dhall expression that describes how this subcommand should be invoked
# when performing command line completion.  Function printed by this command
# uses invocation syntax standard to Command Wrapper tools, which is:
#
#   --completion --index=INDEX --shell=SHELL -- [WORD ...]
#
# Usage:
#
#   stdCompletionInfo
#
# See `command-wrapper-subcommand-protocol(7)` for more details on how this
# works.
function stdCompletionInfo() {
    cat <<'EOF'
  λ(shell : < Bash | Fish | Zsh >)
→ λ(index : Natural)
→ λ(words : List Text)
→   [ "--completion"
    , "--index=${Natural/show index}"
    , "--shell=${merge {Bash = "bash", Fish = "fish", Zsh = "zsh"} shell}"
    , "--"
    ]
  # words
EOF
}
