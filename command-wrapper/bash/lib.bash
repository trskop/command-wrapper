# shellcheck shell=bash

# Library for writing CommandWrapper subcommands in Bash.
#
# Copyright (c) 2018-2020, Peter Trsko
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
# Message is by default printed to stderr.
function info() {
    msgf "${COMMAND_WRAPPER_VERBOSITY:-normal}" 'info' "$@" 1>&2
}

# Print notice-level message.  Printed only if verbosity is 'verbose' or higher.
#
# Usage:
#
#   notice FORMAT [ARGUMENTS]
#
# Message is by default printed to stderr.
function notice() {
    msgf "${COMMAND_WRAPPER_VERBOSITY:-normal}" 'notice' "$@" 1>&2
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

# Compare two version numbers (sequence of dot separated digits).
#
# Usage:
#
#   testVersion VERSION OPERATION VERSION
#
# Arguments
#
#   VERSION
#     Version number, i.e. sequence of dot-separated digits.  For example
#     '1.0.1' or '0.123.01'.
#
#   OPERATION
#     One of '<', '-lt', '>', '-gt', '<=', '-le', '>=', '-ge', '==', '-eq',
#     '!=', and '-ne'.
function testVersion() {
    local -r lhsString="$1"; shift
    local -r op="$1"; shift
    local -r rhsString="$1"; shift

    local -i ret=0
    if [[ "${lhsString}" != "${rhsString}" ]]; then
        local -a lhs
        # shellcheck disable=2206
        IFS='.' lhs=(${lhsString})

        local -a rhs
        # shellcheck disable=2206
        IFS='.' rhs=(${rhsString})

        if (( ${#lhs[@]} < ${#rhs[@]} )); then
            for (( i=${#lhs[@]}; i < ${#rhs[@]}; i++ )); do
                lhs[$i]=0
            done
        else
            for (( i=${#rhs[@]}; i < ${#lhs[@]}; i++ )); do
                rhs[$i]=0
            done
        fi

        for (( i=0; i < ${#lhs[@]}; i++ )); do
            if (( 10#${lhs[$i]} == 10#${rhs[$i]} )); then
                # Inconclusive, we need to test further.
                continue;
            elif (( 10#${lhs[$i]} < 10#${rhs[$i]} )); then
                ret=-1
                break;
            elif (( 10#${lhs[$i]} > 10#${rhs[$i]} )); then
                ret=1
                break;
            fi
        done
    fi

    case "${op}" in
        '<'|'-lt')
            (( ret < 0 ))
            ;;
        '>'|'-gt')
            (( ret > 0 ))
            ;;
        '<='|'-le')
            (( ret <= 0 ))
            ;;
        '>='|'-ge')
            (( ret >= 0 ))
            ;;
        '=='|'-eq')
            (( ret == 0 ))
            ;;
        '!='|'-ne')
            (( ret != 0 ))
            ;;
        *)
            error '%s: Unexpected operator' "${op}"
            return 2
            ;;
    esac
}

# Test that version is greater or equal than a specified version bound
# (VERSION >= MIN_BOUND_VERSION).
#
# Usage:
#
#   testVersionMinBound VERSION MIN_BOUND_VERSION
#
# Examples:
#
#   testVersionMinBound 0.1 0.2 # $? = 1 (false)
#   testVersionMinBound 0.3 0.2 # $? = 0 (true)
function testVersionMinBound() {
    local -r version="$1"; shift
    local -r minVersion="$1"; shift

    testVersion "${version}" -ge "${minVersion}"
}

# Check that the Command Wrapper environment variables, as defined in
# Subcommand Protocol, were passed to us.  If any of them is missing then this
# this function will terminate the the subcommand with exit code 2, and
# appropriate error message.
#
# Usage:
#
#   dieIfExecutedOutsideOfCommandWrapperEnvironment [MIN_VERSION]
#
# Arguments:
#
#   MIN_VERSION
#       If specified then we check that the Command Wrapper Subcommand Protocol
#       version is either MIN_VERSION or later.
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
    if (( $# )); then
        local -r minVersion="$1"; shift
    else
        local -r minVersion="1.0.0"  # Initial version of subcommand protocol.
    fi
    if (( $# )); then
        local -r maxVersion="$1"; shift
    else
        local -r maxVersion='1.0.1'
    fi

    if  [[ -z "${COMMAND_WRAPPER_EXE}" ]]; then
        die 2 'COMMAND_WRAPPER_EXE: %s: %s' \
            'Missing environment variable' \
            'This command must be executed inside command-wrapper environment.'
    fi

    if  [[ -z "${COMMAND_WRAPPER_VERSION}" ]]; then
        die 2 'COMMAND_WRAPPER_VERSION: %s: %s' \
            'Missing environment variable' \
            'This command must be executed inside command-wrapper environment.'
    elif testVersion "${COMMAND_WRAPPER_VERSION}" -ge "${minVersion}" \
        && testVersion "${COMMAND_WRAPPER_VERSION}" -lt "${maxVersion}"
    then
        die 2 'COMMAND_WRAPPER_VERSION: %s: %s. %s: %s.' \
            "${COMMAND_WRAPPER_VERSION}" \
            'Unsupported Subcommand protocol version by this subcommand' \
            'Minimum required version is' "${minVersion}"
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

    # We are checking if COMMAND_WRAPPER_CONFIG variable is set or not, which
    # is a different beast from it being empty.
    if  [[ -z "${COMMAND_WRAPPER_CONFIG+variable-is-set}" ]]; then
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

# List Command Wrapper environment variables that are part of the Subcommand
# Protocol.  Useful if we need to remove them before executing something.
#
# Usage:
#
#   commandWrapperEnvironmentVariables
#
# See `command-wrapper-subcommand-protocol(7)` for more information.
function commandWrapperEnvironmentVariables() {
    env | sed -n '
        s/\(COMMAND_WRAPPER_\(EXE\|VERSION\|NAME\)*\)=.*/\1/;
        s/\(COMMAND_WRAPPER_\(SUBCOMMAND\|CONFIG\)*\)=.*/\1/;
        s/\(COMMAND_WRAPPER_\(VERBOSITY\|COLOUR\)*\)=.*/\1/;
        T;
        p
    '
}

# Uses 'commandWrapperEnvironmentVariables' to list Command Wrapper environment
# variables and then unsets them.  Useful if we need to remove them before
# executing something.
#
# Usage:
#
#   removeCommandWrapperEnvironmentVariables
#
# Be aware that once the environment variables are removed the subcommand has
# no way to access information provided by the Command Wrapper.  See `exec_`
# function, which uses `removeCommandWrapperEnvironmentVariables`.
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
#   exec_ [-cl] [-a NAME] [COMMAND [ARGUMENTS ...]] [REDIRECTION ...]
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
            --verbosity="${COMMAND_WRAPPER_VERBOSITY}" \
            --colour="${COMMAND_WRAPPER_COLOUR}" \
            --no-aliases \
            "$@"
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
    toolset config --dhall "$@"
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
    toolset config --dhall-filter "$@"
}

# Format Dhall expression.
#
# Usage:
#
#   dhall-format
#     [--expression=EXPRESSION|--expression EXPRESSION|--input=FILE|--input FILE|-i FILE]
#     [--output=FILE|--output FILE|-o FILE]
#
# See `TOOLSET help [--man] config` for more details.
function dhall-format() {
    toolset config --dhall-format "$@"
}

# Add integrity checks to import statements of a Dhall expression.
#
# Usage:
#
#   dhall-freeze
#     [--[no-]remote-only]
#     [--for-security|--for-caching]
#     [--expression=EXPRESSION|--expression EXPRESSION|--input=FILE|--input FILE|-i FILE]
#     [--output=FILE|--output FILE|-o FILE]
#
# See `TOOLSET help [--man] config` for more details.
function dhall-freeze() {
    toolset config --dhall-freeze "$@"
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
    toolset config --dhall-hash "$@"
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
    toolset config --dhall-bash "$@"
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
    toolset config --dhall-text "$@"
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
    toolset config --dhall-exec "$@"
}

# Display selection menu.  Selected value is printed to standard output.
#
# Usage:
#
#   edit-file [FILE|--subcommand-config SUBCOMMAND]
#
# See `TOOLSET help [--man] config` for more details.
function edit-file() {
    toolset config --edit "$@"
}

# Display selection menu.  Selected value is printed to standard output.
#
# Usage:
#
#   select-menu [--input=FILE|--arguments [STRING ...]]
#
# See `TOOLSET help [--man] config` for more details.
function select-menu() {
    toolset --no-aliases config --menu "$@"
}

# Primitive completion queries for when we don't want to use Bash's `compgen`,
# but Command Wrapper's `completion --query` functionality.
#
# Usage:
#
#   completion-query {--subcommands|--subcommand-aliases}
#     [--algorithm=ALGORITHM] [--pattern=PATTERN] [--prefix=STRING]
#     [--suffix=STRING] [--output=FILE]
#
#   completion-query {--supported-shells|--verbosity-values|--colo[u]r-values}
#     [--algorithm=ALGORITHM] [--pattern=PATTERN] [--prefix=STRING]
#     [--suffix=STRING] [--output=FILE]
#
#   completion-query --file-system=TYPE
#     [--[no-]tilde-expansion] [--[no-]substitute-tilde]
#     [--algorithm=ALGORITHM] [--pattern=PATTERN] [--prefix=STRING]
#     [--suffix=STRING] [--output=FILE]
#
#   completion-query --words
#     [--algorithm=ALGORITHM] [--pattern=PATTERN] [--prefix=STRING]
#     [--suffix=STRING] [--output=FILE]
#     [--] [WORD ...]
#
# See `TOOLSET help [--man] completion` for more details.
function completion-query() {
    toolset completion --query "$@"
}

# Print Dhall expression that describes how this subcommand should be invoked
# when performing command line completion.  Function printed by this command
# uses invocation syntax standard to Command Wrapper tools, which is:
#
#   --completion --index=INDEX --shell=SHELL -- [WORD ...]
#
# Usage:
#
#   stdCompletionInfo [--hash]
#
# Options:
#
#   --hash
#       Print semantic hash of Dhall expressions instead of the Dhall
#       expression itself.
#
# See `command-wrapper-subcommand-protocol(7)` for more details on how this
# works.
function stdCompletionInfo() {
    local -r hash='sha256:50721655961fc58a6a66435ac062939e5038cb68955c041fc6e8ca52b5d7ac21'
    local -r expression=$'
  λ(shell : < Bash | Fish | Zsh >)
→ λ(index : Natural)
→ λ(words : List Text)
→   [ "--completion"
    , "--index=${Natural/show index}"
    , "--shell=${merge {Bash = "bash", Fish = "fish", Zsh = "zsh"} shell}"
    , "--"
    ]
  # words
'

    if [[ $# -gt 0 && "$1" = '--hash' ]]; then
        echo "${hash}"
    else
        cat <<< "${expression}"
    fi
}

# Test if configuration is available or not.
#
# Usage:
#
#   haveConfiguration [--or-die [FORMAT [ARGUMENTS]]]
#
# Options:
#
#   --or-die [FORMAT [ARGUMENTS]]
#       Instead of behaving as a predicate perform a check, and if
#       configuration is not available die with exit status `1` (mandated by
#       Subcommand Protocol).
#
#       If `FORMAT [ARGUMENTS]` is specified then it overrides the default
#       error message.  The syntax is the same as for `printf` command.
#
# Usage:
#
#    ```Bash
#    # ...
#    if haveConfiguration then
#        # Use `dhall-to-bash` to access configuration.
#        :
#    else
#        # Use default values instead.
#        :
#    fi
#    ```
#
#    ```Bash
#    haveConfiguration --or-die '%s %s' \
#      "Configuration file is required and it's missing." \
#      "Use '--init-config' to create initial configuration file."
#    ```
function haveConfiguration() {
    local die=0
    local -a arguments=()

    local arg
    while (( $# )); do
        arg="$1"; shift

        case "${arg}" in
            --or-die)
                die=1
                arguments=("$@")
                break;
                ;;
            *)
                # Explicitly itnored.
                break;
                ;;
        esac
    done

    # Subcommand Protocol dictates taht `COMMAND_WRAPPER_CONFIG` environment
    # variable must be set, but it may be empty.  When it's empty it indicates
    # that Command Wrapper failed to find a configuration file for the
    # subcommand.
    if [[ -n "${COMMAND_WRAPPER_CONFIG}" ]]; then
        return 0
    elif (( die )); then
        # Be aware that exit status `1` is required by Subcommand Protocol.
        if (( ${#arguments[@]} )); then
            die 1 "${arguments[@]}"
        else
            die 1 "Configuration file required and it's missing."
        fi
    else
        return 1
    fi
}
