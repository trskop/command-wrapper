-- vim: filetype=dhall

''
#!/usr/bin/env bash

# shellcheck shell=bash

# If you're using Nix then `nix-shell` can be used to call pinned version of
# Bash, and also provide necessary dependencies:
#
#!/usr/bin/env nix-shell
#!nix-shell -i bash
#!nix-shell [--packages PACKAGES|-p PACKAGES|PATH]
#...
#
# Somewhat useful documentation can be found in `nix-shell(1)` documentation
# (https://nixos.org/nix/manual/#sec-nix-shell).
#
# Some usage examples can be found on:
# https://trskop.github.io/articles/2019-12-05-nix-shell-shebang.html

# So called Bash strict mode.  See `bash(1)` documentation for more details.
set -eo pipefail

if (( BASH_VERSINFO[0] > 4 || ( BASH_VERSINFO[0] == 4 && BASH_VERSINFO[1] >= 4 ) )); then
    # Treat unset variables and parameters as an error when expanding.  This
    # wasn't very reliable in older Bash versions, hence the version check.
    set -u

    # Enable the behaviour of 'set -e' inside command substitution.  This
    # option is available since Bash 4.4.
    shopt -s inherit_errexit
fi

if [[ "''${COMMAND_WRAPPER_VERBOSITY:-}" = 'annoying' ]]; then
    # This will cause Bash to print commands before executing them.
    set -x
fi

# Import Command Wrapper Bash library
#
# Environment variables `COMMAND_WRAPPER_NAME` and `COMMAND_WRAPPER_EXE` are
# provided when by Command Wrapper when subcommand is executed.  For more
# information see `command-wrapper-subcommand-protocol(7)` manual page.
#
# shellcheck source=/dev/null
source <(
    COMMAND_WRAPPER_INVOKE_AS="''${COMMAND_WRAPPER_NAME}" "''${COMMAND_WRAPPER_EXE}" \
        completion --library --shell=bash --content
)

# Print help message to standard output.
#
# Usage:
#
#   printHelp
#   printHelp 1>&2
#
# When subcommand is invoked with `--help|-h` option then help information
# needs to be printed to standard output.  If help message is printed as part
# of error message, then it needs to be printed to standard error output.  This
# means that redirection will work the way user expects.  For example:
#
# *   Redirecting help message, e.g. `TOOLSET_COMMAND help SUBCOMMAND | less`
#     will work as expected.
#
# *   Using subcommand in a pipeline.  E.g.
#     `TOOLSET_COMMAND SUBCOMMAND [ARGUMENT [...]] | SOME_COMMAND` will not
#     pass help message as an input to `SOME_COMMAND` if there was an error.
function printHelp() {
    local -r command="''${COMMAND_WRAPPER_NAME} ''${COMMAND_WRAPPER_SUBCOMMAND}"

    # Compose a help command.  If manual page is available then pass '--man'
    # option.
    #
    # Usage:
    #
    #   helpCommand [--man]
    function helpCommand() {
        local -r prefix="''${COMMAND_WRAPPER_NAME} help"

        local opts='''
        if (( $# )); then
            opts="$1"; shift
        fi

        echo "''${prefix} ''${opts:+''${opts} }''${COMMAND_WRAPPER_SUBCOMMAND}"
    }

    # Some command line options that are part of Command Wrapper Subcommand
    # Protocol (`command-wrapper-subcommand-proticol(7)`), like
    # `--completion-info` and `--completion` are considered internal, and
    # should not be presented to the user.
    cat <<EOF
TODO: Hereby I promise to describe this subcommand one day.

Usage:

  ''${command}
  ''${command} {--help|-h}
  $(helpCommand --man)

Options:

  --help, -h
      Print short help message and exit.  Same as: $(helpCommand)
EOF
}

# Print Dhall expression that describes how this subcommand should be invoked
# when performing command line completion.  See
# `command-wrapper-subcommand-protocol(7)` for more details on how this works.
#
# Usage:
#
#   completionInfo [--hash]
#
# Options:
#
#   --hash
#       Print semantic hash of Dhall expressions instead of the Dhall
#       expression itself.
function completionInfo() {
    # Function stdCompletionInfo is provided by Command Wrapper Bash library
    # and it prints Dhall expression that corresponds to standard Command
    # Wrapper completion calling convention:
    #
    # COMMAND --completion --index=INDEX --shell=SHELL -- [WORD ...]
    #
    # Calling convention has to be in sync with what `completion` function
    # accepts.  You may need to specify your own Dhall expression if there's a
    # mismatch.
    stdCompletionInfo "$@"
}

# Perform command line completion.
#
# Usage:
#
#   completion [--index=INDEX] [--shell=SHELL] [[--] WORD [...]]
#
# Bash itself provides `compgen` builtin command that can do a lot of standard
# command line completion functions.  See `bash(1)` or `help compgen` for more
# details.
#
# Make sure that Dhall expression provided by `completionInfo` is compatible
# with this function.
function completion() {
    local index=-1
    local -a words=

    local arg
    while (( $# )); do
        arg="$1"; shift
        case "''${arg}" in
            --index=*)
                index="''${arg#*=}"
                ;;
            --shell=*)
                # Ignored.
                ;;
            --)
                words=("$@")
                break
                ;;
            -*)
                die 1 "'%s': Unknown completion option." "''${arg}"
                ;;
            *)
                words=("''${arg}" "$@")
                break
                ;;
        esac
    done

    if (( ! ''${#words[@]} )); then
        words=("")
    fi

    local -r currentWord="''${words[''${index}]}"

    local -i hadDashDash=0
    local -a previousWords=()
    if (( index )); then
        previousWords=("''${words[@]:0:$((index - 1))}")
    fi

    for opt in "''${previousWords[@]}"; do
        if [[ "''${opt}" = '--' ]]; then
            hadDashDash=1
            break
        fi
    done

    local -r -a options=(
        --help -h
        #--init-config
    )

    if (( ! hadDashDash )) && [[ "''${currentWord}" == -* ]]; then
        compgen -W "''${options[*]}" -- "''${currentWord}"
    else
        # File completion.
        compgen -f -- "''${currentWord}"
    fi
}

# Read configuration file in Dhall format and declare Bash variables based on
# what's in it.  Subcommand configuration files are required to be in Dhall
# format, however, if you are more comfortable with JSON or YAML then there
# are tools to convert Dhall into them:
#
# * 'dhall-to-json'
# * 'dhall-to-yaml'
#
# Best approach in such case would be to create a temporary file with
# JSON/YAML content that is then used as a configuration file instead.
#
# Note that Subcommands aren't required to use configuration files.  If you
# don't need to use it then delete this function and relevant code in the
# 'main' function.
#
# This function is very simple example of how configuration can be read.  It
# expects configuration to be a record where individual fields are primitive
# values in Bash sense.  See documentation of Command Wrapper's `config`
# command, specifically `--dhall-bash` functionality.
#
# Usage:
#
#   declareCfg ATTRIBUTE_NAME
#
# This function uses Dhall expression in `''${COMMAND_WRAPPER_CONFIG}`
# environment variable to access `ATTRIBUTE_NAME` and delcare it.
function declareCfg() {
    local -r name="$1"; shift

     dhall-to-bash \
        --declare="''${name}" \
        --expression="(env:COMMAND_WRAPPER_CONFIG).\`''${name}\`"
}

# Main entry point.
#
# Usage:
#   main {-h|--help}
#   main --completion-info[-hash]
#   main --completion [--index=INDEX] [--shell=SHELL] [[--] WORD [...]]
#   main [OPTION [...]] [ARGUMENT [...]]
function main() {
    # This function makes sure that this subcommand was executed using
    # expected calling convention.  If it wasn't it will terminate this
    # script with appropriate error message.  It is hard to guarantee
    # anything in case of it being executed in any other way.  Mentioned
    # calling convention is described in a dedicated manual page:
    #
    #   command-wrapper-subcommand-protocol(7)
    dieIfExecutedOutsideOfCommandWrapperEnvironment

    # It is up to subcommand to decide what command line options should be
    # supported, and how they should be parsed, but each subcommand has to
    # support following:
    #
    # * `--help`, `-h`
    # * `--completion-info`
    #
    # In addition to the above following command line options are reserved for
    # future use by the Subcommand Protocol:
    #
    # *   `--completion-info-hash`
    # *   `--config-constructor`
    # *   `--config-constructor-hash`
    #
    # Subcommands are encouraged to also support:
    #
    # *   `--init-config` -- It's name is not mandated either.  Purpose of this
    #     option is to print initial configuration file to standard output.
    #
    # See Command Wrapper's Subcommand Protocol for more information.  It is
    # available in the form of command-wrapper-subcommand-protocol(7) manual
    # page.
    local arg
    while (( $# )); do
        arg="$1"; shift
        case "''${arg}" in
            # Supporting '--help' is required by the Subcommand Protocol.
            --help|-h)
                # Printing help to standard output and using exit code 0 is
                # required by Subcommand Protocol.
                printHelp
                exit 0
                ;;

            # Supporting '--completion-info' is required by the Subcommand
            # Protocol.
            --completion-info)
                completionInfo
                exit 0
                ;;

            # Option reserved by Subcommand Protocol for future use.
            --completion-info-hash)
                completionInfo --hash
                exit 0
                ;;

            # Option reserved by Subcommand Protocol for future use.
            --config-constructor)
                die 1 "'%s': Unknown option." "''${arg}"
                #configConstructor
                #exit 0
                ;;

            # Option reserved by Subcommand Protocol for future use.
            --config-constructor-hash)
                die 1 "'%s': Unknown option." "''${arg}"
                #configConstructor --hash
                #exit 0
                ;;

            # Implementation of command line completion.  It is up to
            # individual subcommands to decide how this should be implemented,
            # however the implementation of '--completion-info' has to print
            # Dhall expression that is consistent with the chosen approach.
            #
            # Please make sure that `completionInfo` and `completion` functions
            # are defined in compatible way.  See
            # `command-wrapper-subcommand-protocol(7)` for more details on how
            # completion works.
            --completion)
                completion "$@"
                exit 0
                ;;

            # Following is not mandated by subcommand protocol, but it's a good
            # idea to have it if the subcommand cannot work without a
            # configuration file.  Subcommands are not aware of the location of
            # their configuration file, therefore, it is best to just print it
            # to standard output and point user to `command-wrapper(1)`
            # documentation.
            #
            # This option, or something similar, can be useful even if
            # subcommand can operate without configuration file.  Notably, when
            # subcommand is installed using Nix, option like this can be used
            # to generate configuration file during installation.
            #
            # Be aware that there is no skeleton provided for
            # `constructInitialConfiguration`.  It has to be written from
            # scratch.
            #--init-config)
            #    constructInitialConfiguration
            #    exit 0
            #    ;;

            # TODO: Define additional options here.
            -*)
                # Using exit code 1 here is required by Subcommand Protocol.
                die 1 "'%s': Unknown option." "''${arg}"
                ;;

            # TODO: Define arguments here.
            *)
                # Using exit code 1 here is required by Subcommand Protocol.
                die 1 "'%s': Too many arguments." "''${arg}"
                ;;
        esac
    done

    # Subcommands aren't required to use configuration files.  If you don't
    # need it then just delete or comment-out configuration related code.
    #
    # When subcommand is using configuration file and that doesn't exist it
    # is allowed to do one of the following:
    #
    # 1. Use hardcoded defaults.
    # 2. Fail with error message indicating that the configuration file is
    #    missing.
    #
    # See command-wrapper-subcommand-protocol(7) manual page for more
    # information.
    #
    # Following code assumes that the subcommand uses the former option (the
    # one marked as 1.).  Code that doesn't allow subcommand to work without a
    # configuration file is commented out further down.
    if haveConfiguration; then
        eval "$(declareCfg 'someValue')"
    else
        # Provide default configuration values here.
        #
        # At the moment the variable is not used, this will disable shellcheck
        # warning for it:
        #
        # shellcheck disable=2034
        declare -r someValue="this is the hardcoded default"
    fi

    # Following code assumes that subcommand requires configuration file to
    # exist.
    #haveConfiguration --or-die

    # An alternative to the above with custom error message:
    #haveConfiguration --or-die '%s %s' \
    #    "Configuration file is required and it's missing." \
    #    "Use '--init-config' to create initial configuration file."

    # TODO: Implement me!  Here is the place where the real functionality
    # should be.
}

main "$@"
''
