# shellcheck shell=bash
# vim: filetype=direnv

# While this library is self documenting a lot more information and usage
# examples can be found in `command-wrapper-direnv-library(7) manual` page.  It
# can be accessed by calling:
#
# ```
# "${TOOLSET}" help --man direnv-library
# ```

# Copyright (c) 2020, Peter Trsko
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

# Same as Direnv's `watch_file`, but allows to specify multiple file paths at
# the same time.
#
# Usage:
#
#   watch_files [FILE ...]
#
# Arguments:
#
#   FILE
#       File path of a file which should be watched by Direnv for changes.  If
#       its contents change Direnv will reload the environment.
function watch_files() {
    local -a -r files="$@"

    for file in "${files[@]}"; do
        watch_file "${file}"
    done
}

# Get the value of Command Wrapper cache directory for given toolset and Direnv
# purposes.
#
# Usage:
#
#   command_wrapper_cache_dir TOOLSET_NAME
#
# Arguments:
#
#   TOOLSET_NAME
#       Name of the toolset we setting up environment for.
#
# Q: Why not use `direnv_layout_dir`?
# A: There's a lot of stuff that can be safely shared beteween invocations and
#    Direnv environments when content hash is part of the generated file name.
function command_wrapper_cache_dir() {
    local -r toolset="$1"; shift

    # This is where we'll store generated files.  Operations in this directory
    # have to be performed with concurrent access in mind.
    local -r cache_dir="${XDG_CACHE_HOME:-${HOME}/.cache}/${toolset}/direnv"
    mkdir -p "${cache_dir}" || {
        if (( ! ${DIRENV_IN_ENVRC:-0} )); then
            DIRENV_LOG_FORMAT="direnv: ${FUNCNAME[0]}: %s" log_error \
                "${cache_dir}: Unable to create directory."
        else
            printf '%s: %s: %s' "${FUNCNAME[0]}" "${cache_dir}" \
                'Unable to create directory.' >&2
        fi
        return 1
    }

    echo "${cache_dir}"
}

# Create a cached Dhall expression.
#
# Usage:
#
#   command_wrapper_dhall_cache TOOLSET_NAME FILE_NAME TARGET_DIR [INPUT_FILE]
#
# Arguments:
#
#   TOOLSET_NAME
#       Name of the toolset we setting up environment for.
#
#   FILE_NAME
#       Base name of the file that we are creating, i.e. it doesn't contain
#       any path separators (e.g. slashes).
#
#   TARGET_DIR
#       Where the resulting symbolic link is created.
#
#   INPUT_FILE
#       If provided then `command_wrapper_dhall_cache` will read this file
#       instead of standard input.  Be aware that when standard input is read
#       then the content can be arbitrary Dhall expression whereas `INPUT_FILE`
#       has to be a file path.
#
# Real content is stored in directory returned by:
#
# ```
# command_wrapper_dhall_cache TOOLSET_NAME
# ```
#
# That allows us to safely share content between different environments since
# Dhall files stored there contain integrity hash in their name.
function command_wrapper_dhall_cache() {
    local -r toolset="$1"; shift
    local -r file_name="$1"; shift
    local -r target_dir="$1"; shift

    local input_file
    if (( $# )); then
        input_file="$1"; shift
    fi

    local cache_dir
    cache_dir="$(command_wrapper_cache_dir "${toolset}")"

    # Command `toolset config --dhall-freeze` writes its output atomically,
    # But we need to put content hash into the name of the result, hence
    # the temporary file.
    local file_tmp
    file_tmp="$(
        mktemp "${cache_dir}/${file_name}.tmp.XXXXXXXX"
    )" || {
        log_error 'Unable to create temporary file.'
        return 1
    }

    # Usage:
    #
    #   config [ARGUMENT [...]]
    function config() {
        "${toolset}" --no-aliases config "$@"
    }

    # Following file reads standard input, i.e. the whole
    # `command_wrapper_dhall_cache` reads standard input.
    #
    # Be aware that on some files this may take a while.
    config --dhall-freeze --no-remote-only --for-security \
        ${input_file:+--input="${input_file}"} --output="${file_tmp}"

    # We hash the content as text to take into account that we may have
    # gotten the content through a different route (file path / URL)
    # even if it normalises to the same content.  Normally it wouldn't
    # be a problem to use Dhall semantic hash of its resulting
    # expression, but if cache gets deleted or if it's explicitly
    # ignored then we may end up trying to import non-existing path.
    file_hash="$(config --dhall-hash --expression="${file_tmp} as Text")"
    file_hash="${file_hash#sha256:}"
    file="${cache_dir}/${file_name}-${file_hash}.dhall"

    # Both ${file_tmp} and ${file} need to be in the same directory for
    # this operation to be atomic on sensible file systems.
    mv -f "${file_tmp}" "${file}"

    ln -f -s "${file}" "${target_dir}/${file_name}" || {
        log_error \
            "${target_dir}/${file_name}: Failed to create symbolic link."
        return 1
    }
}

# Usage:
#
#   use command_wrapper TOOLSET_NAME CONFIG_DIR
#
# Arguments:
#
#   TOOLSET_NAME
#       Name of the toolset we setting up environment for.
#
#   CONFIG_DIR
#       Project-specific (local environment setup by direnv) configuration
#       directory for Command Wrapper. Directory `${CONFIG_DIR}/${TOOLSET_NAME}`
#       is where the toolset will be looking for its configuration.
#
# See `command-wrapper-direnv-library(7)` section CONFIGURING TOOLSET for more
# information.
function use_command_wrapper() {
    if (( ${DEBUG_COMMAND_WRAPPER_DIRENV:-0} )); then
        set -x
        : "${BASH_VERSION}"
    fi

    local DIRENV_LOG_FORMAT="direnv: ${FUNCNAME[0]}: %s"
    if (( ! ${DIRENV_IN_ENVRC:-0} )); then
        # Function log_error may not be available since it is provided by
        # Direnv's stdlib.
        log_error "This function is intended to be evaluated inside '.envrc'." \
        || printf '%s: %s' "${FUNCNAME[0]}" \
            "This function is intended to be evaluated inside '.envrc'." >&2
        return 1
    fi

    local -r toolset="$1"; shift
    local -r config_dir_param="$1"; shift

    if ! command -v "${toolset}" &>/dev/null; then
        log_error "'${toolset}': Not in PATH, has it been installed?"
        return 1
    fi

    # Usage:
    #
    #   toolset [ARGUMENT [...]]
    function toolset() {
        "${toolset}" --no-aliases "$@"
    }

    # Let's make sure that the config dir exists and get an absolute path.
    # This is one of the few ways how to do this reliably on MacOS and Linux.
    local config_dir
    config_dir="$(cd "${config_dir_param}" &>/dev/null && pwd)" || {
        log_error \
            "'${config_dir_param}': Directory does not exist or is not a directory."
        return 1
    }

    local cache_dir
    cache_dir="$(command_wrapper_cache_dir "${toolset}")"

    # {{{ Convenient Imports ##################################################
    #
    # Convenient ways of importyng Command Wrapper Dhall libraries and
    # Dhall Prelude. One way is via environment variable and the second one is
    # via file in "${config_dir}/lib/${lib_name}".
    #
    # Environment variables may have already been exported by e.g. `nix-shell`.
    # In such case they will probably contain absolute path to a file generated
    # when instantiating Command Wrappper toolset derivation.

    declare -A lib_environment_variables=(
        ['COMMAND_WRAPPER_LIB']='command-wrapper:CommandWrapper'
        ['COMMAND_WRAPPER_EXEC_LIB']='exec:Exec'
        ['COMMAND_WRAPPER_PRELUDE_LIB']='prelude:Prelude'
    )

    local lib_import
    local lib_hash
    local lib_name
    local lib_file_name
    local lib_file
    local lib_file_tmp
    mkdir -p "${config_dir}/lib"
    for var in "${!lib_environment_variables[@]}"; do
        lib_name="${lib_environment_variables[${var}]%%:*}"
        lib_file_name="${lib_environment_variables[${var}]#*:}"
        lib_import="${!var:-}"

        if [[ -z "${lib_import:-}" ]]; then
            lib_import="$(
                toolset completion --library --dhall="${lib_name}" --import
            )"
            export "${var}=${lib_import}"
        fi

        # Value of ${lib_import} is general Dhall expression, not a file name.
        # We need to pass it via stdin to `command_wrapper_dhall_cache`.
        command_wrapper_dhall_cache "${toolset}" "${lib_file_name}" \
            "${config_dir}/lib" <<< "${lib_import}"
    done

    # }}} Convenient Imports ##################################################

    # {{{ Shell Completion ####################################################

    # Completion is toolset-specific.  For that reason we need to include
    # toolset name in the einvironment variable we want to generate.
    #
    # The resulting value doesn't need to be nice, but it needs to be
    # predictable and consistent with other environment variable names.
    local env_var_prefix="${toolset^^*}"
    env_var_prefix="${env_var_prefix//[^a-zA-Z0-9_]/_}"

    declare -A completion_environment_variables=(
        ["${env_var_prefix}_BASH_COMPLETION"]='bash'
        ["${env_var_prefix}_FISH_COMPLETION"]='fish'
        ["${env_var_prefix}_ZSH_COMPLETION"]='zsh'
    )

    local completion_file
    local completion_file_tmp
    local completion_file_hash
    local completion_shell
    for var in "${!completion_environment_variables[@]}"; do
        completion_file="${!var:-}"
        completion_shell="${completion_environment_variables[${var}]}"

        if [[ -z "${completion_file:-}" || ! -r "${completion_file:-}" ]]; then
            local completion_file_tmp
            completion_file_tmp="$(
                mktemp "${cache_dir}/completion.${completion_shell}.tmp.XXXXXXXX"
            )" || {
                log_error 'Unable to create temporary file.'
                return 1
            }
            toolset completion --script --shell="${completion_shell}" \
                --output="${completion_file_tmp}"

            completion_file_hash="$(
                toolset config --dhall-hash \
                    --expression="${completion_file_tmp} as Text"
            )"
            completion_file_hash="${completion_file_hash#sha256:}"
            completion_file="${cache_dir}/completion-${completion_file_hash}.${completion_shell}"
            mv -f "${completion_file_tmp}" "${completion_file}"

            export "${var}=${completion_file}"
        fi
    done

    # }}} Shell Completion ####################################################

    # {{{ Cached Dhall Configs ################################################

    local toolset_config_dir="${config_dir}/${toolset}"
    local default_config_constructor="${toolset_config_dir}/default/constructor.dhall"
    if [[ -f "${default_config_constructor}" ]]; then
        watch_file "${default_config_constructor}"
        command_wrapper_dhall_cache "${toolset}" "default.dhall" \
            "${config_dir}/${toolset}" "${default_config_constructor}"
    fi

    # }}} Cached Dhall Configs ################################################

    # This will make toolset aware of project-level configuration.  We need to
    # keep at the bottom so that above invocations of toolset will not fail
    # while we may be building the project-level configuration.
    export COMMAND_WRAPPER_LOCAL_CONFIG_DIR="${config_dir}"
}
