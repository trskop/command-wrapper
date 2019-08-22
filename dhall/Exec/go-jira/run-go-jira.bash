#!/usr/bin/env bash

# Requirements:
# - curl
# - sha256sum from GNU coreutils

set -euo pipefail

if [[ "${COMMAND_WRAPPER_VERBOSITY:-}" == 'annoying' ]]; then
    set -x
fi

function main() {
    local -r cacheDir="${XDG_CACHE_HOME:-${HOME}/.cache}/go-jira"
    local downloadUrl
    local downloadSha256
    local binary
    case "$(uname -m)" in
        x86_64)
            # https://github.com/go-jira/jira/releases/tag/v1.0.20
            downloadUrl='https://github.com/go-jira/jira/releases/download/v1.0.20/jira-linux-amd64'
            downloadSha256='4eb49f39caa87b2fc799409d2a81118c8da0f11dcd74e51eab959bbcb86e394c'
            binary="${cacheDir}/${downloadSha256}/${downloadUrl##*/}"
            ;;
        *)
            exit 1
            ;;
    esac

    if [[ ! -e "${binary}" ]]; then
        mkdir -p "$(dirname "${binary}")"
        curl --silent --location "${downloadUrl}" > "${binary}"
        sha256sum --check --status - <<< "${downloadSha256}  ${binary}"
    fi

    if [[ ! -x "${binary}" ]]; then
        chmod +x "${binary}"
    fi

    "${binary}" "$@"
}

main "$@"
