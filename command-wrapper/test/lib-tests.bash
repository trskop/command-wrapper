# shellcheck shell=bash

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

function testMsg() {
    echo "$@" 1>&2
}

function test_testVersionMinBound () {
    local -r message="$1"; shift
    local -r -i expectedExitCode="$1"; shift

    testVersionMinBound "$@"
    local -r -i ret=$?

    if (( ret == expectedExitCode )); then
        testMsg "    PASS: ${message}"
        return 0
    else
        testMsg "    FAIL: ${message}: Expected exit code ${expectedExitCode}," \
            "but got ${ret}."
        return 1
    fi
}

function main() {
    local -r bashLibraryFile="$1"; shift

    local -i testFailed=0

    # shellcheck source=/dev/null
    source "${bashLibraryFile}"

    testMsg "Command Wrapper Bash library: ${bashLibraryFile}"
    testMsg "  testVersionMinBound():"

    local -A test_testVersionMinBound_data=(
        ['1 >= 1']='0:1:1'
        ['1.1 >= 1']='0:1.1:1'
        ['2.1 >= 2.2']='1:2.1:2.2'
        ['3.0.4.10 >= 3.0.4.2']='0:3.0.4.10:3.0.4.2'
        ['4.08 >= 4.08.01']='1:4.08:4.08.01'
        ['3.2.1.9.8144 >= 3.2']='0:3.2.1.9.8144:3.2'
        ['3.2 >= 3.2.1.9.8144']='1:3.2:3.2.1.9.8144'
        ['1.2 >= 2.1']='1:1.2:2.1'
        ['2.1 >= 1.2']='0:2.1:1.2'
        ['5.6.7 >= 5.6.7']='0:5.6.7:5.6.7'
        ['1.01.1 >= 1.1.1']='0:1.01.1:1.1.1'
        ['1.1.1 >= 1.01.1']='0:1.1.1:1.01.1'
        ['1 >= 1.0']='0:1:1.0'
        ['1.0 >= 1']='0:1.0:1'
        ['1.0.2.0 >= 1.0.2']='0:1.0.2.0:1.0.2'
    )

    local -a testArgs
    local testName
    for testName in "${!test_testVersionMinBound_data[@]}"; do
        mapfile -d ':' -c 3 -t testArgs \
          <<< "${test_testVersionMinBound_data[${testName}]}:"
        if ! test_testVersionMinBound "${testName}" "${testArgs[@]}"; then
            testFailed=1
        fi
    done

    if (( testFailed )); then
        exit 1
    fi
}

main "$@"
