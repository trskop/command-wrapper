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

    testVersion "$@"
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
        ['1.0.0 > 1.0.0']=1
        ['1 > 1']=1
        ['1.1 > 1']=0
        ['2.1 > 2.2']=1
        ['3.0.4.10 > 3.0.4.2']=0
        ['4.08 > 4.08.01']=1
        ['3.2.1.9.8144 > 3.2']=0
        ['3.2 > 3.2.1.9.8144']=1
        ['1.2 > 2.1']=1
        ['2.1 > 1.2']=0
        ['5.6.7 > 5.6.7']=1
        ['1.01.1 > 1.1.1']=1
        ['1.1.1 > 1.01.1']=1
        ['1 > 1.0']=1
        ['1.0 > 1']=1
        ['1.0.2.0 > 1.0.2']=1

        ['1.0.0 -gt 1.0.0']=1
        ['1 -gt 1']=1
        ['1.1 -gt 1']=0
        ['2.1 -gt 2.2']=1
        ['3.0.4.10 -gt 3.0.4.2']=0
        ['4.08 -gt 4.08.01']=1
        ['3.2.1.9.8144 -gt 3.2']=0
        ['3.2 -gt 3.2.1.9.8144']=1
        ['1.2 -gt 2.1']=1
        ['2.1 -gt 1.2']=0
        ['5.6.7 -gt 5.6.7']=1
        ['1.01.1 -gt 1.1.1']=1
        ['1.1.1 -gt 1.01.1']=1
        ['1 -gt 1.0']=1
        ['1.0 -gt 1']=1
        ['1.0.2.0 -gt 1.0.2']=1

        ['1.0.0 < 1.0.0']=1
        ['1 < 1']=1
        ['1.1 < 1']=1
        ['2.1 < 2.2']=0
        ['3.0.4.10 < 3.0.4.2']=1
        ['4.08 < 4.08.01']=0
        ['3.2.1.9.8144 < 3.2']=1
        ['3.2 < 3.2.1.9.8144']=0
        ['1.2 < 2.1']=0
        ['2.1 < 1.2']=1
        ['5.6.7 < 5.6.7']=1
        ['1.01.1 < 1.1.1']=1
        ['1.1.1 < 1.01.1']=1
        ['1 < 1.0']=1
        ['1.0 < 1']=1
        ['1.0.2.0 < 1.0.2']=1

        ['1.0.0 -lt 1.0.0']=1
        ['1 -lt 1']=1
        ['1.1 -lt 1']=1
        ['2.1 -lt 2.2']=0
        ['3.0.4.10 -lt 3.0.4.2']=1
        ['4.08 -lt 4.08.01']=0
        ['3.2.1.9.8144 -lt 3.2']=1
        ['3.2 -lt 3.2.1.9.8144']=0
        ['1.2 -lt 2.1']=0
        ['2.1 -lt 1.2']=1
        ['5.6.7 -lt 5.6.7']=1
        ['1.01.1 -lt 1.1.1']=1
        ['1.1.1 -lt 1.01.1']=1
        ['1 -lt 1.0']=1
        ['1.0 -lt 1']=1
        ['1.0.2.0 -lt 1.0.2']=1

        ['1.0.0 >= 1.0.0']=0
        ['1 >= 1']=0
        ['1.1 >= 1']=0
        ['2.1 >= 2.2']=1
        ['3.0.4.10 >= 3.0.4.2']=0
        ['4.08 >= 4.08.01']=1
        ['3.2.1.9.8144 >= 3.2']=0
        ['3.2 >= 3.2.1.9.8144']=1
        ['1.2 >= 2.1']=1
        ['2.1 >= 1.2']=0
        ['5.6.7 >= 5.6.7']=0
        ['1.01.1 >= 1.1.1']=0
        ['1.1.1 >= 1.01.1']=0
        ['1 >= 1.0']=0
        ['1.0 >= 1']=0
        ['1.0.2.0 >= 1.0.2']=0

        ['1.0.0 -ge 1.0.0']=0
        ['1 -ge 1']=0
        ['1.1 -ge 1']=0
        ['2.1 -ge 2.2']=1
        ['3.0.4.10 -ge 3.0.4.2']=0
        ['4.08 -ge 4.08.01']=1
        ['3.2.1.9.8144 -ge 3.2']=0
        ['3.2 -ge 3.2.1.9.8144']=1
        ['1.2 -ge 2.1']=1
        ['2.1 -ge 1.2']=0
        ['5.6.7 -ge 5.6.7']=0
        ['1.01.1 -ge 1.1.1']=0
        ['1.1.1 -ge 1.01.1']=0
        ['1 -ge 1.0']=0
        ['1.0 -ge 1']=0
        ['1.0.2.0 -ge 1.0.2']=0

        ['1.0.0 <= 1.0.0']=0
        ['1 <= 1']=0
        ['1.1 <= 1']=1
        ['2.1 <= 2.2']=0
        ['3.0.4.10 <= 3.0.4.2']=1
        ['4.08 <= 4.08.01']=0
        ['3.2.1.9.8144 <= 3.2']=1
        ['3.2 <= 3.2.1.9.8144']=0
        ['1.2 <= 2.1']=0
        ['2.1 <= 1.2']=1
        ['5.6.7 <= 5.6.7']=0
        ['1.01.1 <= 1.1.1']=0
        ['1.1.1 <= 1.01.1']=0
        ['1 <= 1.0']=0
        ['1.0 <= 1']=0
        ['1.0.2.0 <= 1.0.2']=0

        ['1.0.0 -le 1.0.0']=0
        ['1 -le 1']=0
        ['1.1 -le 1']=1
        ['2.1 -le 2.2']=0
        ['3.0.4.10 -le 3.0.4.2']=1
        ['4.08 -le 4.08.01']=0
        ['3.2.1.9.8144 -le 3.2']=1
        ['3.2 -le 3.2.1.9.8144']=0
        ['1.2 -le 2.1']=0
        ['2.1 -le 1.2']=1
        ['5.6.7 -le 5.6.7']=0
        ['1.01.1 -le 1.1.1']=0
        ['1.1.1 -le 1.01.1']=0
        ['1 -le 1.0']=0
        ['1.0 -le 1']=0
        ['1.0.2.0 -le 1.0.2']=0

        ['1.0.0 == 1.0.0']=0
        ['1 == 1']=0
        ['1.1 == 1']=1
        ['2.1 == 2.2']=1
        ['3.0.4.10 == 3.0.4.2']=1
        ['4.08 == 4.08.01']=1
        ['3.2.1.9.8144 == 3.2']=1
        ['3.2 == 3.2.1.9.8144']=1
        ['1.2 == 2.1']=1
        ['2.1 == 1.2']=1
        ['5.6.7 == 5.6.7']=0
        ['1.01.1 == 1.1.1']=0
        ['1.1.1 == 1.01.1']=0
        ['1 == 1.0']=0
        ['1.0 == 1']=0
        ['1.0.2.0 == 1.0.2']=0

        ['1.0.0 -eq 1.0.0']=0
        ['1 -eq 1']=0
        ['1.1 -eq 1']=1
        ['2.1 -eq 2.2']=1
        ['3.0.4.10 -eq 3.0.4.2']=1
        ['4.08 -eq 4.08.01']=1
        ['3.2.1.9.8144 -eq 3.2']=1
        ['3.2 -eq 3.2.1.9.8144']=1
        ['1.2 -eq 2.1']=1
        ['2.1 -eq 1.2']=1
        ['5.6.7 -eq 5.6.7']=0
        ['1.01.1 -eq 1.1.1']=0
        ['1.1.1 -eq 1.01.1']=0
        ['1 -eq 1.0']=0
        ['1.0 -eq 1']=0
        ['1.0.2.0 -eq 1.0.2']=0

        ['1.0.0 != 1.0.0']=1
        ['1 != 1']=1
        ['1.1 != 1']=0
        ['2.1 != 2.2']=0
        ['3.0.4.10 != 3.0.4.2']=0
        ['4.08 != 4.08.01']=0
        ['3.2.1.9.8144 != 3.2']=0
        ['3.2 != 3.2.1.9.8144']=0
        ['1.2 != 2.1']=0
        ['2.1 != 1.2']=0
        ['5.6.7 != 5.6.7']=1
        ['1.01.1 != 1.1.1']=1
        ['1.1.1 != 1.01.1']=1
        ['1 != 1.0']=1
        ['1.0 != 1']=1
        ['1.0.2.0 != 1.0.2']=1

        ['1.0.0 -ne 1.0.0']=1
        ['1 -ne 1']=1
        ['1.1 -ne 1']=0
        ['2.1 -ne 2.2']=0
        ['3.0.4.10 -ne 3.0.4.2']=0
        ['4.08 -ne 4.08.01']=0
        ['3.2.1.9.8144 -ne 3.2']=0
        ['3.2 -ne 3.2.1.9.8144']=0
        ['1.2 -ne 2.1']=0
        ['2.1 -ne 1.2']=0
        ['5.6.7 -ne 5.6.7']=1
        ['1.01.1 -ne 1.1.1']=1
        ['1.1.1 -ne 1.01.1']=1
        ['1 -ne 1.0']=1
        ['1.0 -ne 1']=1
        ['1.0.2.0 -ne 1.0.2']=1
    )

    local -a testArgs
    local testData
    for testData in "${!test_testVersionMinBound_data[@]}"; do
        mapfile -d ' ' -c 3 -t testArgs <<< "${testData} "
        if ! test_testVersionMinBound "${testData}" \
                "${test_testVersionMinBound_data[${testData}]}" \
                "${testArgs[@]}"
        then
            testFailed=1
        fi
    done

    if (( testFailed )); then
        exit 1
    fi
}

main "$@"
