#!/usr/bin/env sh

n_tested=0
n_failed=0
failed=""

testsuites="mangle-test fb-util-tests thrift-compiler-tests thrift-lib-tests thrift-server-tests"

for t in $testsuites; do
    cabal run $t
    if [ $? -ne 0 ]; then
       n_failed=$(($n_failed+1))
       failed="$failed $t"
    fi
    n_tested=$(($n_tested+1))
done

if [ $n_failed -ne 0 ]; then
    echo "$n_failed / $n_tested testsuites failing:$failed"
    exit 1
else
    echo "All tests passed"
fi
