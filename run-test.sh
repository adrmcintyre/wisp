#!/bin/bash
echo '(load "tests/r4rstest.wisp")' | ./build/src/wisp > test-results.txt
rm -f tmp1 tmp2 tmp3
grep -B1 'BUT EXPECTED' test-results.txt > test-failures.txt
cat test-failures.txt
