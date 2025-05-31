#!/bin/bash

LOG=autogen.log

function abort()
{
    echo "An error occured, the output below can be found in $LOG"
    echo "-------"
    cat $LOG
    exit 1
}

function run()
{
    echo -n "Running \"$@\" ... "
    echo "$ $@" >>$LOG
    eval $@ >>$LOG 2>&1
    if [ $? -eq 0 ]; then
        echo "OK"
    else
        echo "FAILED"
        abort
    fi
}

if [ -e $LOG ]; then
    rm "$LOG"
fi

run "mkdir build-aux"
run "aclocal -I m4"
run "autoheader"
run "autoconf"
run "automake --add-missing --copy"

echo "done; Full log available in $LOG"
