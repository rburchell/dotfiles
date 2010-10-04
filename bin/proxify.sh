#!/bin/bash
all=`cat "/dev/stdin"`

# check they are actually trying to run something
if [ $# -eq 0 ]
then
    echo "$0: you need to pass an argument";
    exit 1
fi

if `/sbin/ifconfig eth0 | /bin/grep "inet addr" | /usr/bin/awk -F: '{ print $2 }' | /usr/bin/awk '{ print $1 }' |  /bin/grep -qE '172\.21\.[0-9]{1,3}\.[0-9]{1,3}'`; then
    echo "$0: tsocksifying";
    # check for started ssh tunnel
    COMMAND="ssh -fND 1234 w00t@dereferenced.net"
    echo "$0: tunnelling"
    pgrep -f -x "$COMMAND" > /dev/null 2>&1 || $COMMAND
    echo "$0: done tunnelling"

    export TSOCKS_CONF_FILE=~/.tsocks.conf
    echo "$all" | tsocks $*
else
    echo "$all" | $*
fi
