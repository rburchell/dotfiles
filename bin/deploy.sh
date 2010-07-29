#!/bin/sh
for i in `ls -1`; do
    sudo cp $i /usr/local/bin/
done
