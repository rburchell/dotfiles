#!/bin/sh
for i in `ls -1 *.rb`; do
    sudo cp $i /usr/local/bin/
done
for i in `ls -1 *.sh`; do
    sudo cp $i /usr/local/bin/
done

sudo cp new_workdir /usr/lib/git-core/git-new-workdir
