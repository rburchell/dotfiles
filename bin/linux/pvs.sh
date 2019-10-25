#!/bin/sh

pvs-studio-analyzer trace -- $@

# -e /path/to/exclude-path
pvs-studio-analyzer analyze -o $(pwd)/pvs.log -j8
#pvs-studio-analyzer analyze --compiler=i686-pc-serenity-gcc --compiler=i686-pc-serenity-g++ -o $(pwd)/pvs.log -j8
plog-converter -a GA:1,2 -t tasklist -o $(pwd)/pvs.tasks.log $(pwd)/pvs.log
