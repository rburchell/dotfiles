#!/bin/sh
unity-settings-daemon &
vmtoolsd -n vmusr &
xsetroot -solid "rgb:80/c3/42"
exec i3

