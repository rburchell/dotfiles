#!/bin/sh
shopt -s dotglob
find ~/dotfiles/.ssh -mindepth 1 -maxdepth 1 -name ".*" -exec mv {} ~/.ssh \;
rm -r ~/dotfiles/.ssh
find ~/dotfiles/ -mindepth 1 -maxdepth 1 -name ".*" -exec mv {} ~/.ssh \;
mv ~/dotfiles/* ~
rm -r ~/dotfiles
chmod og-rwx .ssh/*
