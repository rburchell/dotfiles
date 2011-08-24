umask 022

autoload colors
colors

source ~/.zsh/functions
source ~/.zsh/vim-mode

if [ -e "/etc/chroot-name" ]; then
  CHROOT_PS1="(chroot-$(cat /etc/chroot-name))"
else
  CHROOT_PS1=
fi

function setTitle {
    [ "$_ZSH_VIM_MODE" ] || _ZSH_VIM_MODE="INSERT"

    if [ $TERM = "xterm" ] || [ $TERM = "rxvt" ] || \
       [ $TERM = "xterm-color" ]; then
        if [ `whoami` = "burchr" ]; then
            print -Pn "\e]0;%m: %~ $_ZSH_VIM_MODE\a"
	else
	    print -Pn "\e]0;%n@%m: %~ $_ZSH_VIM_MODE\a"
	fi
    fi

}

function precmd {
    setTitle
    COLLABORA=0

    case `pwd` in
        /home/burchr/code/qt/*) COLLABORA=1;;
        /home/burchr/code/collabora/*) COLLABORA=1;;
        /j/scratchbox/*) COLLABORA=1;;
        /scratchbox/*) COLLABORA=1;;
    esac

    if [ $COLLABORA -eq 1 ]; then
        export GIT_AUTHOR_EMAIL="robin.burchell@collabora.co.uk"
        export GIT_COMMITTER_EMAIL="robin.burchell@collabora.co.uk"
    else
        export GIT_AUTHOR_EMAIL="viroteck@viroteck.net"
        export GIT_COMMITTER_EMAIL="viroteck@viroteck.net"
    fi

    export HOSTNAME="`hostname`$CHDOOT_PS1"

    case $HOSTNAME in
        virgin)
            COLORHOST="$fg[yellow]$HOSTNAME$reset_color"
            ;;
        iris)
            COLORHOST="$fg[green]$HOSTNAME$reset_color"
            ;;
        *)
            COLORHOST=$HOSTNAME
            ;;
    esac

    export WHOAMI="`whoami`"
    case $WHOAMI in
        burchr)
            COLORWHOAMI=""
            ;;
        *)
            COLORWHOAMI=$fg[blue]$bg[red]$WHOAMI$reset_color@
            ;;
    esac

    export PS1="$COLORWHOAMI$COLORHOST:%~%% "
    export RPS1="($(date))"
}

preexec() {
    if [ $TERM = "xterm" ] || [ $TERM = "rxvt" ] || \
       [ $TERM = "xterm-color" ]; then
        if [ $WHOAMI = "burchr" ]; then
            print -Pn "\e]0;$1 (%m: %~)\a";
        else
            print -Pn "\e]0;$1 (%n@%m: %~)\a";
        fi
    fi
}

export PATH=~/bin::/usr/local/bin:/usr/local/sbin:/bin:/sbin:/usr/bin:/usr/sbin:/usr/games
export EDITOR="vim"
#export LANG="en_US.UTF-8"
#export LC_MESSAGES="en_US.UTF-8"
#export LC_CTYPE="en_US.UTF-8"
#export LC_NUMERIC=C
#export LC_COLLATE=C
export EMAIL="viroteck@viroteck.org"
export DEBEMAIL=$EMAIL

READNULLCMD=${PAGER:-/usr/bin/less}
which lesspipe >/dev/null 2>&1 && eval "$(lesspipe)"

alias gvim='gvim --servername VIM --remote-tab-silent'
alias vim='vim --servername VIM --remote-tab-silent'
alias ls='ls -A --color=auto'
alias lsl='ls -A --color=auto -l'
alias cl='clear && logout'
alias g='grep -I --exclude-dir=debian --exclude-dir=obj-\* --exclude-dir=hw/dmx/doc --exclude-dir=autom4te.cache --exclude=configure --exclude=tags --exclude=Makefile.in --exclude=cscope.out'
alias cs='cscope **/*.[ch]'
alias nmctl='sudo invoke-rc.d network-manager'
kbl () {
    sudo sh -c "echo $1 > /sys/class/leds/smc::kbd_backlight/brightness"
}

alias gci='git commit'
alias gco='git checkout'
alias gl='git log'
gd() {
    git diff $* | vim -
}
gs() {
    git show $* | vim -
}
gc() {
    git cat-file -p $(git ls-tree $1 $2 | awk '{ print $3; }') | vim -
}
wifi() {
    sudo -v
    sudo killall wpa_supplicant dhclient
    sudo wpa_supplicant -ieth0 -c/etc/wpa_supplicant/$1.conf &|
    sleep 2
    sudo dhclient -v eth0 &|
}

alias d='(test -d obj-i386 || mkdir obj-i386) && pushd obj-i386 && ../autogen.sh --prefix=/usr && make distcheck && popd'
genlog() {
    git-log --no-merges $1-$2..'HEAD^1' | git shortlog > ~/x/xorg/final/$1-$3
}

setopt GLOB EXTENDED_GLOB MAGIC_EQUAL_SUBST RC_EXPAND_PARAM \
       HIST_EXPIRE_DUPS_FIRST HIST_IGNORE_DUPS HIST_VERIFY CORRECT HASH_CMDS \
       PRINT_EXIT_VALUE RC_QUOTES AUTO_CONTINUE MULTIOS VI INC_APPEND_HISTORY \
       APPENDHISTORY
unsetopt beep
unset MAIL

REPORTTIME=10
HISTFILE=~/.zsh/history
HISTSIZE=5000
SAVEHIST=5000

bindkey '^[OH' beginning-of-line
bindkey '^A' beginning-of-line
bindkey '^[OF' end-of-line
bindkey '^E' end-of-line
bindkey -M viins '^R' history-incremental-search-backward
bindkey -M vicmd '^R' history-incremental-search-backward

source ~/.zsh/compinstall
if [ -e ~/.zsh/host ]; then
    source ~/.zsh/host
fi

 # date trick courtesy of http://ubuntuforums.org/showthread.php?t=1111038
 # (yes, it's simple, but I'm too lazy to DIY)
 dato1=`date +%A`; #day of the week
 dato2=`date +%d`; #day of the month
 dato3=`date +%B`; #month of the year
 dato4=`date +%Y`; #Year
 dato5=`date +%U`; #week of the year
  
 echo "Date is now: $dato1, $dato2. $dato3, $dato4 (W$dato5)"
 echo "Your host is: `hostname`"

# if the command-not-found package is installed, use it
if [ -x /usr/lib/command-not-found -o -x /usr/share/command-not-found ]; then
    function command_not_found_handler {
        # check because c-n-f could've been removed in the meantime
        if [ -x /usr/lib/command-not-found ]; then
           /usr/bin/python /usr/lib/command-not-found -- $1
           return $?
        elif [ -x /usr/share/command-not-found ]; then
           /usr/bin/python /usr/share/command-not-found -- $1
           return $?
        else
           return 127
        fi
    }
fi
