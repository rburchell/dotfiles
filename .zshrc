umask 022

autoload colors
colors

zmodload zsh/pcre &>/dev/null

export PLATFORM='unknown'
local unamestr=$(uname)
if [[ "$unamestr" == 'Linux' ]]; then
   export PLATFORM='linux'
elif [[ "$unamestr" == 'Darwin' ]]; then
   export PLATFORM='osx'
else
   echo "warning: platform unknown"
fi

# detect whether we can start vim graphically
if [[ "$PLATFORM" == "osx" ]]; then
    if [ -e /usr/local/bin/mvim ]; then
        alias mvim='/usr/local/bin/mvim --remote-tab-silent'
        alias gvim='/usr/local/bin/mvim --remote-tab-silent'
        alias vim='/usr/local/bin/mvim --remote-tab-silent'
    fi
else
    vim --help | grep servername 2>&1 > /dev/null
    if [ $? -eq 0 ]; then
        alias mvim='gvim --servername VIM --remote-tab-silent'
        alias gvim='gvim --servername VIM --remote-tab-silent'
        alias vim='vim --servername VIM --remote-tab-silent'
    fi
fi

function setTitle {
    if [ $TERM = "xterm" ] || [ $TERM = "rxvt" ] || \
       [ $TERM = "xterm-color" ]; then
        if [ "$LOGNAME" = "burchr" ]; then
            print -Pn "\e]0;%m: %~\a"
        else
            print -Pn "\e]0;%n@%m: %~\a"
        fi
    fi

}

function precmd {
    # must be done early to save status
    local exit_status=$?

    if [ $exit_status -ne 0 ]; then
        echo "zsh: exit $fg[red]$exit_status$reset_color";
    else
        echo "zsh: exit $exit_status";
    fi

    setTitle

    case $(pwd) in
        # TODO: is there a less awkward way to handle a path or anything under it?
        */burchr/code/qt/*)
            ;& # fallthrough
        */burchr/code/qt)
            export GIT_AUTHOR_EMAIL="robin+qt@viroteck.net"
            export GIT_COMMITTER_EMAIL="robin+qt@viroteck.net"
            ;;
        *)
            export GIT_AUTHOR_EMAIL="robin+git@viroteck.net"
            export GIT_COMMITTER_EMAIL="robin+git@viroteck.net"
            ;;
    esac

    export HOSTNAME="$HOST$CHDOOT_PS1"
    local shorthost
    hostname -s 2>/dev/null | read shorthost
    if [ $? -ne 0 ]; then
        # workaround for https://bugzilla.redhat.com/show_bug.cgi?id=531702
        shorthost=$(hostname)
    fi

    case $HOSTNAME in
        vestal.local.viroteck.net)
            COLORHOST="%{$fg[cyan]%}$shorthost%{$reset_color%}"
            ;;
        iris.dereferenced.net)
            COLORHOST="%{$fg[red]%}$shorthost%{$reset_color%}"
            ;;
        zoe.dereferenced.net)
            COLORHOST="%{$fg[green]%}$shorthost%{$reset_color%}"
            ;;
        fedora.vm)
            COLORHOST="%{$fg[yellow]%}$shorthost%{$reset_color%}"
            ;;
        *)
            COLORHOST=$HOSTNAME
            ;;
    esac

    case $LOGNAME in
        burchr)
            COLORWHOAMI=""
            ;;
        root)
            COLORWHOAMI="%{$fg[white]$bg[red]%}$WHOAMI%{$reset_color%}@"
            ;;
        *)
            COLORWHOAMI="%{$fg[blue]$bg[yellow]%}$WHOAMI%{$reset_color%}@"
            ;;
    esac

    if [ -e "/etc/debian_chroot" ]; then
        CHROOT_PS1=":(chroot-$(cat /etc/debian_chroot))"
    else
        CHROOT_PS1=
    fi

    export PS1="$COLORWHOAMI$COLORHOST$CHROOT_PS1:%~%% "

    export RPS1="($(date '+W%U - %m/%d@%H:%M:%S %Z'))"
}

preexec() {
    if [ $TERM = "xterm" ] || [ $TERM = "rxvt" ] || \
       [ $TERM = "xterm-color" ] || [ $TERM = "xterm-256color" ]; then
        if [ "$LOGNAME" = "burchr" ]; then
            print -Pn "\e]0;$1 (%m: %~)\a";
        else
            print -Pn "\e]0;$1 (%n@%m: %~)\a";
        fi
    fi
}

export PATH=~/bin:/usr/local/bin:/usr/local/sbin:/bin:/sbin:/usr/bin:/usr/sbin:/usr/games
export EDITOR="vim"
export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"
export LC_MESSAGES="en_US.UTF-8"
export LC_CTYPE="en_US.UTF-8"
export LC_NUMERIC=C
export LC_COLLATE=C
export EMAIL="robin@viroteck.net"
export DEBEMAIL=$EMAIL

READNULLCMD=${PAGER:-/usr/bin/less}
which lesspipe >/dev/null 2>&1 && eval "$(lesspipe)"

if [[ "$PLATFORM" == "linux" ]]; then
    alias ls='ls -A --color=auto'
    alias lsl='ls -A --color=auto -l'
elif [[ "$PLATFORM" == 'osx' ]]; then
    alias ls='ls -A -G'
    alias lsl='ls -A -l -G'
fi

alias cl='clear && logout'
alias g='grep -I --exclude-dir=debian --exclude-dir=obj-\* --exclude-dir=hw/dmx/doc --exclude-dir=autom4te.cache --exclude=configure --exclude=tags --exclude=Makefile.in --exclude=cscope.out'

alias gp='git push'
alias gpr='git pull --rebase'
alias gci='git commit'
alias gcia='git commit -a'
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

genlog() {
    git-log --no-merges $1-$2..'HEAD^1' | git shortlog > ~/x/xorg/final/$1-$3
}

ffind() {
    find . -name "*$1*"
}

# PRINT_EXIT_VALUE removed, we do that ourselves in precmd
setopt GLOB EXTENDED_GLOB MAGIC_EQUAL_SUBST RC_EXPAND_PARAM \
       HIST_EXPIRE_DUPS_FIRST HIST_IGNORE_DUPS HIST_VERIFY CORRECT HASH_CMDS \
       RC_QUOTES AUTO_CONTINUE MULTIOS VI INC_APPEND_HISTORY \
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
bindkey -e

source ~/.zsh/compinstall

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

# thanks to Thiago Macieira for this.
function removefrom()
{
    local varname=$1
    local entry="$2"

    unset newvar
    for value in `IFS=: eval echo \\$$varname`; do
	if [ "$value" != "$entry" ]; then
	    newvar="${newvar}${newvar+:}${value}"
	fi
    done

    # Set it again
    eval $varname="$newvar"
    unset newvar
}

function addto()
{
    local varname=$1
    local newvalue=$2
    eval $varname="$newvalue:\${$varname}"
    eval $varname=\${$varname/%:}
}

function qt()
{
    local devhome=${DEVHOME-$HOME}
    local srchome=${SRCHOME-$devhome/code}
    local qthome=${QTHOME-$srchome/qt}

    # Get or set the Qt version
    if [ $# -eq 0 ]; then
	# Get the Qt version
	if [ -z "$QTDIR" ]; then
	    echo "Not using Qt."
	else
	    ver=${QTDIR/#$qthome\/qt}
	    ver=${ver/#-}
	    echo "Using Qt $ver"
	fi
    else
	# Set the working Qt version
	wanted=$qthome/$1
	test -d $wanted || wanted=$qthome/qt$1

	if [ x$1 != xnone -a ! -d $wanted ]; then
	    echo "Cannot find Qt $1 at $wanted"
	else
	    # Remove old
	    removefrom PATH $QTDIR/bin
	    removefrom LD_LIBRARY_PATH $QTDIR/lib
	    removefrom PKG_CONFIG_PATH $QTDIR/lib/pkgconfig

	    # Add new
	    if [ x$1 != xnone ]; then
		QTDIR=$wanted

		addto PATH $QTDIR/bin
		addto LD_LIBRARY_PATH $QTDIR/lib
		addto PKG_CONFIG_PATH $QTDIR/lib/pkgconfig

		echo "Using Qt $1 from $QTDIR"
	    else
		echo "Not using Qt"
	    fi
	fi
    fi

    export LD_LIBRARY_PATH PKG_CONFIG_PATH
    export QTDIR QTSRCDIR
}

# make git completion not be so ridiculously slow
__git_files () {
    _wanted files expl 'local files' _files
}

function ocd() {
    if [ -z $1 ]; then
        local software=`pwd`
        software=`basename "$software"`
    else
        local software="$1"
    fi

    local oscdir=`find ~/osc -path "*/$software/.osc" -print0 | cut -d '' -f1`
    oscdir=`echo "$oscdir" | sed s,/.osc,,`
    cd "$oscdir"
}

function ccd() {
    if [ -z $1 ]; then
        local software=`pwd`
        software=`basename "$software"`
    else
        local software="$1"
    fi

    local pwd=`pwd`
    cd `find ~/code -maxdepth 2 -path "*/$software"`
    if [[ "$pwd" == `pwd` ]]; then
        cd ~/code
    fi
}

function randstr() {
    local rand=`hexdump -n 16 -v -e '/1 "%02X"' /dev/urandom`
    echo $rand
}

function pastebin() {
    if [ -z $1 ]; then
        echo "$0: need a filename to pastebin"
        return 1
    fi

    if ! [ -f "$1" ]; then
        echo "$0: file does not exist"
        return 1
    fi

    local ext=`echo $1 | awk -F . '{ print $NF } '`
    local rand=`hexdump -n 16 -v -e '/1 "%02X"' /dev/urandom`
    scp "$1" "zoe:/var/www/qtl.me/www/$rand.$ext"
    echo "pasted http://qtl.me/$rand.$ext"
}

function upload() {
    if [ -z $1 ]; then
        echo "$0: need a filename to upload"
        return 1
    fi

    if ! [ -f "$1" ]; then
        echo "$0: file does not exist"
        return 1
    fi

    local name=`basename "$1"`
    scp "$1" "zoe:/var/www/qtl.me/www/$name"
    echo "uploaded http://qtl.me/$name"
}

function viyaml() {
    if [ -z "$1" ]; then
        vim rpm/*.yaml
        (cd rpm; specify)
    else
        vim "$1"
        (cd `dirname "$1"`; specify `basename "$1"`)
    fi
}

function vizsh() {
    vim ~/.zshrc
    zsh -n ~/.zshrc

    if [ $? -eq 0 ]; then
        source ~/.zshrc
    else
        echo "$0: syntax error, not reloading"
    fi
}

function syncup() {
    local src
    local target
    if [[ -z "$1" || "$1" == "." ]]; then
        src=.
        target=$(readlink -f .)
    else
        src="$1"
        target=$(dirname $(readlink -f "$1"))
    fi

    echo "syncup $src to $target"
    sudo rsync -aAXvzoglpEtHAXS --progress --super --rsync-path="sudo rsync --super" --delete "$src" "burchr@zoe.dereferenced.net:/$target"
}

function syncdown() {
    local src
    local target=.
    if [[ -z "$1" || "$1" == "." ]]; then
        src=`pwd`/
    else
        src="$(readlink -f "$1")"
        target="$(dirname "$1")"
    fi

    echo "syncdown $src to $target"
    sudo rsync -aAXvzoglpEtHAXS --progress --super --rsync-path='sudo rsync --super' --delete "burchr@zoe.dereferenced.net:/$src" "$target"
}

