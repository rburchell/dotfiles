source /etc/profile

umask 022

autoload colors && colors

setopt prompt_subst

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

function setTitle {
    if [ $TERM = "xterm" ] || [ $TERM = "rxvt" ] || \
       [ $TERM = "xterm-color" ]; then
        if [ "$USER" = "burchr" ]; then
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
            export GIT_AUTHOR_EMAIL="robin.burchell@viroteck.net"
            export GIT_COMMITTER_EMAIL="robin.burchell@viroteck.net"
            ;;
        */burchr/code/digia/*)
            export GIT_AUTHOR_EMAIL="robin.burchell@qt.io"
            export GIT_COMMITTER_EMAIL="robin.burchell@qt.io"
            ;;
        *)
            export GIT_AUTHOR_EMAIL="robin+git@viroteck.net"
            export GIT_COMMITTER_EMAIL="robin+git@viroteck.net"
            ;;
    esac

    # do this again to make sure it's up to date.
    local shorthost=$(echo "$HOST" | cut -d'.' -f1)

    local iterm_r=255
    local iterm_g=255
    local iterm_b=255

    case $HOST in
        Jolla)
            iterm_r=30; iterm_g=159; iterm_b=30;
            COLORHOST="%F{028}$shorthost%f"
            ;;
        lea.local)
            iterm_r=0; iterm_g=171; iterm_b=255;
            COLORHOST="%F{045}$shorthost%f"
            ;;
        mia.local)
            iterm_r=0; iterm_g=171; iterm_b=32;
            COLORHOST="%F{046}$shorthost%f"
            ;;
        amy.local)
            COLORHOST="%F{027}$shorthost%f"
            ;;
        aly.local)
            iterm_r=226; iterm_g=105; iterm_b=255;
            COLORHOST="%F{213}$shorthost%f"
            ;;
        liz.dereferenced.net)
            iterm_r=108; iterm_g=148; iterm_b=255;
            COLORHOST="%F{075}$shorthost%f"
            ;;
        zac.vm)
            iterm_r=255; iterm_g=187; iterm_b=108;
            COLORHOST="%F{202}$shorthost%f"
            ;;
        *)
            COLORHOST=$HOST
            ;;
    esac

    # iterm2: proprietary codes to set tab color
    echo -n -e "\033]6;1;bg;red;brightness;$iterm_r\a"
    echo -n -e "\033]6;1;bg;green;brightness;$iterm_g\a"
    echo -n -e "\033]6;1;bg;blue;brightness;$iterm_b\a"

    case $USER in
        nemo)
            ;& # fallthrough
        burchr)
            COLORWHOAMI=""
            ;;
        root)
            COLORWHOAMI="%{$fg[white]$bg[red]%}$USER%{$reset_color%}@"
            ;;
        *)
            COLORWHOAMI="%{$fg[blue]$bg[yellow]%}$USER%{$reset_color%}@"
            ;;
    esac

    if [ -e "/etc/debian_chroot" ]; then
        CHROOT_PS1=":(chroot-$(cat /etc/debian_chroot))"
    else
        CHROOT_PS1=
    fi

    export PS1="$COLORWHOAMI$COLORHOST$CHROOT_PS1:%~%% "
}

preexec() {
    if [ $TERM = "xterm" ] || [ $TERM = "rxvt" ] || \
       [ $TERM = "xterm-color" ] || [ $TERM = "xterm-256color" ]; then
        if [ "$USER" = "burchr" ]; then
            print -Pn "\e]0;$1 (%m: %~)\a";
        else
            print -Pn "\e]0;$1 (%n@%m: %~)\a";
        fi
    fi
}

export WORDCHARS=''
export PATH=~/bin:~/bin/$PLATFORM:/usr/local/bin:/usr/local/sbin:/bin:/sbin:/usr/bin:/usr/sbin:/usr/games:~/code/go
export GOPATH=~/code/go
export EDITOR="vim"
export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"
export LC_MESSAGES="en_US.UTF-8"
export LC_CTYPE="en_US.UTF-8"
export LC_NUMERIC=C
export LC_COLLATE=C
export EMAIL="robin@viroteck.net"
export DEBEMAIL=$EMAIL
export MAKEOPTS='-j8'
export QT_MESSAGE_PATTERN="[%{if-debug}D%{endif}%{if-warning}W%{endif}%{if-critical}C%{endif}%{if-fatal}F%{endif}] %{category}: %{function}:%{line} - %{message}"

READNULLCMD=${PAGER:-/usr/bin/less}
which lesspipe >/dev/null 2>&1 && eval "$(lesspipe)"

if [[ "$PLATFORM" == "linux" ]]; then
    alias ls='ls -A --color=auto'
    alias lsl='ls -A --color=auto -l'
    alias open='xdg-open'
elif [[ "$PLATFORM" == 'osx' ]]; then
    alias ls='ls -A -G'
    alias lsl='ls -A -l -G'
    alias vim='mvim -v'
    export LSCOLORS=GxFxCxDxBxegedabagaced # get slightly less obnoxious coloring
fi


alias m='make'
alias pd='popd'
alias cl='clear && logout'
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

ffind() {
    find . -iname "*$1*"
}

# PRINT_EXIT_VALUE removed, we do that ourselves in precmd
setopt GLOB EXTENDED_GLOB MAGIC_EQUAL_SUBST RC_EXPAND_PARAM \
       HIST_EXPIRE_DUPS_FIRST HIST_IGNORE_DUPS HIST_VERIFY CORRECT HASH_CMDS \
       RC_QUOTES AUTO_CONTINUE MULTIOS VI INC_APPEND_HISTORY \
       APPENDHISTORY INTERACTIVE_COMMENTS autopushd prompt_subst
unsetopt beep
unset MAIL

REPORTTIME=10
HISTFILE=~/.zsh/history
HISTSIZE=5000
SAVEHIST=5000

bindkey -e

bindkey '^[[H' beginning-of-line
bindkey '^A' beginning-of-line
bindkey '^[[F' end-of-line
bindkey '^E' end-of-line
bindkey -M viins '^R' history-incremental-search-backward
bindkey -M vicmd '^R' history-incremental-search-backward

# lea's preferred escapes
bindkey '^[[1;5D' backward-word
bindkey '^[[1;5C' forward-word

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

# make git completion not be so ridiculously slow
__git_files () {
    _wanted files expl 'local files' _files
}

function ccd() {
    if [ -z $1 ]; then
        cd ~/code
        return 0
    else
        local software="$1"
        cd `find ~/code -maxdepth 2 -path "*/$software"`
    fi
}

function viyaml() {
    if [ -z "$1" ]; then
        vim rpm/*.yaml
        if [ $? == 0 ]; then
            (cd rpm; specify)
        fi
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

function vicron() {
    vim ~/bin/gencron
    ~/bin/gencron | crontab
    echo "$0: installed cron"
}

#which tmux >/dev/null 2>&1
#if [[ -z "$TMUX" && $? -eq 0 ]]; then
#    tmux list-sessions >/dev/null 2>&1
#    if [ $? -eq 0 ]; then
#        tmux a
#    fi
#fi

(nohup git pull >/dev/null 2>&1 &)
grep kien ~/.git/modules/.vim/bundle/ctrlp.vim/config >/dev/null 2>&1
if [[ $? -eq 0 ]]; then
    # changing the origin of the module seems to have made git unhappy..
    (nohup git submodule deinit -f . >/dev/null 2>&1 &)
    (nohup rm -rf .git/modules >/dev/null 2>&1 &)
fi
(nohup git submodule init >/dev/null 2>&1 &)
(nohup git submodule update >/dev/null 2>&1 &)

# set up ssh key. we do this if it's a symlink always, so there's no chance of
# it becoming stale. we also try link if there is no pubkey in the case of a
# freshly restored host.
if [[ -L ~/.ssh/id_rsa.pub || ! -f ~/.ssh/id_rsa.pub ]]; then
    unlink ~/.ssh/id_rsa.pub 2>/dev/null

    if [ -f ~/.ssh/pubkeys/$HOST.pub ]; then
        ln -s ~/.ssh/pubkeys/$HOST.pub ~/.ssh/id_rsa.pub
    else
        echo "WARNING: No such SSH pubkey for: " $HOST
    fi
fi

if [ -f ~/.ssh/hosts/$HOST.sh ]; then
    source ~/.ssh/hosts/$HOST.sh
fi

