# Preserve inherited toolchain precedence before global settings reorder PATH.
local -a rb_inherited_path=("${path[@]}")

# Load global settings (if any).
source /etc/profile
umask 022
autoload colors && colors
setopt prompt_subst
zmodload zsh/pcre &>/dev/null

# Set a platform var, so my own scripts can easier handle platform differences.
export RB_PLATFORM='unknown'
local unamestr=$(uname)
if [[ "$unamestr" == 'Linux' ]]; then
   export RB_PLATFORM='linux'
elif [[ "$unamestr" == 'Darwin' ]]; then
   export RB_PLATFORM='osx'
else
   echo "warning: platform unknown"
fi

# Set the terminal title, if the terminal likely supports it.
if [[ "$TERM" == (Eterm*|alacritty*|aterm*|gnome*|konsole*|kterm*|putty*|rxvt*|screen*|tmux*|xterm*) ]]; then
    rb_do_set_xterm_title=1
fi

# This magical function is run before every prompt. Used for the little
# niceties in life like setting a pretty PS1.
function precmd() {
    # must be done early to save status
    local exit_status=$?

    if [[ "$rb_do_set_xterm_title" -eq 1 ]]; then
        print -Pn -- '\e]2;%n@%m %~\a'
        [[ "$TERM" == 'screen'* ]] && print -Pn -- '\e_\005{g}%n\005{-}@\005{m}%m\005{-} \005{B}%~\005{-}\e\\'
    fi

    # a nicer replacement for PRINT_EXIT_VALUE
    if [ $exit_status -ne 0 ]; then
        echo "zsh: exit $fg[red]$exit_status$reset_color";
    fi

    # Set up git author info without me having to edit git config in each repo.
    # Could be done globally, but left in precmd so it can be overridden per directory if wanted.
    export GIT_AUTHOR_EMAIL="robin.burchell@crimson.no"
    export GIT_COMMITTER_EMAIL="robin.burchell@crimson.no"

    # do this again to make sure it's up to date.
    local shorthost=$(echo "$HOST" | cut -d'.' -f1)

    local iterm_r=255
    local iterm_g=255
    local iterm_b=255

    # Bright hostname text with matching softer iTerm2 tab colours.
    # Preview the full palette with: host-colour-preview
    case ${shorthost} in
        jamie) # Sky
            iterm_r=80; iterm_g=158; iterm_b=207;
            COLORHOST="%F{81}$shorthost%f" ;;
        eli) # Lavender
            iterm_r=159; iterm_g=132; iterm_b=202;
            COLORHOST="%F{183}$shorthost%f" ;;
        mia) # Mint
            iterm_r=92; iterm_g=176; iterm_b=133;
            COLORHOST="%F{121}$shorthost%f" ;;
        tia) # Amber
            iterm_r=205; iterm_g=158; iterm_b=65;
            COLORHOST="%F{221}$shorthost%f" ;;
        clanker) # Coral
            iterm_r=224; iterm_g=112; iterm_b=112;
            COLORHOST="%F{210}$shorthost%f" ;;
        rm-builder) # Teal
            iterm_r=62; iterm_g=166; iterm_b=166;
            COLORHOST="%F{80}$shorthost%f" ;;

        # Spare palettes: uncomment a block and replace your-host with its hostname.
        # your-host) # Rose
        #     iterm_r=204; iterm_g=126; iterm_b=164;
        #     COLORHOST="%F{218}$shorthost%f" ;;
        # your-host) # Peach
        #     iterm_r=213; iterm_g=151; iterm_b=108;
        #     COLORHOST="%F{216}$shorthost%f" ;;
        # your-host) # Periwinkle
        #     iterm_r=115; iterm_g=138; iterm_b=208;
        #     COLORHOST="%F{111}$shorthost%f" ;;
        # your-host) # Lime
        #     iterm_r=158; iterm_g=181; iterm_b=79;
        #     COLORHOST="%F{155}$shorthost%f" ;;
        # your-host) # Orchid
        #     iterm_r=182; iterm_g=115; iterm_b=193;
        #     COLORHOST="%F{177}$shorthost%f" ;;
        # your-host) # Ice
        #     iterm_r=118; iterm_g=181; iterm_b=191;
        #     COLORHOST="%F{159}$shorthost%f" ;;
        # your-host) # Sage
        #     iterm_r=139; iterm_g=166; iterm_b=116;
        #     COLORHOST="%F{151}$shorthost%f" ;;
        # your-host) # Terracotta
        #     iterm_r=187; iterm_g=117; iterm_b=87;
        #     COLORHOST="%F{173}$shorthost%f" ;;
        # your-host) # Steel
        #     iterm_r=111; iterm_g=145; iterm_b=176;
        #     COLORHOST="%F{110}$shorthost%f" ;;
        # your-host) # Sand
        #     iterm_r=184; iterm_g=165; iterm_b=125;
        #     COLORHOST="%F{223}$shorthost%f" ;;
        # your-host) # Raspberry
        #     iterm_r=198; iterm_g=93; iterm_b=135;
        #     COLORHOST="%F{204}$shorthost%f" ;;
        # your-host) # Silver
        #     iterm_r=157; iterm_g=165; iterm_b=177;
        #     COLORHOST="%F{252}$shorthost%f" ;;

        *)
            COLORHOST=$HOST ;;
    esac

    # Keep iTerm2's tab-colour escapes out of other terminals and multiplexers.
    if [[ ${LC_TERMINAL:-} == iTerm2 && -z ${TMUX:-} && -z ${STY:-} &&
          $TERM != (screen*|tmux*) ]]; then
        printf '\033]6;1;bg;red;brightness;%s\a' "$iterm_r"
        printf '\033]6;1;bg;green;brightness;%s\a' "$iterm_g"
        printf '\033]6;1;bg;blue;brightness;%s\a' "$iterm_b"
    fi

    # Add a pretty username to the PS1 too.
    case ${USER} in
        burchr)
            COLORWHOAMI="" ;;
        root)
            # Pale rose on dark burgundy: prominent, danger sign
            COLORWHOAMI="%B%F{224}%K{52} $USER %k%f%b@" ;;
        *)
            # Charcoal on soft amber distinguishes other accounts
            COLORWHOAMI="%F{235}%K{180} $USER %k%f@" ;;
    esac

    export PS1="${COLORWHOAMI}${COLORWHEREAMI}${COLORHOST}${CHROOT_PS1:+(${CHROOT_PS1})}:%~%% "
}

# This function is executed when a command is read, before it is run.
function preexec() {
    if [[ "$rb_do_set_xterm_title" -eq 1 ]]; then
        print -Pn -- '\e]2;%n@%m %~ %# ' && print -n -- "${(q)1}\a"
        [[ "$TERM" == 'screen'* ]] && { print -Pn -- '\e_\005{g}%n\005{-}@\005{m}%m\005{-} \005{B}%~\005{-} %# ' && print -n -- "${(q)1}\e\\"; }
    fi
}

export WORDCHARS=''

# Rebuild PATH from existing directories in the supplied order.
build_path() {
    local dir
    path=()
    for dir in "$@"; do
        if [[ -d "$dir" ]]; then
            path+=("$dir")
        fi
    done
    export PATH
}

export GOPATH=~/src/go
typeset -U path
build_path "${rb_inherited_path[@]}" \
    "$GOPATH/bin" \
    "$HOME/.local/bin/" \
    "$HOME/.local/bin/$RB_PLATFORM" \
    "$HOME/.cargo/bin"
unset rb_inherited_path

if [[ "$RB_PLATFORM" == "osx" ]]; then
    build_path "${path[@]}" \
        /opt/homebrew/bin /opt/homebrew/sbin \
        /Library/Apple/usr/bin
fi

build_path "${path[@]}" \
    /usr/local/bin /usr/local/sbin \
    /bin /sbin /usr/bin /usr/sbin \
    "$HOME/.nix-profile/bin" \
    /run/current-system/sw/bin \
    /nix/var/nix/profiles/default/bin \
    /pkg/env/global/bin

if [[ "$RB_PLATFORM" == "osx" ]]; then
    # Keep Cryptex paths even when absent, as macOS may populate them later.
    path+=(
        /System/Cryptexes/App/usr/bin
        /var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/local/bin
        /var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/bin
        /var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/appleinternal/bin
    )
fi

export EDITOR="e"
export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"
export LC_MESSAGES="en_US.UTF-8"
export LC_CTYPE="en_US.UTF-8"
export LC_NUMERIC=C
export LC_COLLATE=C
export EMAIL="robin@viroteck.net"
export QT_MESSAGE_PATTERN="%{time process} [%{if-debug}D%{endif}%{if-info}I%{endif}%{if-warning}W%{endif}%{if-critical}C%{endif}%{if-fatal}F%{endif}] %{category}: %{function}:%{line} - %{message}"
export CMAKE_GENERATOR=Ninja
export CTEST_PROGRESS_OUTPUT=1
export CMAKE_EXPORT_COMPILE_COMMANDS=1
export RUNFILE_ROOTS=~
export TSAN_OPTIONS="suppressions=$HOME/.tsan-suppressions.txt"

READNULLCMD=${PAGER:-/usr/bin/less}
which lesspipe >/dev/null 2>&1 && eval "$(lesspipe)"

if [[ "$RB_PLATFORM" == "linux" ]]; then
    alias ls='ls -A --color=auto'
    alias lsl='ls -A --color=auto -l'
    alias e="$EDITOR"
elif [[ "$RB_PLATFORM" == "osx" ]]; then
    # Matching ANSI completion and native macOS ls file-type palettes.
    export LS_COLORS="di=01;34:ln=01;36:so=01;35:pi=40;33:ex=01;32:bd=40;33;01:cd=40;33;01:su=37;41:sg=30;43:tw=30;42:ow=34;42:st=37;44"
    export LSCOLORS="ExGxFxdaCxDaDahbadacecah"
    alias ls='ls -G'
    alias lsl='ls -Gl'
fi

# retrain my mental habits
alias vi="echo 'Use the right command: e' && sleep 5 && e"
alias vim="echo 'Use the right command: e' && sleep 5 && e"
alias xdg-open="echo 'Use the right command: open' && sleep 5 && open"

alias gp='git push'
alias gpr='git pull --rebase'
alias gci='git commit'
alias gcia='git commit -a'
alias gco='git checkout'
alias gl='git log'

devshell() {
    # We set SHELL because nix develop will overwrite it to its own bash, even with -c zsh.
    # That will be annoying if we want to 'nix shell' inside a devshell, as that uses $SHELL by default.
    CHROOT_PS1="$1" nix develop "$HOME/nix-config#$1" -c zsh -s SHELL "/bin/zsh"
}

if [[ "$TERM" == "xterm-kitty" ]]; then
    alias icat="kitty +kitten icat --align=left"
else
    alias icat="echo 'icat not available without kitty :('"
fi

setopt GLOB EXTENDED_GLOB MAGIC_EQUAL_SUBST RC_EXPAND_PARAM \
       HIST_EXPIRE_DUPS_FIRST HIST_IGNORE_DUPS HIST_VERIFY CORRECT HASH_CMDS \
       RC_QUOTES AUTO_CONTINUE MULTIOS VI \
       APPENDHISTORY INTERACTIVE_COMMENTS autopushd prompt_subst

# Import new commands from the history file in other shell instances
setopt share_history

unsetopt beep
unset MAIL

REPORTTIME=10
HISTFILE=~/.zsh/history
HISTSIZE=500000
SAVEHIST=500000


bindkey -e

# set up keys for basic navigation. sigh...
# NB, to use this on Mac, you need to go to Keyboard settings, shortcuts tab &
# reconfigure "move left/right a space" to something else.
if [[ "$RB_PLATFORM" == "osx" ]]; then
    bindkey '^[[1;3D' backward-word
    bindkey '^[[1;3C' forward-word

    bindkey '^[[H' beginning-of-line # home
    bindkey '^[[F' end-of-line # home
else
    bindkey '^[[1;5D' backward-word
    bindkey '^[[1;5C' forward-word
    bindkey '^[OH' beginning-of-line # home
    bindkey '^[OF' end-of-line # home
fi



bindkey '^[[H' beginning-of-line
bindkey '^A' beginning-of-line
bindkey '^[[F' end-of-line
bindkey '^E' end-of-line
bindkey '^[[1~' beginning-of-line
bindkey '^[[4~' end-of-line
bindkey -M viins '^R' history-incremental-search-backward
bindkey -M vicmd '^R' history-incremental-search-backward
bindkey "^[[3~" delete-char

source ~/.zsh/compinstall

# Work around double-escaping in zsh's _remote_files completion helper.
# Legacy scp passed remote paths through a second shell, so completion
# escaped spaces twice (foo\\\ bar). Since OpenSSH 9.0, scp uses SFTP by
# default and no longer needs that second layer; the extra backslash can
# become part of the filename and cause transfers to fail.
#
# macOS's bundled zsh 5.9 still contains the old completion code. Patch
# the loaded function to let compadd escape filenames once, and escape
# directory prefixes correctly when requesting further remote matches.
# This also affects other commands using _remote_files, including rsync.
#
# To check: start `zsh -f`, run `autoload +X _remote_files`, then inspect
# `functions _remote_files`. The compadd calls should no longer contain
# ${(q)remdispf...} or ${(q)remdispd...}, and rempat should quote PREFIX
# with ${(q)PREFIX...}. Test remote completion with this workaround
# disabled before removing it permanently.
#
# Upstream patch:
# https://www.zsh.org/mla/workers/2024/msg00446.html
autoload +X _remote_files
functions[_remote_files]=${functions[_remote_files]//'${(q)remdispf'/'${remdispf'}
functions[_remote_files]=${functions[_remote_files]//'${(q)remdispd'/'${remdispd'}
functions[_remote_files]=${functions[_remote_files]//'${PREFIX%%'/'${(q)PREFIX%%'}

# make git completion not be so ridiculously slow
__git_files () {
    _wanted files expl 'local files' _files
}

# make tab completion case insensitive
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'

function ezsh() {
    e ~/.zshrc
    zsh -n ~/.zshrc

    if [ $? -eq 0 ]; then
        source ~/.zshrc
    else
        echo "$0: syntax error, not reloading"
    fi
}

nohup "$HOME/.local/bin/dotfiles-autosync" --update </dev/null >/dev/null 2>&1 &!

if [ -f ~/.ssh/hosts/$HOST.sh ]; then
    source ~/.ssh/hosts/$HOST.sh
fi

if [ -f /usr/share/fzf/shell/key-bindings.zsh ]; then
    source /usr/share/fzf/shell/key-bindings.zsh
fi

if [ -f /usr/share/zsh/site-functions/fzf ]; then
    source /usr/share/zsh/site-functions/fzf
fi
