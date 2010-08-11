" give terminals our title.
set title
" let ~ be used as an action (e.g. ~e to change case of one word)
set tildeop
" don't be compatible with vi
set nocompatible
" enable incremental search
set incsearch
" wrap lines for search
set wrapscan
" ignore case.
set ignorecase
" don't save .swp files all the time.
set nobackup
" show command results as they happen
set showcmd
" decent newlines support.
set ffs=unix,dos,mac

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" GRAPHICAL SETTINGS
colorscheme mytheme
" terminal colors!
set t_Co=256
" hilight current column/line
set cursorcolumn
set cursorline
" custom statusbar..
set statusline=%t%h%m%r%=[%b\ 0x%02B]\ \ \ %l,%c%V\ %P
" always set a status line on windows
set laststatus=2
" always show ruler on windows
set ruler
" make backspace behave almost human (allow backspacing over lines, etc.)
set backspace=2
" allow mouse usage everywhere(supersceded by F8 rule)
" set mouse=a
" allow cursor to roam everywhere. (all breaks hl with tabs.. :()
set virtualedit=onemore
" visible tabs, useful. i have removed eol:$.
set list
set listchars=tab:>-,trail:.,extends:#
" hilight search matches
set hlsearch

" menu to select stuff when tab completing filenames (e.g.)
set wildmenu
set wildchar=<tab> " tab complete ^
set wildmode=longest:full,full " how big the list should be


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" TAB SETTINGS
" tabs = 4 spaces
set tabstop=4
set softtabstop=4
set shiftwidth=4
" treat tabs like spaces.
set expandtab
" we want to be able to expand folds
set foldcolumn=1

" for gui edit mode
if has("gui_running")
    set guifont=Consolas\ 10
endif

source ~/.vim/vimrc/hex_hilight.vimrc
source ~/.vim/vimrc/coding_rules.vimrc

