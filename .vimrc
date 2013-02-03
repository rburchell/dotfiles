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
if version >= 700
    " open new files in tabs
    tab all
endif

" when loading a file, cd to the right place
if exists('+autochdir')
  set autochdir
else
  autocmd BufEnter * silent! lcd %:p:h:gs/ /\\ /
endif

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" GRAPHICAL SETTINGS
colorscheme xoria256
" terminal colors!
set t_Co=256

if version >= 700
    " hilight current column/line
    set cursorcolumn
    set cursorline
endif

" custom statusbar..
set statusline=%t%h%m%r%=[%b\ 0x%02B]\ \ \ %l,%c%V\ %P
" always set a status line on windows
set laststatus=2
" always show ruler on windows
set ruler
" make backspace behave almost human (allow backspacing over lines, etc.)
set backspace=2

if version >= 700
    " allow cursor to roam everywhere. (all breaks hl with tabs.. :()
    set virtualedit=onemore
endif

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

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" FIREFOX-LIKE TAB NAVIGATION
:nmap <C-S-tab> :tabprevious<CR>
:nmap <C-w> :tabclose<CR>
:nmap <C-tab> :tabnext<CR>
:map <C-S-tab> :tabprevious<CR>
:map <C-w> :tabclose<CR>
:map <C-tab> :tabnext<CR>
:imap <C-S-tab> <Esc>:tabprevious<CR>i
:imap <C-w> :tabclose<CR>i
:imap <C-tab> <Esc>:tabnext<CR>i
:nmap <C-t> :tabnew .<CR>
:imap <C-t> <Esc>:tabnew .<CR>

" for gui edit mode
if has("gui_running")
    set guifont=Consolas\ 10
    set guioptions=em
endif
set mousemodel=popup

if version >= 700
    source ~/.vim/vimrc/hex_hilight.vimrc
endif
source ~/.vim/vimrc/coding_rules.vimrc
source ~/.vim/syntax/qml.vim

let g:session_autoload='yes'
let g:session_autosave='yes'

set sessionoptions-=resize
set sessionoptions-=winpos
set sessionoptions-=winsize

" map delete/copy/paste to the system keyboard for ease of use
vnoremap y "+y
vnoremap p "+gP
vnoremap d "+x

nnoremap y "+y
nnoremap yy V"+y
nnoremap dd V"+x
nnoremap d "+x
nnoremap p "+gP

