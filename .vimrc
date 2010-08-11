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
" CODE SETTINGS
" insert comment leader on return and let gq format comments
set formatoptions=rqtc
" auto-indent everything
set autoindent
" show line numbers
set number
" smart indent!
set cindent
" enable syntax hilighting
syntax on
" show matching brackets
set showmatch
" how long in tenths of seconds to show them
set matchtime=5
" wordwrap
set tw=80
" Show trailing whitespace and spaces before tabs
hi link localWhitespaceError Error
au Syntax * syn match localWhitespaceError /\(\zs\%#\|\s\)\+$/ display
au Syntax * syn match localWhitespaceError / \+\ze\t/ display

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" TAB SETTINGS
" tabs = 4 spaces
set tabstop=4
" tabs = 4 spaces
set softtabstop=4
" tabs = 4 spaces
set shiftwidth=4
" treat tabs like spaces.
set expandtab
" we want to be able to expand folds
set foldcolumn=1

" for gui edit mode
if has("gui_running")
    set guifont=Consolas\ 10
endif


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" TAB COMPLETION
function MyTabOrComplete()
	let col = col('.')-1
		  if !col || getline('.')[col-1] !~ '\k'
		return "\<tab>"
	else
		return "\<C-N>"
	endif
endfunction
inoremap <Tab> <C-R>=MyTabOrComplete()<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" COMPILE HOTKEY
map <silent> <f5> :!make<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" filetype rules
" automatically insert "#!/bin/sh" line for *.sh files
function! <SID>openSh()
    :call setline(1, "#!/bin/sh")
    :call append(1, "")
    exe 2
endfunction
au BufEnter *.sh if getline(1) == "" | :call s:openSh() | endif
" automatically give executable permissions if file begins with #!/bin/sh
au BufWritePost * if getline(1) =~ "^#!/bin/[a-z]*sh" | silent !chmod a+x <afile> | endif

" different reformatting rules
" always useful, always enabled
inoremap , ,<Space>
inoremap " ""<Left>

function! <SID>simpleCodingRules()
    " {<enter> becomes: {<enter><tab>*cursor here*}
    " good for if statements, functions, whatever. :>
    inoremap { {<CR>}<Esc>kA<CR>
    inoremap ( ()<Left>
    inoremap [ []<Left>

    " escape from stuff.
    " TODO: would be nice if we could insert one if one didn't exist.
    inoremap ) <Esc>:call search(')', "W")<CR>a
    inoremap } <Esc>:call search('}', "W")<CR>a
    inoremap ] <Esc>:call search(']', "W")<CR>a
    " too annoying, can't < or > in if statements easily etc.
    "inoremap > <Esc>:call search('>', "W")<CR>a
endfunction

" these are done seperately to avoid annoyance when writing emails
au BufEnter *.cpp :call s:simpleCodingRules()
au BufEnter *.h :call s:simpleCodingRules()
au BufEnter *.py :call s:simpleCodingRules()
au BufEnter *.rb :call s:simpleCodingRules()
au BufEnter *.php :call s:simpleCodingRules()
au BufEnter *.sh :call s:simpleCodingRules()


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Theme: wombat. (tweaked a bit.)
" Maintainer:	Lars H. Nielsen (dengmao@gmail.com)
" Last Change:	January 22 2007

set background=dark

hi clear

if exists("syntax_on")
syntax reset
endif

let colors_name = "wombat"


" Vim >= 7.0 specific colors
if version >= 700
hi CursorLine guibg=#2d2d2d
hi CursorColumn guibg=#2d2d2d
hi MatchParen guifg=#f6f3e8 guibg=#857b6f gui=bold
hi Pmenu		guifg=#f6f3e8 guibg=#444444
hi PmenuSel		guifg=#000000 guibg=#cae682
endif

" General colors
hi Cursor			guifg=NONE    guibg=#656565 gui=none
hi Normal			guifg=#f6f3e8 guibg=#242424 gui=none
hi NonText		guifg=#808080 guibg=#303030 gui=none
hi LineNr			guifg=#857b6f guibg=#000000 gui=none
hi StatusLineNC guifg=#857b6f guibg=#444444 gui=none
hi StatusLine		guifg=#f6f3e8 guibg=#444444 gui=none
""hi StatusLine		guifg=#f6f3e8 guibg=#444444 gui=italic
hi VertSplit	guifg=#444444 guibg=#444444 gui=none
hi Folded			guibg=#384048 guifg=#a0a8b0 gui=none
hi Title				 guifg=#f6f3e8 guibg=NONE gui=bold
hi Visual        guifg=#f6f3e8 guibg=#444444 gui=none
hi SpecialKey    guifg=#808080 guibg=#343434 gui=none

" Syntax highlighting

""hi Comment		guifg=#99968b gui=italic
""hi Todo			guifg=#8f8f8f gui=italic
hi Comment		guifg=#99968b gui=none
hi Todo			guifg=#8f8f8f gui=none
hi Constant		guifg=#e5786d gui=none
hi Identifier		guifg=#cae682 gui=none
""hi String			guifg=#95e454 gui=italic
hi String			guifg=#95e454 gui=none
hi Function		guifg=#cae682 gui=none
hi Type			guifg=#cae682 gui=none
hi Statement	guifg=#8ac6f2 gui=none
hi keyword    guifg=#8ac6f2 gui=none
hi PreProc		guifg=#e5786d gui=none
hi number     guifg=#e5786d gui=none
hi Special    guifg=#e7f6da gui=none


" Adapted from:
"  gvim plugin for highlighting hex codes to help with tweaking colors
"  by Yuri Feldman <feldman.yuri1@gmail.com>
"  License: WTFPL - Do What The Fuck You Want To Public License.
"           Email me if you'd like.
" See also: http://www.vim.org/scripts/script.php?script_id=2937
let s:HexColoredEnabled = 1
let s:HexColors = []
let s:nextHexGroup = 10

function! HexRemoveAllHighlights()
    for hexColor in s:HexColors
        exe 'highlight clear '.hexColor
    endfor
    call clearmatches()
endfunction

function! HexHighlightLineNumber(lineNumber)
    let currentLine = getline(a:lineNumber)
    let hexLineMatch = 1
    while match(currentLine, '#\x\{6}', 0, hexLineMatch) != -1
        let hexMatch = matchstr(currentLine, '#\x\{6}', 0, hexLineMatch)
        exe 'hi hexColor'.s:nextHexGroup.' guibg='.hexMatch
        exe 'let m = matchadd("hexColor'.s:nextHexGroup.'", "'.hexMatch.'", 25, '.s:nextHexGroup.')'
        let s:HexColors += ['hexColor'.s:nextHexGroup]
        let s:nextHexGroup += 1
        let hexLineMatch += 1
    endwhile
endfunction

function! HexHighlightCurrentLine()
    if has("gui_running")
        if s:HexColoredEnabled == 1
            " check for hilights on the current line
            call HexHighlightLineNumber(line("."))
        endif
    endif
endfunction

function! HexHighlightWholeFile()
    if has("gui_running")
        " clear any possibly existing hilight first
        call HexRemoveAllHighlights()
        
        " now away we go
        if s:HexColoredEnabled == 1
            let lineNumber = 0
            while lineNumber <= line("$")
                call HexHighlightLineNumber(lineNumber)
                let lineNumber += 1
            endwhile
            unlet lineNumber
        endif
    endif
endfunction

" rehilight current line each keystroke
autocmd CursorMoved,CursorMovedI * call HexHighlightCurrentLine()

" and rehilight the whole file on entry
autocmd BufEnter * call HexHighlightWholeFile()

function! EnableHexHighlight()
    echo "Highlighting hex colors..."
    let s:HexColoredEnabled = 1
    call HexHighlightWholeFile()
endfunction

function! DisableHexHighlight()
    echo "Unhighlighting hex colors..."
    let s:HexColoredEnabled = 0
    call HexRemoveAllHighlights()
endfunction
