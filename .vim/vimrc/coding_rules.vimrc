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

if version >= 700
    " Show trailing whitespace and spaces before tabs
    hi link localWhitespaceError Error
    au Syntax * syn match localWhitespaceError /\(\zs\%#\|\s\)\+$/ display
    au Syntax * syn match localWhitespaceError / \+\ze\t/ display
endif

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" TAB COMPLETION
function! MyTabOrComplete()
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

command! DoCompile call DoCompile()
function! DoCompile()
    :wa
    :make!
endfunction

map <silent> <f5> :DoCompile<CR>
inoremap <silent> <f5> <C-R>=DoCompile()<CR>
"map <silent> <f5> :!make<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" filetype rules
" automatically insert "#!/bin/sh" line for *.sh files
function! <SID>openSh()
    :call setline(1, "#!/bin/sh")
    :call append(1, "")
    exe 2
endfunction
au BufEnter *.sh if getline(1) == "" | :call s:openSh() | endif

" automatically give executable permissions if it looks like a good candidate
function! MySetExecutableIfScript(line1, current_file)
    if a:line1 =~ '^#!\(/usr\)*/bin/'
        set autoread
        silent !chmod +x %
        set autoread<
    endif
endfunction
autocmd BufWritePost * call MySetExecutableIfScript(getline(1), expand("%:p"))

" add a space after a comma
inoremap , ,<Space>

