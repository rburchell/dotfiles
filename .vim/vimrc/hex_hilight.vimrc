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
