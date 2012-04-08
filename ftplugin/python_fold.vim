" Yet another folding expression for Python
" Author: Akinori Hattori <hattya@gmail.com>
" License: MIT

if exists('b:undo_ftplugin')
  let b:undo_ftplugin .= ' | '
else
  let b:undo_ftplugin = ''
endif
let b:undo_ftplugin .= 'setl fdm< fde< fdt<' .
\                      ' | ' .
\                      'unlet! b:python_fold_changedtick b:python_fold_level'

setlocal foldmethod=expr
setlocal foldexpr=PythonFold(v:lnum)
setlocal foldtext=PythonFoldText()

if exists('*PythonFold')
  finish
endif

let b:python_fold_changedtick = 0
let b:python_fold_level = {}

" constant: Python
let s:maxindent = 100
let s:tabsize = 8

" regex
let s:blank = '\v^\s*$'
let s:comment = '\v^\s*#'
let s:backslash = '\v\\$'
let s:decl = '\v^\s*%(class|def)%(\s.*|\s*\\)$'
let s:colon = '\v:\s*%(#.*)=$'
let s:deco = '\v^\s*\@'

function! PythonFold(lnum)
  if !get(b:, 'python_fold_changedtick') || b:changedtick != b:python_fold_changedtick
    " clear
    let b:python_fold_changedtick = b:changedtick
    let b:python_fold_level = {}
    " parse buffer
    let [lnum, eof] = [1, line('$')]
    let fold = {'lv': 0,
    \           'stack': [[]]}
    let tok = {'tabsize': s:tabsize,
    \          'indent': 0,
    \          'indstack': [0],
    \          'alttabsize': 1,
    \          'altindstack': [0]}
    while lnum <= eof
      let line = getline(lnum)
      if line =~# s:blank
        " blank
        call add(fold.stack[fold.lv], ['blank', lnum, lnum])
      elseif line =~# s:comment
        " comment
        call add(fold.stack[fold.lv], ['comment', lnum, lnum])
      else
        " logical line
        let [llbeg, llend] = [lnum, s:logical_line_end(lnum)]
        if !llend | break | endif
        " indent level
        let prev_indent = tok.indent
        if s:indent(tok, llbeg) | break | endif
        " fold level
        let fold.lv = tok.indent
        " dedent
        if tok.indent < prev_indent
          " remove last blank lines
          while 0 < len(fold.stack[-1]) && fold.stack[-1][-1][0] =~# '\v^%(blank|comment)$'
            call remove(fold.stack[-1], -1)
          endwhile
        endif

        let new_area = []
        if line =~# s:decl
          " class or function declare
          let type = 'declare'
          " search decorator
          let i = len(fold.stack[fold.lv]) - 1
          while 0 <= i && fold.stack[fold.lv][i][0] =~# '\v^%(comment|decorator)$'
            let i -= 1
          endwhile
          let i += 1
          if i < len(fold.stack[fold.lv])
            while fold.stack[fold.lv][i][0] ==# 'comment'
              let i += 1
            endwhile
            if fold.stack[fold.lv][i][0] == 'decorator'
              let new_area = remove(fold.stack[fold.lv], i, len(fold.stack[fold.lv]) - 1)
            endif
          endif
          if getline(llend) =~# s:colon | let fold.lv += 1 | endif
        elseif line =~# s:deco
          " decorator
          let type = 'decorator'
        elseif getline(llend) =~# s:colon
          " clause
          let type = 'clause'
          let fold.lv += 1
        else
          " statement
          let type = 'statement'
          if len(fold.stack) - 1 < fold.lv
            " unexpected indent
            echomsg 'E_INDENT:'.llbeg
            break
          endif
        endif

        " fold: logical line
        if llbeg < llend
          let b:python_fold_level[llbeg] = '>'.(fold.lv + 1)
          let b:python_fold_level[llend] = '<'.(fold.lv + 1)
        endif
        " fold: indent level
        if tok.indent < fold.lv
          " fold start
          if len(fold.stack) == fold.lv
            call add(fold.stack, new_area)
          else
            " fold end
            let b:python_fold_level[fold.stack[-1][-1][2]] = '<'.fold.lv
            while fold.lv < len(fold.stack) - 1
              call remove(fold.stack, -1)
            endwhile
            let fold.stack[fold.lv] = []
          endif
          let lnum = !len(new_area) ? llbeg : new_area[0][1]
          " fold: decorator
          if llbeg != lnum && has_key(b:python_fold_level, lnum)
            let i = 0
            while i < len(fold.stack[-1])
              if fold.stack[-1][i][1] == lnum
                let lv = get(b:python_fold_level, fold.stack[-1][i][2], '')
                if lv != ''
                  let b:python_fold_level[fold.stack[-1][i][2]] = '<'.(lv[1 :] + 1)
                endif
              endif
              let i += 1
            endwhile
          endif
          let b:python_fold_level[lnum] = '>'.(fold.lv + has_key(b:python_fold_level, lnum))
        elseif fold.lv < len(fold.stack) - 1
          " fold end
          let b:python_fold_level[fold.stack[-1][-1][2]] = '<'.(fold.lv + 1)
          while fold.lv < len(fold.stack) - 1
            call remove(fold.stack, -1)
          endwhile
          let fold.stack[fold.lv] = []
        endif

        call add(fold.stack[fold.lv], [type, llbeg, llend])
        let lnum = llend
      endif
      let lnum += 1
    endwhile
  endif

  return get(b:python_fold_level, a:lnum, '=')
endfunction

function! PythonFoldText()
  let text = foldtext()
  return foldtext()
endfunction

function! s:logical_line_end(lnum)
  let [lnum, eof] = [a:lnum, line('$')]
  let col = 0
  while lnum <= eof
    let line = getline(lnum)
    if line =~# s:blank || line =~# s:comment
      " blank or comment
      break
    elseif line =~# s:backslash
      " line continuation
      let lnum += 1
      let col = 0
    else
      " paren
      let paren = 0
      while 1
        let new_col = matchend(line, '\V(\|{\|[', col)
        if new_col == -1 | break | endif
        let col = new_col
        if s:is_statement(lnum, col)
          " search paren
          let pos = s:search_paren(lnum, col, 'Wn')
          if pos == [0, 0]
            " paren mismatch
            return 0
          elseif lnum < pos[0]
            let paren = 1
            let [lnum, col] = pos
            break
          else
            let col = pos[1]
          endif
        endif
      endwhile
      if !paren
        " triple-quoted string
        let qqq = 0
        while 1
          let new_col = matchend(line, '\v%("{3}|''{3})', col)
          if new_col == -1 | break | endif
          let col = new_col
          if s:is_statement(lnum, col)
            " search triple-quoted string
            let pos = s:search_qqq_string(lnum, col, line[col - 3 : col - 1], 'Wn')
            if pos == [0, 0]
              " triple-quote mismatch
              return 0
            elseif lnum < pos[0]
              let qqq = 1
              let [lnum, col] = pos
              break
            else
              let col = pos[1]
            endif
          endif
        endwhile
        if !qqq | break | endif
      endif
    endif
  endwhile
  return lnum
endfunction

function! s:is_statement(lnum, col)
  return synIDattr(synID(a:lnum, a:col, 1), 'name') !~? '\v%(Comment|String)$'
endfunction

function! s:search_paren(lnum, col, flags)
  " save cursor
  let pos = getpos('.')
  " search paren
  call cursor(a:lnum, a:col)
  let rv = searchpairpos('\V(\|{\|[', '', '\V]\|}\|)', a:flags, '!s:is_statement(line("."), col("."))')
  " restore cursor
  call setpos('.', pos)
  return rv
endfunction

function! s:search_qqq_string(lnum, col, pattern, flags)
  " save cursor
  let pos = getpos('.')
  " search triple-quoted string
  call cursor(a:lnum, a:col)
  let rv = searchpos(a:pattern, a:flags)
  " restore cursor
  call setpos('.', pos)
  return rv
endfunction

function! s:indent(tok, lnum)
  let [col, altcol] = [0, 0]
  let line = getline(a:lnum)
  let end = match(line, '\S')
  if 0 < end
    let tab = 0
    for sp in split(strpart(line, 0, end), '\t', 1)
      if tab
        let col = (col / a:tok.tabsize + 1) * a:tok.tabsize
        let altcol = (altcol / a:tok.alttabsize + 1) * a:tok.alttabsize
      else
        let tab = 1
      endif
      let col += len(sp)
      let altcol += len(sp)
    endfor
  endif

  if col == a:tok.indstack[a:tok.indent]
    " no change
    if altcol != a:tok.altindstack[a:tok.indent]
      echomsg 'E_TABSPACE:'.a:lnum
      return 1
    endif
  elseif a:tok.indstack[a:tok.indent] < col
    " indent
    if s:maxindent <= a:tok.indent + 1
      echomsg 'E_TOODEEP:'.a:lnum
      return 1
    elseif altcol <= a:tok.altindstack[a:tok.indent]
      echomsg 'E_TABSPACE:'.a:lnum
      return 1
    endif
    let a:tok.indent += 1
    call add(a:tok.indstack, col)
    call add(a:tok.altindstack, altcol)
  else
    " dedent
    while 0 < a:tok.indent && col < a:tok.indstack[a:tok.indent]
      let a:tok.indent -= 1
      call remove(a:tok.indstack, -1)
      call remove(a:tok.altindstack, -1)
    endwhile
    if col != a:tok.indstack[a:tok.indent]
      echomsg 'E_DEDENT:'.a:lnum
      return 1
    elseif altcol != a:tok.altindstack[a:tok.indent]
      echomsg 'E_TABSPACE:'.a:lnum
      return 1
    endif
  endif
endfunction
