syntax on

execute pathogen#infect()

set encoding=utf-8
set fileencoding=utf-8

set expandtab
set tabstop=2
set shiftwidth=2

" don't break lines in the middle of words
set linebreak

set hlsearch

set laststatus=2

set relativenumber
set number

" Delete comment character when joining commented lines
set formatoptions+=j

" use a leader key that's convenient for dvorak
let mapleader = '-'

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" CUSTOM AUTOCMDS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
augroup vimrcEx
  " Clear all autocmds in the group
  autocmd!

  " Jump to last cursor position unless it's invalid or in an event handler
  autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif
augroup END

" oooh so pretty
set background=dark
let g:solarized_termcolors=256
colorscheme solarized

filetype indent on
filetype plugin on
filetype on

" be OCD about stray whitespace
highlight RedundantSpaces term=standout ctermbg=red guibg=red
match RedundantSpaces /\s\+$\| \+\ze\t/

" default syntastic colours are hard to read
highlight SyntasticWarning ctermfg=254 ctermbg=088
highlight SignColumn ctermbg=236

let g:syntastic_ruby_checkers=['rubocop']

let g:formatdef_scalafmt = "'scalafmt'"
let g:formatters_scala = ['scalafmt']

let g:airline#extensions#ale#enabled = 1

let g:elm_format_autosave = 1

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" MISC KEY MAPS
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

function! MapCR()
  nnoremap <cr> :nohlsearch<cr>
endfunction
call MapCR()

" <c-^> means 'back to previous file'
nnoremap <leader><leader> <c-^>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" MULTIPURPOSE TAB KEY
" Indent if we're at the beginning of a line. Else, do completion.
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! InsertTabWrapper()
    let col = col('.') - 1
    if !col || getline('.')[col - 1] !~ '\k'
        return "\<tab>"
    else
        return "\<c-p>"
    endif
endfunction
inoremap <tab> <c-r>=InsertTabWrapper()<cr>
inoremap <s-tab> <c-n>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" MAPS TO JUMP TO SPECIFIC COMMAND-T TARGETS AND FILES
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
call denite#custom#var('file_rec', 'command', ['ag', '--follow', '--nocolor', '--nogroup', '-g', ''])

map <leader>ga :Denite file_rec:api<cr>
map <leader>gd :Denite file_rec:admin<cr>
map <leader>go :Denite file_rec:ops<cr>
map <leader>gl :Denite file_rec:lib<cr>
map <leader>gt :Denite file_rec:test<cr>
map <leader>f :Denite file_rec<cr>

"map <leader>ga :CtrlP api<cr>
"map <leader>gd :CtrlP admin<cr>
"map <leader>go :CtrlP ops<cr>
"map <leader>gl :CtrlP lib<cr>
"map <leader>gt :CtrlP test<cr>
"map <leader>f :CtrlP<cr>

" if executable('matcher')
"   let g:ctrlp_user_command = ['.git/', 'cd %s && git ls-files . -co --exclude-standard']
"   let g:ctrlp_match_func = { 'match': 'GoodMatch' }
" 
"   function! GoodMatch(items, str, limit, mmode, ispath, crfile, regex)
"     " Create a cache file if not yet exists
"     let cachefile = ctrlp#utils#cachedir().'/matcher.cache'
"     if !( filereadable(cachefile) && a:items == readfile(cachefile) )
"       call writefile(a:items, cachefile)
"     endif
"     if !filereadable(cachefile)
"       return []
"     endif
" 
"     " a:mmode is currently ignored. In the future, we should probably do
"     " something about that. the matcher behaves like "full-line".
"     let cmd = 'matcher --limit '.a:limit.' --manifest '.cachefile.' '
"     if !( exists('g:ctrlp_dotfiles') && g:ctrlp_dotfiles )
"       let cmd = cmd.'--no-dotfiles '
"     endif
"     let cmd = cmd.a:str
" 
"     return split(system(cmd), "\n")
"   endfunction
" end

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" SWITCH BETWEEN TEST AND PRODUCTION CODE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! OpenTestAlternate()
  let new_file = AlternateForCurrentFile()
  exec ':e ' . new_file
endfunction
function! AlternateForCurrentFile()
  let current_file = expand("%")
  let new_file = current_file
  let in_spec = match(current_file, '^spec/') != -1
  let going_to_spec = !in_spec
  let in_app = match(current_file, '\<controllers\>') != -1 || match(current_file, '\<models\>') != -1 || match(current_file, '\<views\>') || match(current_file, '\<helpers\>') != -1
  if going_to_spec
    if in_app
      let new_file = substitute(new_file, '^app/', '', '')
    end
    let new_file = substitute(new_file, '\.rb$', '_spec.rb', '')
    let new_file = 'spec/' . new_file
  else
    let new_file = substitute(new_file, '_spec\.rb$', '.rb', '')
    let new_file = substitute(new_file, '^spec/', '', '')
    if in_app
      let new_file = 'app/' . new_file
    end
  endif
  return new_file
endfunction
nnoremap <leader>. :call OpenTestAlternate()<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" RUNNING TESTS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map <leader>t :call RunTestFile()<cr>
map <leader>T :call RunNearestTest()<cr>

function! RunTestFile(...)
    if a:0
        let command_suffix = a:1
    else
        let command_suffix = ""
    endif

    " Run the tests for the previously-marked file.
    let in_test_file = match(expand("%"), 'test') != -1
    if in_test_file
        call SetTestFile()
    elseif !exists("t:grb_test_file")
        return
    end
    call RunTests(t:grb_test_file . command_suffix)
endfunction

function! RunNearestTest()
    let spec_line_number = line('.')
    call RunTestFile(" -l " . spec_line_number)
endfunction

function! SetTestFile()
    " Set the spec file that tests will be run for.
    let t:grb_test_file=@%
endfunction

function! RunTests(filename)
    " Write the file and run tests for the given filename
    :w
    :silent !echo;echo;echo;echo;echo;echo;echo;echo;echo;echo
    :silent !echo;echo;echo;echo;echo;echo;echo;echo;echo;echo
    :silent !echo;echo;echo;echo;echo;echo;echo;echo;echo;echo
    :silent !echo;echo;echo;echo;echo;echo;echo;echo;echo;echo
    :silent !echo;echo;echo;echo;echo;echo;echo;echo;echo;echo
    :silent !echo;echo;echo;echo;echo;echo;echo;echo;echo;echo
    if filereadable("./scripts/bin/test")
      exec ":!bundle exec ./scripts/bin/test " . a:filename
    else
      exec ":!bundle exec ruby " . a:filename
    end
endfunction
