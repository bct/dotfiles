set encoding=utf-8
set fileencoding=utf-8

set expandtab
set tabstop=2
set shiftwidth=2

" don't break lines in the middle of words
set linebreak

filetype indent on
filetype on

" enable spelling
set spell
" (but not when i'm programming)
autocmd FileType python,ruby,haskell,erlang,c set nospell
" or when i'm configuring
autocmd BufRead /etc/* set nospell

" be OCD about stray whitespace
highlight RedundantSpaces term=standout ctermbg=red guibg=red
match RedundantSpaces /\s\+$\| \+\ze\t/

" -- setup for specific projects --

" flora
autocmd BufNewFile,BufRead /home/bct/projects/free-library-on-rails* set noexpandtab
autocmd BufNewFile,BufRead /home/bct/projects/free-library-on-rails* set ts=4
autocmd BufNewFile,BufRead /home/bct/projects/free-library-on-rails* set sw=4

" gajim
autocmd BufNewFile,BufRead /home/bct/projects/gajim* set noexpandtab
autocmd BufNewFile,BufRead /home/bct/projects/gajim* set ts=3
autocmd BufNewFile,BufRead /home/bct/projects/gajim* set shiftwidth=3
