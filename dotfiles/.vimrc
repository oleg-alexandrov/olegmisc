:syn on

set wrap nowrap

:set textwidth=65
:set hls
:set incsearch
:set autoindent
:set nocompatible
:set ignorecase
:set wrap nowrap
":set paste

:colorscheme desert

" imap is interactive mode, unlike map
imap <C-D> <Delete>
imap <C-J> <Esc>{gq}<End>a
imap <C-K> <Esc>ddi

map  <C-A> <Home>
map  <C-E> <End>
map  <C-K> dd 
map <Backspace> i<Backspace>
map <C-D> i<Delete>
map <C-G> :set hls!<bar>set hls?<CR>
map <C-H> i<Backspace>
map <C-J> {gq}j
map <Delete> i<Delete>
map <Return> i<Return>
map! <C-A> <Home>
map! <C-E> <End>

noremap <C-Home> <Esc>1gg

map  <C-F> <Esc>/
imap <C-F> <Esc>/

" Quit with Control-Q
map <C-Q> <Esc>:q<CR>
imap <C-Q> <Esc>:q<CR>

" Save and quit with Control-S
map <C-S> <Esc>:wq<CR>
imap <C-S> <Esc>:wq<CR>

imap <C-v> <Esc>l<C-v>

cabbrev W  w
cabbrev Q  q
cabbrev Wq wq 
cabbrev WQ wq 
cabbrev nw set wrap nowrap 
cabbrev dw set nowrap wrap 
cabbrev sp set paste
cabbrev np set nopaste

ab st set shared::job::dbgTrackKaroList                      {0}

" Make the backspace key delete newlines, etc.
":set backspace=indent,eol,start
