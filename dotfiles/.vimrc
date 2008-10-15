:syn on

set wrap nowrap

:set textwidth=65
:set hls
:set incsearch
:set autoindent
:set nocompatible
:set ignorecase

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

cabbrev Q  q
cabbrev Wq wq 
cabbrev nw set wrap nowrap 
cabbrev dw set nowrap wrap 
cabbrev sp set paste
cabbrev np set nopaste

