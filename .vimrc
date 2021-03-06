:syn on

set wrap nowrap

:set textwidth=10000 " don't break long lines
:set hls
:set incsearch
:set autoindent
:set nocompatible
:set ignorecase
:set wrap nowrap

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

map <C-Home> <Esc>1gg
map! <C-Home> <Esc>1gg
noremap <C-Home> <Esc>1gg

"map  <C-F> <Esc>/
"imap <C-F> <Esc>/

" Save and quit with Control-S and Control-Q
" For this to work need to first insert
" stty stop undef # control s
" stty start undef # control q
" in .bashrc
map  <C-S> <Esc>:wq<CR>
imap <C-S> <Esc>:wq<CR>

"map  <C-w> <Esc>:w<CR>
"imap <C-w> <Esc>:w<CR>

" Move to other window in vimdiff
map  <C-p> <Esc>:wincmd w<CR>
imap <C-p> <Esc>:wincmd w<CR>

map  <C-Q> <Esc>:q<CR>
imap <C-Q> <Esc>:q<CR>

imap <C-v> <Esc>l<C-v>

" Insert a comment with control-C
map  <C-C> <Esc><Home>i#<Esc>
imap <C-C> <Esc><Home>i#<Esc>

cabbrev W  w
cabbrev Q  q
cabbrev Wq wq 
cabbrev WQ wq 
cabbrev nw set wrap nowrap 
cabbrev dw set nowrap wrap 
cabbrev sp set paste
cabbrev np set nopaste
cabbrev tf s/true/false/
cabbrev ft s/false/true/

" Make the backspace key delete newlines, etc.
":set backspace=indent,eol,start

source ~/.vim/tf.vim
map <C-i> call ToggleTrueFalse()
map! <C-T> call ToggleTrueFalse()
noremap <C-T> call ToggleTrueFalse()

set wrap

" Use mouse in terminal
"set mouse=a

set autoindent
" use intelligent indentation for C
set smartindent
" configure tabwidth and insert spaces instead of tabs
set tabstop=4 " tab width is 4 spaces
set shiftwidth=4 " indent also with 4 spaces
set noexpandtab " expand tabs to spaces

"vimdiff
highlight DiffChange cterm=none ctermfg=green ctermbg=blue gui=none guifg=blue guibg=Yellow

