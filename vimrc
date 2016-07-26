" ---- Make sure colors work properly ----
if $COLORTERM == 'gnome-terminal'
  set t_Co=256
endif
set shell=/bin/bash

" ---- Vundle Setup ----
set nocompatible            " be improved
filetype off                " required! for vundle
set rtp+=~/.vim/bundle/Vundle.vim

" ---- Vundle Bundles ----
call vundle#begin()
Plugin 'PotatoesMaster/i3-vim-syntax'
Plugin 'dag/vim-fish'
Plugin 'VundleVim/Vundle.vim'
Plugin 'airblade/vim-gitgutter'
Plugin 'edkolev/promptline.vim'
Plugin 'gregsexton/MatchTag'
Plugin 'junegunn/goyo.vim'
Plugin 'scrooloose/nerdcommenter'
Plugin 'scrooloose/syntastic'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'vim-airline/vim-airline'
" themes
Plugin 'vim-airline/vim-airline-themes'
Plugin 'tomasr/molokai'
Plugin 'vim-scripts/twilight'
"Plugin 'morhetz/gruvbox'
" unused
"Plugin 'pangloss/vim-javascript'
call vundle#end()

" ---- Tab Indenting Stuff ----
set expandtab
set smarttab
set softtabstop=4
set shiftwidth=4
set tabstop=4

set autoindent              " autoindent to previous level
set smartindent				" better indenting based on brackets
filetype plugin indent on   " fancy auto indenting. REQUIRED for Vundle.

" ---- Searching ---- 
"set incsearch       " better searching
set ignorecase      " ignore case when searching...
set smartcase       " unless there are some uppercase letters
"set hlsearch		" highlight search results

" ---- Menu ----
set wildmenu " menu for nice file opening
set wildignore=*.o,*~,*.pyc,*.class " ignore compiled files

" ---- Colors ----
let g:molokai_original = 1
let g:rehash256 = 1
colorscheme molokai
let g:airline_theme = 'distinguished'

" ---- Key Bindings ----
let g:multi_cursor_use_default_mapping=0
let g:multi_cursor_next_key='<C-e>'
let g:multi_cursor_quit_key='<Esc>'

imap <C-v> <esc>lv
imap <C-d> <esc>lyypi
nmap <C-d> yyp
imap <C-F> <esc>:Goyo<return>i
nmap <C-F> :Goyo<return>

" move lines up and down
nmap <C-Up> :m -2<return>
nmap <C-Down> :m +1<return>
imap <C-Up> <esc>:m -2<return>i
imap <C-Down> <esc>:m +1<return>i
vnoremap <C-Up> :m '<-2<CR>gv=gv
vnoremap <C-Down> :m '>+1<CR>gv=gv

" move between tabs
nmap <A-tab> :bn<return>

nmap <C-s> :w<return>
nmap <C-w> :q<return>
nmap <C-q> :qa<return>
imap <C-s> <esc>:w<return>
imap <C-w> <esc>:q<return>
imap <C-q> <esc>:qa<return>

" ---- Airline ----
set laststatus=2
let g:airline_powerline_fonts=1
let g:bufferline_echo=0
set noshowmode
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'

" ---- Other Stuff ----
syntax on
set updatetime=50
set timeoutlen=1000 ttimeoutlen=0 " No delay when switching modes
let g:gitgutter_enabled = 1       " 
set nu                            " line numbers
set mouse=a                       " mouse control
set ruler                         " line/char number in statusbar
set showmatch                     " match brackets
set noerrorbells                  " Turn off error bells
set title                         " set the terminal title
set cursorline
set scrolloff=5

" ---- Promptline ----
let g:promptline_theme = 'airline'
let g:promptline_powerline_symbols = 1
let g:promptline_preset = {
    \'a' : [ promptline#slices#host({'only_if_ssh': 1}) ],
    \'b' : [ promptline#slices#cwd({'dir_limit': 3}) ],
    \'z' : [ promptline#slices#vcs_branch() ],
    \'warn' : [ promptline#slices#last_exit_code() ]}

