" ---- Vundle Setup ----
set nocompatible            " be iMproved
filetype off                " required! for vundle
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" ---- Vundle Bundles ----
Bundle 'gmarik/vundle'
Bundle 'Valloric/YouCompleteMe'
Bundle 'scrooloose/syntastic'
Bundle 'jdonaldson/vaxe'
Bundle 'gregsexton/MatchTag'
Bundle 'MarcWeber/vim-addon-manager'
Bundle 'pangloss/vim-javascript'
Bundle 'kchmck/vim-coffee-script'
"Bundle 'MarcWeber/vim-haxe'

" VAM addons
"call vam#ActivateAddons(["vim-haxe"])

" You CompleteMe
let g:ycm_confirm_extra_conf = 0
"let g:ycm_key_

" ---- Tab Indenting Stuff ----
set expandtab
set smarttab
set softtabstop=4
set shiftwidth=4
set tabstop=4

" ---- Filetypes
autocmd BufRead,BufNewFile *.as set filetype=as3    " ActionScript 3

" ---- Other Stuff ----
syntax on
set autoindent              " autoindent to previous level
set smartindent				" better indenting based on brackets
filetype plugin indent on   " `fancy auto indenting. REQUIRED for Vundle.

set nu              " line numbers
set mouse=a         " mouse control
set ruler
set showmatch		" match brackets
set noerrorbells	" 

set incsearch       " better searching
set ignorecase      " ignore case when searching...
set smartcase       " unless there are some uppercase letters
set hlsearch		" highlight search results

set wildmenu " menu for nice file opening
set wildignore=*.o,*~,*.pyc,*.class " ignore compiled files

" ---- Key Bindings ----
imap <C-e> <esc>$i<right>
imap <C-a> <esc>0i

