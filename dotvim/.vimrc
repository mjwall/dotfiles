"filetype off
call pathogen#infect()
syntax on
filetype plugin indent on
call pathogen#helptags()

set nocompatible
set hidden

set backup
set backupdir=~/.backup
set directory=~/.backup

set clipboard=unnamed
" set paste "causes issues with vim-endwise in the terminal
set ttymouse=xterm

<<<<<<< Updated upstream
"set background=dark
"colors vividchalk
=======
" set background=dark
" colors vividchalk
>>>>>>> Stashed changes

set tabstop=2
set shiftwidth=2
set shiftround
set expandtab
set autoindent

set showmode
set showcmd
set ruler
set laststatus=2
set smarttab
set backspace=eol,start,indent
set showmatch
set nowrap

" set guifont=Monaco:h15

set textwidth=79

set incsearch

set number

" set foldmethod=syntax
" set foldlevel=20

filetype plugin on

" easier navigation between window splits
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

nnoremap <leader>a :Ack
nnoremap <leader>n :NERDTree
nnoremap <leader>t :NERDTreeToggle
" guarantees that the NERDTrees for all tabs will be one and the same
map <F2> :NERDTreeToggle \| :silent NERDTreeMirror<CR>


" gpg stuff
let g:GPGPreferArmor=1
let g:GPGDefaultRecipients=["mjwall@gmail.com"]

" journal stuff
let g:journal_directory='~/notes'
let g:journal_encrypted=1

" vimoutliner encryption
au! BufRead,BufNewFile *.otl.gpg setfiletype vo_base

au BufNewFile,BufRead Vagrantfile* set filetype=ruby

" tmux mouse helper
"if has("mouse")
"  set mouse=a
"  set mousehide
"endif

"clj-vim
let g:clj_paren_rainbow = 1
