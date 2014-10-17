set nocompatible " Because we don't need compatibility
set encoding=utf-8 " Necessary to show Unicode glyphs
call pathogen#infect() " Initialize pathogen
set history=50 " Keep 50 lines of command line history
set ruler " Show the cursor position all the time
set showcmd	" Display incomplete commands
syntax on   " Enable syntax highlighting
set hlsearch " Highlight search results
set incsearch " Incremental searching
set ignorecase " Searching is not case sensitive
set smartcase " Searching is case sensitive when containing an uppercase char
set directory-=. " Don't let vim create swap files in the current directory
set undodir=~/.vimundo " Store undo files in that folder
set undofile " Keep undo files

" Gvim options
set guioptions-=TmrL

set mouse=a " Enable mouse support

filetype plugin indent on " Load filetype plugins

" Set automatic wrapping for certain filetypes
augroup wrapping
autocmd!
" For all text and markdown files set 'textwidth' to 79 characters.
autocmd FileType text setlocal textwidth=79 colorcolumn=80
autocmd FileType day setlocal textwidth=79 colorcolumn=80
autocmd FileType markdown setlocal textwidth=79 colorcolumn=80
autocmd FileType org setlocal textwidth=79 colorcolumn=80
autocmd FileType lua setlocal tabstop=2 shiftwidth=2 softtabstop=2
autocmd FileType moon setlocal tabstop=2 shiftwidth=2 softtabstop=2
augroup END

let g:PHP_default_indenting = 1
let g:PHP_outdentphpescape = 0

color molokai
if has("gui_running")
    color base16-tomorrow
    set gfn=Meslo\ LG\ S\ 13
endif

set expandtab " Turn tabs into spaces
" Each tab should be 4 spaces wide
set tabstop=4
set shiftwidth=4
set softtabstop=4

set hidden " Enable switching buffers without saving

" Save read-only files that aren't opened as root
cmap w!! w !sudo tee % > /dev/null

" always use regular expressions
nnoremap / /\v
vnoremap / /\v

let mapleader = " "
" Easily disable highlighting
nnoremap <leader><leader> :noh<cr>
" Easily select the text just pasted
nnoremap <leader>v V`]
" Toggle NERDTree
nnoremap <leader>n :NERDTreeToggle<CR>
" Better NERDTree
let g:NERDTreeMinimalUI = 1

" Toggle Gundo
nnoremap <leader>g :GundoToggle<CR>

" Make paredit not mess with muscle memory
let g:paredit_shortmaps = 0
let g:paredit_leader = '\'
