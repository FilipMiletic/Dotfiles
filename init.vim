call plug#begin('~/.config/nvim/plugged')

Plug 'itchyny/lightline.vim'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'zchee/deoplete-clang'
Plug 'zchee/deoplete-jedi'
Plug 'scrooloose/nerdtree'
Plug 'rakr/vim-one'

call plug#end()

let g:lightline = {
      \ 'colorscheme': 'onedark',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'readonly', 'filename', 'modified' ] ]
      \ }
      \ }

let g:lightline.mode_map = {
		    \ 'n' : 'N',
		    \ 'i' : 'I',
		    \ 'R' : 'R',
		    \ 'v' : 'V',
		    \ 'V' : 'V-LINE',
		    \ "\<C-v>": 'V-BLOCK',
	        \ }

augroup reload_vimrc
    autocmd!
    autocmd bufwritepost $MYVIMRC nested source $MYVIMRC
augroup END

set hidden
set autoread
set showcmd
set backspace=indent,eol,start
set autoindent
set expandtab
set smarttab
set tabstop=4
set softtabstop=4
set shiftwidth=4
set scrolloff=1                                                                 " always show content after scroll
set scrolljump=1                                                                " minimum number of lines to scroll
set display+=lastline

set wildmenu                                                                    " show list for autocomplete
set wildmode=list:longest,full
set wildignore+=*/.git/*,*/.hg/*,*/.svn/*,*/.idea/*,*/.DS_Store
set noswapfile
set splitbelow
set splitright
set mouse=a
map <ScrollWheelUp> <C-Y>
map <ScrollWheelDown> <C-E>
set showmatch                                                                   " automatically highlight matching braces/brackets/
set matchtime=1                                                                 " tens of a second to show matching parentheses
set number
set lazyredraw
set laststatus=2
set noshowmode
set nowrap                                                                      " disable folds by default
set fillchars=vert:│
" disable sounds
set noerrorbells
set novisualbell

" searching
set hlsearch                                                                    " highlight searches
set incsearch                                                                   " incremental searching
set ignorecase                                                                  " ignore case for searching
set smartcase                                                                   " do case-sensitive if there's a capital letter

set cursorline
"autocmd WinLeave * setlocal nocursorline
"autocmd WinEnter * setlocal cursorline
set colorcolumn=80

let g:mapleader = ","
nnoremap <expr> j v:count ? 'j' : 'gj'
nnoremap <expr> k v:count ? 'k' : 'gk'
nnoremap <leader>w <C-w>v
nnoremap <leader>hw <C-w>s
nnoremap <leader><leader> <C-w><C-w>
nnoremap <leader>t :NERDTreeToggle<CR>

let $NVIM_TUI_ENABLE_CURSOR_SHAPE = 1
let &t_SI = "\<Esc>]50;CursorShape=1\x7"
let &t_EI = "\<Esc>]50;CursorShape=0\x7"

let &t_SR = "\<Esc>]50;CursorShape=2\x7"

let g:deoplete#enable_at_startup = 1
let g:deoplete#sources#clang#libclang_path="/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/libclang.dylib"
let g:deoplete#sources#clang#clang_header="/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang"

syntax on
set termguicolors
set background=dark
colorscheme one
