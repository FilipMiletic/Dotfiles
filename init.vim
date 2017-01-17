call plug#begin('~/.config/nvim/plugged')

Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'zchee/deoplete-jedi'
Plug 'rhysd/vim-clang-format'
Plug '/usr/local/opt/fzf' | Plug 'junegunn/fzf.vim'
Plug 'airblade/vim-gitgutter'
Plug 'easymotion/vim-easymotion'
Plug 'ajh17/Spacegray.vim'

call plug#end()

set hidden
set autoread
set showcmd
set backspace=indent,eol,start
set tabstop=4
set shiftwidth=4
set softtabstop=4
set autoindent
set expandtab
set smarttab
set scrolloff=1
set scrolljump=1
set display+=lastline
set wildmenu
set wildmode=list:longest,full
set wildignore+=*/.git/*,*/.hg/*,*/.svn/*,*/.idea/*,*/.DS_Store
set noswapfile
set splitbelow
set splitright
set ruler
set mouse=a
map <ScrollWheelUp> <C-Y>
map <ScrollWheelDown> <C-E>
set showmatch
set matchtime=1
set number
set lazyredraw
set laststatus=2
set showmode
set nowrap
set fillchars=|
set noerrorbells
set novisualbell
set hlsearch
set incsearch
set ignorecase
set smartcase
set cursorline
set colorcolumn=80
let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1
let g:deoplete#enable_at_startup=1
let g:mapleader = ","
nnoremap <expr> j v:count ? 'j' : 'gj'
nnoremap <expr> k v:count ? 'k' : 'gk'
nnoremap <silent> <leader>W :call <SID>StripTrailingWhitespaces()<CR>
nnoremap <leader>b :Buffers<CR>
nnoremap <leader>f :Files<CR>
map <leader>z mzgg=G`z
nmap <C-n> :bnext<CR>
nmap <C-p> :bprev<CR>

" map to <Leader>cf in C/C++ code
autocmd FileType c,cpp,objc nnoremap <buffer><Leader>cf :<C-u>ClangFormat<CR>
autocmd FileType c,cpp,objc vnoremap <buffer><Leader>cf :ClangFormat<CR>

function! <SID>StripTrailingWhitespaces()
    " Preparation: save last search, and cursor position.
    let _s=@/
    let l = line(".")
    let c = col(".")
    " Do the business:
    %s/\s\+$//e
    " Clean up: restore previous search history, and cursor position
    let @/=_s
    call cursor(l, c)
endfunction

let g:clang_format#style_options = {
            \ "BasedOnStyle": 'llvm',
            \ "IndentWidth": 4,
            \ "Standard": "C++11"}

syntax on
set background=dark
colorscheme spacegray
