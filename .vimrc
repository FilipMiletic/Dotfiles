call plug#begin('~/.vim/plugged')
" -- FZF interface for rip-grep and fd
Plug '/usr/local/opt/fzf'
Plug 'junegunn/fzf.vim'
" -- tags
Plug 'ludovicchabant/vim-gutentags'
Plug 'majutsushi/tagbar'
" -- syntaxes
Plug 'w0rp/ale'
Plug 'tikhomirov/vim-glsl'
Plug 'rust-lang/rust.vim'
Plug 'robertmeta/nofrils'
call plug#end()

syntax on
" set term=xterm-256color
set cole=0
set encoding=utf8
set shell=/bin/bash
set shiftwidth=2
set shortmess+=I
set tabstop=2
set expandtab
set hidden
set viminfo+=n~/.vim/viminfo
set colorcolumn=80
set timeoutlen=1000 ttimeoutlen=0
set laststatus=2
set backspace=indent,eol,start
set nowritebackup
set nobackup
set noswapfile
set mouse=a
set splitbelow
set splitright
set ruler
set showmatch
set nofoldenable
set nohlsearch
set incsearch
set number
set ignorecase
set smartcase
set smarttab
set statusline=%<%f\ (%{&ft})\ %-4(%m%)%=%-19(%3l,%02c%03V%)
let mapleader=","
let &t_SI = "\e[6 q"
let &t_EI = "\e[2 q"

nnoremap <leader>z :cd %:h<CR>
nnoremap <C-\> :vsp<CR>
nnoremap <leader><leader> <C-W>w
nnoremap <C-j> <C-W>j
nnoremap <C-k> <C-W>k
nnoremap <C-h> <C-W>h
nnoremap <C-l> <C-W>l
nnoremap <leader>W :w !sudo tee %<CR>
nnoremap <leader>[ :bp<CR>
nnoremap <leader>] :bn<CR>

" Show hidden characters
set lcs=tab:›\ ,trail:·,eol:¬,nbsp:_
set fcs=fold:-
nnoremap <silent> <leader>c :set nolist!<CR>

" Yank till the end of line with Y
nnoremap Y y$

" Clear last search (,qs)
map <silent> <leader>qs <Esc>:noh<CR>
inoremap <expr> <CR>   pumvisible() ? "\<C-y>" : "\<CR>"
inoremap <expr> <Down> pumvisible() ? "\<C-n>" : "\<Down>"
inoremap <expr> <Up>   pumvisible() ? "\<C-p>" : "\<Up>"

" Paste from universal clipboard, instead of going to insert mode
map <leader>p :.!pbpaste<CR>
map <leader>q gg=G<CR>

" Search and replace word under cursor (,*)
nnoremap <leader>* :%s/\<<C-r><C-w>\>//<Left>
vnoremap <leader>* "hy:%s/\V<C-r>h//<left>

" Strip trailing whitespace (,ss)
function! StripWhitespace () "
	let save_cursor = getpos(".")
	let old_query = getreg('/')
	:%s/\s\+$//e
	call setpos('.', save_cursor)
	call setreg('/', old_query)
endfunction
noremap <leader>ss :call StripWhitespace ()<CR>

" Better vertical navigation
nnoremap <expr> j v:count ? 'j' : 'gj'
nnoremap <expr> k v:count ? 'k' : 'gk'
map <Leader>. <C-^>
map <Leader>ls :buffers<CR>

let g:gutentags_cache_dir = '~/.vim/tags/'
let g:gutentags_exclude_project_root = ['~/', '/usr/local']

set rtp+=/usr/local/opt/fzf

let g:fzf_layout = { 'down': '~60%' }
let g:fzf_history_dir = '~/.vim/fzf-history'
let g:fzf_buffers_jump = 1 " Jump to existing buffer if available

" FZF keybindings
nnoremap <C-r> :Rg<CR>
nnoremap <C-p> :Files<CR>
nnoremap <C-g> :GFiles?<CR>
nnoremap <C-b> :Buffers<CR>
nnoremap <C-t> :Tags<CR>

" Open file in split view
nnoremap <silent> <leader>v :call fzf#run({
\   'right': winwidth('.') / 2,
\   'sink':  'vertical botright split' })<CR>

" I like my vim as plain as possible in Terminal.app
set bg=dark
colo plain-dark
hi ALEWarning ctermbg=Magenta
