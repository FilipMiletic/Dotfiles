call plug#begin('~/.vim/plugged')
" -- FZF & Rg
Plug '/usr/local/opt/fzf'
Plug 'junegunn/fzf.vim'
" -- tags
Plug 'ludovicchabant/vim-gutentags'
Plug 'majutsushi/tagbar'
call plug#end()

syntax on
set tabstop=4
set shiftwidth=4
set expandtab
set hidden
set viminfo+=n~/.vim/viminfo
set textwidth=120
set colorcolumn=80
set timeoutlen=1000 ttimeoutlen=0
set laststatus=2
set wrap
set linebreak
set nowritebackup
set nobackup
set mouse=a
set noswapfile
set splitbelow
set splitright
set ruler
set showmatch
set hlsearch
set incsearch
set number
set ignorecase
set statusline=%<%f\ [%{&ft}]\%=\ [%p%%:\ %l,%v/%L]
set background=dark
let mapleader=","
let &t_SI = "\e[6 q"
let &t_EI = "\e[2 q"
set t_Co=16

nnoremap <leader>z :cd %:h<CR>
nnoremap <C-\> :vsp<CR>
nnoremap <leader><leader> <C-W>w
nnoremap <C-j> <C-W>j
nnoremap <C-k> <C-W>k
nnoremap <C-h> <C-W>h
nnoremap <C-l> <C-W>l
noremap <leader>W :w !sudo tee %<CR>

" Show hidden characters
set lcs=tab:›\ ,trail:·,eol:¬,nbsp:_
set fcs=fold:-
nnoremap <silent> <leader>c :set nolist!<CR>
nnoremap Y y$
" Clear last search (,qs)
map <silent> <leader>qs <Esc>:noh<CR>

inoremap <expr> <CR>   pumvisible() ? "\<C-y>" : "\<CR>"
inoremap <expr> <Down> pumvisible() ? "\<C-n>" : "\<Down>"
inoremap <expr> <Up>   pumvisible() ? "\<C-p>" : "\<Up>"

" Paste from universal clipboard, instead of going to insert mode
map <leader>p :.!pbpaste<CR>

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

" Better vertical moving
nnoremap <expr> j v:count ? 'j' : 'gj'
nnoremap <expr> k v:count ? 'k' : 'gk'

" Buffer navigation (,,) (gb) (gB) (,ls)
map <Leader>. <C-^>
map <Leader>ls :buffers<CR>
map gb :bnext<CR>
map gB :bprev<CR>

let g:gutentags_cache_dir = '~/.vim/tags/'
let g:gutentags_exclude_project_root = ['~/', '/usr/local']

set rtp+=/usr/local/opt/fzf

let g:fzf_layout = { 'right': '~50%' }
let g:fzf_history_dir = '~/.vim/fzf-history'
let g:fzf_buffers_jump = 1 " Jump to existing buffer if available

nnoremap <leader>rg :Rg -i
nnoremap <C-p> :Files<CR>
nnoremap <C-g> :GFiles?<CR>
nnoremap <C-b> :Buffers<CR>
nnoremap <C-t> :Tags<CR>

nnoremap <silent> <leader>v :call fzf#run({
\   'right': winwidth('.') / 2,
\   'sink':  'vertical botright split' })<CR>

function! s:buflist()
  redir => ls
  silent ls
  redir END
  return split(ls, '\n')
endfunction
function! s:bufopen(e)
  execute 'buffer' matchstr(a:e, '^[ 0-9]*')
endfunction

nnoremap <silent> <Leader><Enter> :call fzf#run({
\   'source':  reverse(<sid>buflist()),
\   'sink':    function('<sid>bufopen'),
\   'options': '+m',
\   'down':    len(<sid>buflist()) + 5
\ })<CR>

" Colorscheme settings -- use default
hi ColorColumn ctermbg=233
hi StatusLine ctermfg=233 ctermbg=255
hi StatusLineNC ctermfg=233 ctermbg=242
hi VertSplit ctermbg=233 ctermfg=233
hi LineNr ctermfg=236
