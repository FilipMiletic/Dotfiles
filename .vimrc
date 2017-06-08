call plug#begin('~/.vim/plugged')
Plug 'scrooloose/nerdtree'
Plug 'w0rp/ale'
Plug 'sjl/badwolf'
Plug '/usr/local/opt/fzf' | Plug 'junegunn/fzf.vim'
call plug#end()

set ttyfast
set hidden
set viminfo=
set backspace=2
set expandtab
set numberwidth=5
set tabstop=2
set shiftwidth=2
set softtabstop=2
set autoindent
set scrolloff=3
set wildmenu
set wildmode=list,longest,full
set wildignore+=*/.git/*,*/.hg/*,*/.svn/*,*/.idea/*,*/.DS_Store
set timeout timeoutlen=1000 ttimeoutlen=100
set noswapfile
set nowrap
set splitbelow
set splitright
set ruler
set mouse=a
set showmatch
set hlsearch
set number
set showtabline=1
set ignorecase
set lazyredraw
set smartcase
set colorcolumn=80
set cursorline
set shortmess+=I
set clipboard+=unnamed
set statusline=\ \%f%m%r%h%w\ \ %y\ [%{&ff}]\%=\ [%p%%:\ %l/%L]
set laststatus=2
set cmdheight=1

set termguicolors
set background=dark
colorscheme badwolf

let mapleader=","
let NERDTreeMinimalUI=1


" Ale Settings {{{
augroup ale_config
  autocmd!

  let g:ale_linters = {'python': ['flake8',]}
  let g:ale_python_flake8_executable = 'python3'
  let g:ale_python_flake8_args = '-m flake8'

augroup END
"}}}


" General {{{
augroup general_config
	autocmd!

	" Speed up viewport scrolling {{{
	nnoremap <C-e> 3<C-e>
	nnoremap <C-y> 3<C-y>
	" }}}

	" Faster split resizing (+,-) {{{
	if bufwinnr(1)
		map + <C-W>+
		map - <C-W>-
	endif
	" }}}

	" Better split switching (Ctrl-j, Ctrl-k, Ctrl-h, Ctrl-l) {{{
	nnoremap <C-j> <C-W>j
	nnoremap <C-k> <C-W>k
	nnoremap <C-h> <C-W>h
	nnoremap <C-l> <C-W>l
	" }}}

	" Sudo write (,W) {{{
	noremap <leader>W :w !sudo tee %<CR>
	" }}}

	" Remap :W to :w {{{
	command! W w
	" }}}

	" Toggle show tabs and trailing spaces (,c) {{{
	set lcs=tab:›\ ,trail:·,eol:¬,nbsp:_
	set fcs=fold:-
	nnoremap <silent> <leader>c :set nolist!<CR>
	" }}}

	" Clear last search (,qs) {{{
	map <silent> <leader>qs <Esc>:noh<CR>
	" map <silent> <leader>qs <Esc>:let @/ = ""<CR>
	" }}}

	" Remap keys for auto-completion menu {{{
	inoremap <expr> <CR>   pumvisible() ? "\<C-y>" : "\<CR>"
	inoremap <expr> <Down> pumvisible() ? "\<C-n>" : "\<Down>"
	inoremap <expr> <Up>   pumvisible() ? "\<C-p>" : "\<Up>"
	" }}}

	" Paste toggle (,p) {{{
	set pastetoggle=<leader>p
	map <leader>p :set invpaste paste?<CR>
	" }}}

	" Yank from cursor to end of line {{{
	nnoremap Y y$
	" }}}

	" Insert newline {{{
	map <leader><Enter> o<ESC>
	" }}}

	" Search and replace word under cursor (,*) {{{
	nnoremap <leader>* :%s/\<<C-r><C-w>\>//<Left>
	vnoremap <leader>* "hy:%s/\V<C-r>h//<left>
	" }}}

	" Strip trailing whitespace (,ss) {{{
	function! StripWhitespace () " {{{
		let save_cursor = getpos(".")
		let old_query = getreg('/')
		:%s/\s\+$//e
		call setpos('.', save_cursor)
		call setreg('/', old_query)
	endfunction " }}}
	noremap <leader>ss :call StripWhitespace ()<CR>
	" }}}

	" Join lines and restore cursor location (J) {{{
	nnoremap J mjJ`j
	" }}}

	" Fix page up and down {{{
	map <PageUp> <C-U>
	map <PageDown> <C-D>
	imap <PageUp> <C-O><C-U>
	imap <PageDown> <C-O><C-D>
	" }}}

	" Relative numbers {{{
	set relativenumber " Use relative line numbers. Current line is still in status bar.
	au BufReadPost,BufNewFile * set relativenumber
	" }}}

	" Better vertical moving {{{
	nnoremap <expr> j v:count ? 'j' : 'gj'
	nnoremap <expr> k v:count ? 'k' : 'gk'
	" }}}
augroup END
" }}}


" Buffers {{{
augroup buffer_control
  autocmd!

  " Buffer navigation (,,) (gb) (gB) (,ls) {{{
  map <Leader>, <C-^>
  map <Leader>ls :buffers<CR>
  map gb :bnext<CR>
  map gB :bprev<CR>
  " }}}

  " Jump to buffer number (<N>gb) {{{
  let c = 1
  while c <= 99
    execute "nnoremap " . c . "gb :" . c . "b\<CR>"
    let c += 1
  endwhile
  " }}}

  " Close Quickfix window (,qq) {{{
  map <leader>qq :cclose<CR>
  " }}}

  " Rename buffer (:Rename) {{{
  function! s:RenameBuffer(name)
    silent! execute 'saveas! ' . a:name
    let l:old_buffer = bufnr("#")
    let l:old_filename = expand("#:t")
    let l:new_buffer = bufnr("%")
    let l:new_filename = expand("%:t")
    silent! execute '!rm ' . shellescape(expand("#"), 1)
    silent! execute 'bd' l:old_buffer
    echom 'Renamed `' . l:old_filename . '` to `' . l:new_filename . '`'
  endfunction
  command! -nargs=1 Rename call s:RenameBuffer(<f-args>)
  " }}}
augroup END
" }}}

" fzf {{{
augroup fzf_config
  set rtp+=/usr/local/opt/fzf

  let g:fzf_layout = { 'up': '~40%' }
  let g:fzf_history_dir = '~/.config/nvim/fzf-history'
  let g:fzf_buffers_jump = 1 " Jump to existing buffer if available

  nnoremap <C-p> :Files<CR>
  nnoremap <C-g> :GFiles?<CR>
  nnoremap <C-b> :Buffers<CR>
  nnoremap <C-t> :Tags<CR>
  nnoremap <C-m> :Marks<CR>
  nnoremap <leader>l :Lines<CR>

  " Insert mode completion
  imap <c-x><c-k> <plug>(fzf-complete-word)
  imap <c-x><c-f> <plug>(fzf-complete-path)
  imap <c-x><c-j> <plug>(fzf-complete-file-ag)
  imap <c-x><c-l> <plug>(fzf-complete-line)
augroup END
" }}}
