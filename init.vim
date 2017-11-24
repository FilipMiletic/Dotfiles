call plug#begin('~/.local/share/nvim/plugged')
Plug 'majutsushi/tagbar'
Plug '/usr/local/opt/fzf' | Plug 'junegunn/fzf.vim'
Plug 'robertmeta/nofrils'
call plug#end()

set copyindent
set preserveindent
set tabstop=4
set shiftwidth=4
set softtabstop=0
set noexpandtab
set autoindent
set hidden
set viminfo='20,\"90,h,%
set backspace=2
set numberwidth=5
set scrolloff=3
set wildmenu
set wildmode=list,longest,full
set wildignore+=*/.git/*,*/.hg/*,*/.svn/*,*/.idea/*,*/.DS_Store
set timeout timeoutlen=1000 ttimeoutlen=100
set autochdir
set nowritebackup
set nobackup
set noswapfile
set nowrap
set splitbelow
set splitright
set ruler
set mouse=a
set showmatch
set hlsearch
set incsearch
set number
set showtabline=1
set ignorecase
set smartcase
set colorcolumn=80
set shortmess+=I
set clipboard+=unnamed
set statusline=%<%f\ [%{&ft}]\%=\ [%p%%:\ %l/%L]
set statusline+=%{&paste?'[paste]\ ':''}
set laststatus=2
set background=dark
let mapleader=","

colorscheme goodwolf

" General {{{
augroup general_config
	"NeoVim Terminal
	tnoremap <Esc> <C-\><C-n>

	" Itallic comments {{{
	set t_ZH="\e[3m"
	set t_ZR="\e[23m"
	" }}}

	" Speed up viewport scrolling {{{
	nnoremap <C-e> 5<C-e>
	nnoremap <C-y> 5<C-y>
	" }}}

	" Faster split resizing (+,-) {{{
	if bufwinnr(1)
		map + <C-W>+
		map - <C-W>-
	endif
	" }}}

	" Make . work with visual selection in Visual mode
	xnoremap . :norm.<CR>

	" Easier vertical & horizontal splitting
	nnoremap <C-\> :vsp<CR>

	" Better split switching (Ctrl-j, Ctrl-k, Ctrl-h, Ctrl-l) {{{
	nnoremap <C-j> <C-W>j
	nnoremap <C-k> <C-W>k
	nnoremap <C-h> <C-W>h
	nnoremap <C-l> <C-W>l
	" }}}

	" Sudo write (,W) {{{
	noremap <leader>W :w !sudo tee %<CR>
	" }}}

	" NERDTree Open
	nnoremap <C-x> :NERDTree<CR>

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

	" Paste from universal clipboard,
	" instead of going to insert mode and
	" doing it as an input (,p) {{{
	map <leader>p :.!pbpaste<CR>
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
	map  <PageUp>   <C-U>
	map  <PageDown> <C-D>
	imap <PageUp>   <C-O><C-U>
	imap <PageDown> <C-O><C-D>
	" }}}

	" Relative numbers {{{
	set number
	au BufReadPost,BufNewFile * set number
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

" fzf & ag {{{
augroup fzf_config
	set rtp+=/usr/local/opt/fzf

	let g:fzf_layout = { 'up': '~30%' }
	let g:fzf_history_dir = '~/.config/nvim/fzf-history'
	let g:fzf_buffers_jump = 1 " Jump to existing buffer if available
	
	" --column: Show column number
	" --line-number: Show line number
	" --no-heading: Do not show file headings in results
	" --fixed-strings: Search term as a literal string
	" --ignore-case: Case insensitive search
	" --no-ignore: Do not respect .gitignore, etc...
	" --hidden: Search hidden files and folders
	" --follow: Follow symlinks
	" --glob: Additional conditions for search (in this case ignore everything in the .git/ folder)
	" --color: Search color options
	command! -bang -nargs=* Find call fzf#vim#grep('rg --column --line-number --no-heading --ignore-case --no-ignore --hidden --follow --glob "!.git/*" --color "always" '.shellescape(<q-args>), 1, <bang>0)

	nnoremap <C-p> :Files<CR>
	nnoremap <C-g> :GFiles?<CR>
	nnoremap <C-b> :Buffers<CR>
	nnoremap <C-t> :Tags<CR>
	nnoremap <C-m> :Marks<CR>
	nnoremap <C-f> :Find<CR>
	nnoremap <leader>l :Lines<CR>

	" Insert mode completion
	imap <c-x><c-k> <plug>(fzf-complete-word)
	imap <c-x><c-f> <plug>(fzf-complete-path)
	imap <c-x><c-l> <plug>(fzf-complete-line)

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

augroup END
" }}}
