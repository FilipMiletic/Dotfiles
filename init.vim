call plug#begin('~/.config/nvim/plugged')

Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'zchee/deoplete-jedi'
Plug 'Rip-Rip/clang_complete'
Plug 'rhysd/vim-clang-format'
Plug 'neomake/neomake'
Plug 'scrooloose/nerdtree'
Plug 'itchyny/lightline.vim'
Plug 'villainy/murmur-lightline'
Plug 'taohex/lightline-buffer'
Plug 'benmills/vimux'
Plug '/usr/local/opt/fzf' | Plug 'junegunn/fzf.vim'
Plug 'easymotion/vim-easymotion'
Plug 'sjl/badwolf'

call plug#end()

set hidden
set autoread
set showcmd
set encoding=utf-8
set backspace=indent,eol,start
set noexpandtab
set tabstop=4
set shiftwidth=4
set softtabstop=4
set autoindent
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
set showmatch
set matchtime=1
set number
set lazyredraw
set laststatus=2
set noshowmode
set nowrap
set fillchars=|
set showtabline=2
set noerrorbells
set novisualbell
set hlsearch
set incsearch
set ignorecase
set smartcase
set shell=/bin/zsh
set cursorline
set colorcolumn=80
set clipboard=unnamed
"set list
"set listchars=tab:›\ ,eol:¬,trail:⋅

let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1

let g:deoplete#enable_at_startup=1
let g:mapleader = ","

nnoremap <expr> j v:count ? 'j' : 'gj'
nnoremap <expr> k v:count ? 'k' : 'gk'
nnoremap <silent> <leader>W :call <SID>StripTrailingWhitespaces()<CR>

" Opens FZF Buffer list
nnoremap <leader>b :Buffers<CR>

" Kills buffer
nnoremap <leader>k :bdelete<CR>

" FZF File search
nnoremap <leader>f :Files<CR>

" Reindents whole files
map <leader>z mzgg=G`z

" Changes buffer
nmap <C-n> :bnext<CR>
nmap <C-p> :bprev<CR>

" <Leader><Leader>w is for easy motion
" Run terminal command in new tmux pane
map <Leader>vp :VimuxPromptCommand<CR>

" Run last command executed by VimuxRunCommand
map <Leader>vl :VimuxRunLastCommand<CR>

" Splits navigation
nnoremap <leader>j <C-W><C-J>
nnoremap <leader>k <C-W><C-K>
nnoremap <leader>l <C-W><C-L>
nnoremap <leader>h <C-W><C-H>

" Vertical split
nnoremap <leader>w :vsplit<CR>

" NerdTREE binding
nnoremap <leader>t :NERDTreeToggle<CR>

" Esc mapped to act usual way in terminal mode too
tnoremap <Esc> <C-\><C-n>

" Map ClangFormat to <Leader>cf in C/C++ code
autocmd FileType c,cpp,objc nnoremap <buffer><Leader>cf :<C-u>ClangFormat<CR>
autocmd FileType c,cpp,objc vnoremap <buffer><Leader>cf :ClangFormat<CR>
autocmd! BufWritePost * Neomake

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

let g:clang_library_path="/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/"
let g:clang_close_preview = 1

let g:NERDTreeMinimalUI = 1
let g:NERDTreeHijackNetrw = 0

let g:lightline = {
			\ 'colorscheme': 'murmur',
			\ 'active': {
			\   'left': [ [ 'mode', 'paste' ],
			\             [ 'filename' ] ],
			\   'right': [ [ 'percent', 'lineinfo' ],
			\              [ 'fileformat', 'fileencoding', 'filetype' ] ]
			\ },
			\ 'tabline': {
			\ 'left': [ [ 'bufferinfo' ], [ 'bufferbefore', 'buffercurrent', 'bufferafter' ], ],
			\ 'right': [ [ 'close' ], ],
			\ },
			\ 'component_expand': {
			\ 'buffercurrent': 'lightline#buffer#buffercurrent2',
			\ },
			\ 'component_type': {
			\ 'buffercurrent': 'tabsel',
			\ },
			\ 'component_function': {
			\   'readonly': 'LightLineReadonly',
			\   'modified': 'LightLineModified',
			\   'filename': 'LightLineFilename',
			\   'bufferbefore': 'lightline#buffer#bufferbefore',
			\   'bufferafter': 'lightline#buffer#bufferafter',
			\   'bufferinfo': 'lightline#buffer#bufferinfo'
			\ },
			\ 'separator': { 'left': "\ue0b0", 'right': "\ue0b2" },
			\ 'subseparator': { 'left': "\ue0b1", 'right': "\ue0b3" }
			\ }

function! LightLineModified()
	if &filetype == "help"
		return ""
	elseif &modified
		return "+"
	elseif &modifiable
		return ""
	else
		return ""
	endif
endfunction

function! LightLineReadonly()
	if &filetype == "help"
		return ""
	elseif &readonly
		return "RO"
	else
		return ""
	endif
endfunction

function! LightLineFilename()
	return ('' != LightLineReadonly() ? LightLineReadonly() . ' ' : '') .
				\ ('' != expand('%:t') ? expand('%:t') : '[No Name]') .
				\ ('' != LightLineModified() ? ' ' . LightLineModified() : '')
endfunction

let g:python_host_prog="/usr/local/bin/python2"
let g:python3_host_prog="/usr/local/bin/python3"
let g:ruby_host_prog="/Users/phil/.rvm/rubies/ruby-2.4.0/bin/ruby"


syntax on
set termguicolors
set background=dark
colorscheme badwolf
