set nocompatible
set autoindent
set smartindent
filetype plugin indent on

syntax enable
set ruler
set ignorecase
set nolazyredraw
set noerrorbells
set novisualbell
set ttyfast
set encoding=utf8
set nowrap
set cursorline
set copyindent
set noswapfile
set showmatch
set mouse=a
if has("mouse_sgr")
	set ttymouse=sgr
end
set autochdir
" indentation settings (and format)
set laststatus=2
set tabstop=4
set shiftwidth=4
set expandtab
set softtabstop=4
set noexpandtab
set fileformat=unix
set showtabline=2

" Better colors in iTerm2. In terminal.app need to turn this off
set termguicolors

" make cursor pipe in insert mode, and block in normal
let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1

call plug#begin('~/.config/nvim/plugged')
" Keep Plug commands between plug#begin/end.
function! DoRemote(arg)
	UpdateRemotePlugins
endfunction
Plug 'itchyny/lightline.vim'
Plug 'scrooloose/nerdtree'
Plug 'rip-rip/clang_complete'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'critiqjo/vim-bufferline'
Plug 'fatih/vim-go'
Plug 'sjl/badwolf'
Plug 'joshdick/onedark.vim'
Plug 'w0ng/vim-hybrid'
Plug 'cocopon/lightline-hybrid.vim'
Plug 'shirataki/lightline-onedark'
Plug 'kristijanhusak/vim-hybrid-material'
Plug '844196/lightline-badwolf.vim'
Plug 'frankier/neovim-colors-solarized-truecolor-only'
Plug 'scrooloose/syntastic'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'Shougo/deoplete.nvim', { 'do': function('DoRemote') }
Plug 'elixir-lang/vim-elixir'
Plug 'jaromero/vim-monokai-refined'
Plug 'aliou/moriarty.vim'
Plug 'tpope/vim-surround'
Plug 'zchee/deoplete-jedi'
Plug 'tpope/vim-rails/'
Plug 'vim-ruby/vim-ruby'
call plug#end()

" =========| Color settings and schemes |========= 
" Turn highlighted string backgrounds for nofrils theme
set background=dark
colorscheme hybrid

" =========| Some of my keybindings |=========	
" My Numbering implementation that gets
" activated on C-n, in Insert mode it
" is set to Relative/hybrid, in Normal
" mode it is set to static, in default
" it is hidden.
function! NumberToggle()
	if(&relativenumber == 1)
		set relativenumber
	else
		set norelativenumber
		set number
	endif
endfunc

nnoremap <C-n> :call NumberToggle()<cr>

function ResetNumbering()
	if (&relativenumber == 1)
		set norelativenumber
		set number
	endif
endfunc

autocmd InsertEnter * :set relativenumber
autocmd InsertLeave * call ResetNumbering()

" set buffers accordingly (for C)
augroup project
	autocmd!
	autocmd BufRead, BufNewFile *.h,*.c set filetype = c.doxygen
augroup END

" remap leader key
let mapleader = ","

",w opens new vertical window
nnoremap <leader>w <C-w>v<C-w>l

"Strip all trailing whitespaces in current file
nnoremap <leader>W :%s/\s\+$//<cr>:let @/=''<CR>

"Mappings for split positioning
"Ccapslock remapped to Ctrl
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

"unbind arrow keys
nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>
inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>
nnoremap k gk
nnoremap j gj

" NERDTREE binding ,t
map <leader>t :NERDTreeToggle<CR>

" Window Focus
nmap <silent> <leader>h :wincmd h<CR>
nmap <silent> <leader>j :wincmd j<CR>
nmap <silent> <leader>k :wincmd k<CR>
nmap <silent> <leader>l :wincmd l<CR>

" ,q to close focused window
nmap <silent> <leader>q <C-w>o

" Reformat/reindent whole file that is currently open
map <leader>f mzgg=G`z

" BufferLine settings
let g:bufferline_active_buffer_left = ''
let g:bufferline_active_buffer_right = ''
let g:bufferline_show_bufnr = 0
let g:bufferline_fname_mod = ':~:.'
let g:bufferline_pathshorten = 1

" Lightline settings
let g:lightline = {
			\ 'colorscheme': 'hybrid',
			\ 'active': {
			\	'left': [ [ 'mode', 'paste'],
			\			  [ 'tabnum' ],
			\			  [ 'fugitive', 'filename', 'modified', 'ctrlpmark' ],
			\			  [ 'go'] ],
			\	'right': [ [ 'lineinfo' ], 
			\			   [ 'percent' ], 
			\			   [ 'fileformat', 'fileencoding', 'filetype' ] ]
			\ },
			\ 'inactive': {
			\	'left': [ [ 'go'],
			\			  [ 'tabnum' ] ]
			\ },
			\ 'component_function': {
			\	'lineinfo': 'LightLineInfo',
			\	'percent': 'LightLinePercent',
			\	'modified': 'LightLineModified',
			\	'filename': 'LightLineFilename',
			\	'go': 'LightLineGo',
			\	'fileformat': 'LightLineFileformat',
			\	'filetype': 'LightLineFiletype',
			\	'fileencoding': 'LightLineFileencoding',
			\	'mode': 'LightLineMode',
			\	'fugitive': 'LightLineFugitive',
			\	'ctrlpmark': 'CtrlPMark',
			\ },
			\ 'tabline': {
			\	'left': [ ['tabs'], ['bufferline'] ],
			\	'right': [ ['fileencoding'] ]
			\ },
			\ 'component': {
			\	'bufferline': '%{MyBufferlineRefresh()}' . bufferline#get_status_string('TabLineSel', 'LightLineLeft_tabline_tabsel_1'),
			\ },
			\ }

function! MyBufferlineRefresh()
  call bufferline#refresh_status()
  let rlen = 4*tabpagenr('$') + len(&fenc) + 8
  call bufferline#trim_status_info(&columns - rlen)
  return ''
endfunction

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

function! LightLineFileformat()
	return winwidth(0) > 70 ? &fileformat : ''
endfunction

function! LightLineFiletype()
	return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype : 'no ft') : ''
endfunction

function! LightLineFileencoding()
	return winwidth(0) > 70 ? (strlen(&fenc) ? &fenc : &enc) : ''
endfunction

function! LightLineInfo()
	return winwidth(0) > 60 ? printf("%3d:%-2d", line('.'), col('.')) : ''
endfunction

function! LightLinePercent()
	return &ft =~? 'vimfiler' ? '' : (100 * line('.') / line('$')) . '%'
endfunction

function! LightLineFugitive()
	return exists('*fugitive#head') ? fugitive#head() : ''
endfunction

function! LightLineGo()
	" return ''
	return exists('*go#jobcontrol#Statusline') ? go#jobcontrol#Statusline() : ''
endfunction

function! LightLineMode()
	let fname = expand('%:t')
	return fname == 'ControlP' ? 'CtrlP' :
				\ &ft == 'vimfiler' ? 'VimFiler' :
				\ winwidth(0) > 60 ? lightline#mode() : ''
endfunction

function! LightLineFilename()
	let fname = expand('%:t')
	if mode() == 't'
		return ''
	endif

	return fname == 'ControlP' ? g:lightline.ctrlp_item :
				\ &ft == 'vimfiler' ? vimfiler#get_status_string() :
				\ ('' != LightLineReadonly() ? LightLineReadonly() . ' ' : '') .
				\ ('' != fname ? fname : '[No Name]')
endfunction

function! LightLineReadonly()
	return &ft !~? 'help' && &readonly ? 'RO' : ''
endfunction

function! CtrlPMark()
	if expand('%:t') =~ 'ControlP'
		call lightline#link('iR'[g:lightline.ctrlp_regex])
		return lightline#concatenate([g:lightline.ctrlp_prev, g:lightline.ctrlp_item
					\ , g:lightline.ctrlp_next], 0)
	else
		return ''
	endif
endfunction

let g:ctrlp_status_func = {
			\ 'main': 'CtrlPStatusFunc_1',
			\ 'prog': 'CtrlPStatusFunc_2',
			\ }

function! CtrlPStatusFunc_1(focus, byfname, regex, prev, item, next, marked)
	let g:lightline.ctrlp_regex = a:regex
	let g:lightline.ctrlp_prev = a:prev
	let g:lightline.ctrlp_item = a:item
	let g:lightline.ctrlp_next = a:next
	return lightline#statusline(0)
endfunction

function! CtrlPStatusFunc_2(str)
	return lightline#statusline(0)
endfunction

let g:ctrlp_cmd = 'CtrlPMRU'
let g:ctrlp_use_caching = 1
let g:clear_cache_on_exit = 1

" configure deoplete to run to startup, and
" TAB to cycle through suggestions
let g:deoplete#enable_at_startup=1

inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function()
	return (pumvisible() ? "\<C-y>" : "" ) . "\<CR>"
endfunction
inoremap <expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

let g:deoplete#sources#jedi#show_docstring = 1

let g:clang_use_library = 1
let g:clang_library_path ='/Library/Developer/CommandLineTools/usr/lib'

let g:vim_tags_auto_generate = 0
