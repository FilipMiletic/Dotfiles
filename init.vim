" Default and basic settings
set nocompatible
filetype off
filetype plugin on

" Always using dark background
syntax enable
set ruler
set ignorecase
set lazyredraw
set noerrorbells
set novisualbell
set ttyfast
set encoding=utf8
set background=dark
set omnifunc=syntaxcomplete#Complete
set nowrap
set linebreak
set copyindent
set noswapfile

"set t_Co=256
set laststatus=2

" Better colors in iTerm2. In terminal.app need to turn this off
let $NVIM_TUI_ENABLE_TRUE_COLOR=1

call plug#begin('~/.config/nvim/plugged')
" Keep Plug commands between plug#begin/end.
Plug 'https://github.com/Valloric/YouCompleteMe'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'scrooloose/nerdtree'
Plug 'Rip-Rip/clang_complete'
Plug 'fatih/vim-go'
Plug 'morhetz/gruvbox'
Plug 'NLKNguyen/papercolor-theme'
Plug 'robertmeta/nofrils'
Plug 'cocopon/iceberg.vim'
Plug 'easysid/mod8.vim'
Plug 'sjl/badwolf'
Plug 'joshdick/onedark.vim'
Plug 'ternjs/tern_for_vim'
Plug 'jelera/vim-javascript-syntax'
Plug 'pangloss/vim-javascript'
Plug 'junegunn/vim-easy-align'
call plug#end()

if !exists('g:airline_symbols')
	let g:airline_symbols = {}
endif

" symbols
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep= ''
let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ''
"let g:airline_theme='lucius'

" Turn highlighted string backgrounds for nofrils theme
" let g:nofrils_strbackgrounds=0

"let g:gruvbox_contrast_dark='hard'
colorscheme badwolf

" This path is needed for YCM
let g:ycm_global_ycm_extra_conf='~/.config/nvim/plugged/YouCompleteMe/third_party/ycmd/cpp/ycm/.ycm_extra_conf.py'

let s:clang_library_path='/Library/Developer/CommandLineTools/usr/lib'
if isdirectory(s:clang_library_path)
	let g:clang_library_path=s:clang_library_path
endif

" My Numbering implementation that gets
" activated on C-n, in Insert mode it
" is set to Relative/hybrid, in Normal
" mode it is set to static, in default
" it is hidden.
" TODO: Need to rewrite it as
" plugin and push it to Github.

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

augroup project
	autocmd!
	autocmd BufRead, BufNewFile *.h,*.c set filetype = c.doxygen
augroup END

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

" NERDTREE
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

