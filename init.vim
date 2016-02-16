" Default and basic settings
set nocompatible
filetype off
filetype plugin on

" Always using dark background
<<<<<<< HEAD
syntax enable
set omnifunc=syntaxcomplete#Complete
set laststatus=2
set ruler
set background=dark
set ignorecase
set lazyredraw
set noerrorbells
set novisualbell
set ttyfast
set encoding=utf8
=======
set background=dark
set omnifunc=syntaxcomplete#Complete

set laststatus=2

augroup project
	autocmd!
	autocmd BufRead, BufNewFile *.h,*.c set filetype=c.doxygen
augroup END

syntax enable
set ruler

set ignorecase

"set t_Co=256

set noerrorbells
set novisualbell

set encoding=utf8
"set relativenumber
set laststatus=2
>>>>>>> c16307df9060f474a42893f343508a99d246d6d1

" Better colors in iTerm2. In terminal.app need to turn this off
let $NVIM_TUI_ENABLE_TRUE_COLOR=1

call plug#begin('~/.config/nvim/plugged')
" Keep Plug commands between plug#begin/end.
Plug 'https://github.com/Valloric/YouCompleteMe'
Plug 'bling/vim-airline'
Plug 'scrooloose/nerdtree'
Plug 'Rip-Rip/clang_complete'
Plug 'NLKNguyen/papercolor-theme'
Plug 'easysid/mod8.vim'
Plug 'sjl/badwolf'
<<<<<<< HEAD
Plug 'joshdick/onedark.vim'
Plug 'vim-airline/vim-airline-themes'
=======
>>>>>>> c16307df9060f474a42893f343508a99d246d6d1
Plug 'ternjs/tern_for_vim'
Plug 'jelera/vim-javascript-syntax'
Plug 'pangloss/vim-javascript'
Plug 'junegunn/vim-easy-align'
call plug#end()

<<<<<<< HEAD
if !exists('g:airline_symbols')
	let g:airline_symbols = {}
endif

" symbols
let g:airline_left_sep = ' '
let g:airline_left_alt_sep = ' '
let g:airline_right_sep= ' '
let g:airline_right_alt_sep = ' '
let g:airline_symbols.branch = ' '
let g:airline_symbols.readonly = ' '
let g:airline_symbols.linenr = ' '
 
" Pick one
colorscheme onedark
" This one accordingly
let g:airline_theme		='onedark'
=======
" Pick one
colorscheme mod8
" This one accordingly
let g:airline_theme='base16'
>>>>>>> c16307df9060f474a42893f343508a99d246d6d1

" This path is needed for YCM
let g:ycm_global_ycm_extra_conf = '~/.config/nvim/plugged/YouCompleteMe/third_party/ycmd/cpp/ycm/.ycm_extra_conf.py'

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
<<<<<<< HEAD

augroup project
	autocmd!
	autocmd BufRead, BufNewFile *.h,*.c set filetype = c.doxygen
augroup END

=======
>>>>>>> c16307df9060f474a42893f343508a99d246d6d1
