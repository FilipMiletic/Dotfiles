call plug#begin('~/.config/nvim/plugged')

Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'zchee/deoplete-clang'
Plug 'zchee/deoplete-jedi'
Plug 'scrooloose/nerdtree'
Plug 'ap/vim-buftabline'
Plug 'w0ng/vim-hybrid'
Plug 'ajh17/Spacegray.vim'
Plug 'sjl/badwolf'
Plug '844196/lightline-badwolf.vim'
Plug 'itchyny/lightline.vim'
Plug 'tpope/vim-fugitive'

call plug#end()

augroup reload_vimrc
    autocmd!
    autocmd bufwritepost $MYVIMRC nested source $MYVIMRC
augroup END

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
set noerrorbells
set novisualbell
set hlsearch                                                                    " highlight searches
set incsearch                                                                   " incremental searching
set ignorecase                                                                  " ignore case for searching
set smartcase                                                                   " do case-sensitive if there's a capital letter
set cursorline
set colorcolumn=80

let g:mapleader = ","
nnoremap <expr> j v:count ? 'j' : 'gj'
nnoremap <expr> k v:count ? 'k' : 'gk'
nnoremap <leader>w <C-w>v
nnoremap <leader>hw <C-w>s
nnoremap <leader><leader> <C-w><C-w>
nnoremap <leader>t :NERDTreeToggle<CR>
nnoremap <Left> :bprev<CR>
nnoremap <Right> :bnext<CR>
nnoremap <Down> :bdelete<CR>
nnoremap <Up> :vnew<CR>
nnoremap <leader>W :%s/\s\+$//<cr>:let @/=''<CR>                                " remove all whitespaces in current file
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-l>l
nmap <silent> <leader>h :wincmd h<CR>
nmap <silent> <leader>j :wincmd j<CR>
nmap <silent> <leader>k :wincmd k<CR>
nmap <silent> <leader>l :wincmd l<CR>
map <leader>f mzgg=G`z                                                          " reformat/reindent whole file that is currently open

let $NVIM_TUI_ENABLE_CURSOR_SHAPE = 1

let g:deoplete#enable_at_startup = 1
let g:deoplete#sources#clang#libclang_path="/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/libclang.dylib"
let g:deoplete#sources#clang#clang_header="/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang"

function! ResetNumbering()
    if (&relativenumber == 1)
        set norelativenumber
        set number
    endif
endfunc

autocmd InsertEnter * :set relativenumber
autocmd InsertLeave * call ResetNumbering()

let g:lightline = {
      \ 'colorscheme': 'badwolf',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste'],
      \             [ 'fugitive', 'filename', 'modified'],
      \             [ 'go'] ],
      \   'right': [ [ 'lineinfo' ], 
      \              [ 'percent' ], 
      \              [ 'fileformat', 'fileencoding', 'filetype' ] ]
      \ },
      \ 'inactive': {
      \   'left': [ [ 'go'] ],
      \ },
      \ 'component_function': {
      \   'lineinfo': 'LightLineInfo',
      \   'percent': 'LightLinePercent',
      \   'modified': 'LightLineModified',
      \   'filename': 'LightLineFilename',
      \   'go': 'LightLineGo',
      \   'fileformat': 'LightLineFileformat',
      \   'filetype': 'LightLineFiletype',
      \   'fileencoding': 'LightLineFileencoding',
      \   'mode': 'LightLineMode',
      \   'fugitive': 'LightLineFugitive',
      \ },
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

syntax on
set termguicolors
set background=dark
colorscheme badwolf
