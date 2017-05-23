call plug#begin('~/.vim/plugged')
Plug 'neomake/neomake'                                  " linter
Plug 'xero/blaquemagick.vim'
Plug 'scrooloose/nerdtree'
Plug 'maralla/completor.vim'
Plug '/usr/local/opt/fzf' | Plug 'junegunn/fzf.vim'     " fuzzy file navigation
call plug#end()


"------------------------------------------------------------------------------
" EDITOR CONFIGURATIONS
"------------------------------------------------------------------------------
set ttyfast
set hidden
set viminfo=
set backspace=2
set expandtab
set numberwidth=5
set tabstop=4
set shiftwidth=4
set softtabstop=4
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
set fillchars=|
set ignorecase
set lazyredraw
set smartcase
set colorcolumn=80
set cursorline
set shortmess+=I
set clipboard+=unnamed
"set statusline=%<%f\ (%{&ft})\ %-4(%m%)%=%-19(%3l,%02c%03V%)
set statusline=\ \%f%m%r%h%w\ ::\ %y\ [%{&ff}]\%=\ [%p%%:\ %l/%L]\ 
set laststatus=2
set cmdheight=1

"------------------------------------------------------------------------------
"  LINTING CODE
"------------------------------------------------------------------------------
autocmd! BufWritePost * Neomake
" remove trailing white space on save
command! Nows :%s/\s\+$//
" remove blank lines
command! Nobl :g/^\s*$/d
"autocmd BufReadPost *
            "\ if line("'\"") > 0 && line ("'\"") <= line("$") |
            " \   exe "normal g'\"" |
            "\ endif

let g:neomake_python_flake8_maker = {
    \ 'args': ['--ignore=E221,E241,E272,E251,W702,E203,E201,E202',  '--format=default'],
    \ 'errorformat':
        \ '%E%f:%l: could not compile,%-Z%p^,' .
        \ '%A%f:%l:%c: %t%n %m,' .
        \ '%A%f:%l: %t%n %m,' .
        \ '%-G%.%#',
    \ }
let g:neomake_python_enabled_makers = ['flake8']

let g:pydoc_cmd = '/usr/bin/pydoc'

" use omni-complete
let b:vcm_tab_complete = 'omni'
set omnifunc=syntaxcomplete#Complete
" select the completion with enter
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
" close preview on completion complete
autocmd CompleteDone * pclose
" or disable previews completely
" set completeopt-=preview
let NERDTreeMinimalUI=1


"------------------------------------------------------------------------------
"  MAPPINGS
"------------------------------------------------------------------------------
let mapleader=","

nnoremap <expr> j v:count ? 'j' : 'gj'
nnoremap <expr> k v:count ? 'k' : 'gk'

" Vertical split
nnoremap <leader>w <C-w>v

" Choose other split
nnoremap <leader><leader> <C-w><C-w>

" Format current buffer
map <leader>z mzgg=G`z

" Open FZF at home folder
map <leader>f :FZF ~<CR>
map <leader>b :Buffers<CR>

" Cycle next/previous tab
nnoremap <leader>p :tabprev<CR>
nnoremap <leader>n :tabnext<CR>

" Open Nerdtree
nnoremap <leader>q :NERDTreeToggle<CR>

" Create new tab
nnoremap <leader>t :tabnew<CR>

" Ctrl-L clears the highlight from the last search
noremap <C-l> :nohlsearch<CR><C-l>
noremap! <C-l> <ESC>:nohlsearch<CR><C-l>

" Indent if we're at the beginning of a line. Else, do completion
function! InsertTabWrapper()
    let col = col('.') - 1
    if !col || getline('.')[col - 1] !~ '\k'
        return "\<tab>"
    else
        return "\<c-p>"
    endif
endfunction
inoremap <expr> <tab> InsertTabWrapper()
inoremap <s-tab> <c-n>

"set termguicolors
set background=dark
colorscheme blaquemagick
