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
set expandtab
set tabstop=2
set shiftwidth=2
set softtabstop=2
set fileformat=unix
set showtabline=2
set colorcolumn=80

" Reload vimrc file after saving
augroup reload_vimrc " {
  autocmd!
  autocmd BufWritePost $MYVIMRC source $MYVIMRC
augroup END " }

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
Plug '/usr/local/opt/fzf' | Plug 'junegunn/fzf.vim'
Plug 'critiqjo/vim-bufferline'
Plug 'fatih/vim-go'
Plug 'sjl/badwolf'
Plug 'rakr/vim-one'
Plug 'reedes/vim-colors-pencil'
Plug '844196/lightline-badwolf.vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'Shougo/deoplete.nvim', { 'do': function('DoRemote') }
Plug 'elixir-lang/vim-elixir'
Plug 'tpope/vim-surround'
Plug 'zchee/deoplete-jedi'
Plug 'tpope/vim-rails/'
Plug 'vim-ruby/vim-ruby'
call plug#end()

" Themes that I use in combination with iTerm2:
" pencil vim + pencil iTerm2
" one vim + one iTerm2
" badwolf vim + one iTerm2
set background=dark
colorscheme pencil

" When in insert mode set relative and when in 
" normal set standard numbering
function! ResetNumbering()
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

" ,w opens new vertical window
nnoremap <leader>w <C-w>v<C-w>l

" Strip all trailing whitespaces in current file
nnoremap <leader>W :%s/\s\+$//<cr>:let @/=''<CR>

" Mappings for split positioning
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

" ,p for previous or ,n for next buffer
nnoremap <leader>n :bnext<CR>
nnoremap <leader>p :bprevious<CR>

" Reformat/reindent whole file that is currently open
map <leader>f mzgg=G`z

" BufferLine settings
let g:bufferline_active_buffer_left = ''
let g:bufferline_active_buffer_right = ''
let g:bufferline_show_bufnr = 0
let g:bufferline_fname_mod = ':~:.'
let g:bufferline_pathshorten = 1

" Lightline settings
" -----------------------------------------------------------------------------
let g:lightline = {
      \ 'colorscheme': 'badwolf',
      \ 'active': {
      \ 'left': [ ['paste'],
      \       [ 'tabnum' ],
      \       [ 'fugitive', 'filename', 'modified', 'ctrlpmark' ],
      \       [ 'go'] ],
      \ 'right': [ [ 'lineinfo' ],
      \        [ 'percent' ],
      \        [ 'fileformat', 'fileencoding', 'filetype' ] ]
      \ },
      \ 'inactive': {
      \ 'left': [ [ 'go'],
      \       [ 'tabnum' ] ]
      \ },
      \ 'component_function': {
      \ 'lineinfo': 'LightLineInfo',
      \ 'percent': 'LightLinePercent',
      \ 'modified': 'LightLineModified',
      \ 'filename': 'LightLineFilename',
      \ 'go': 'LightLineGo',
      \ 'fileformat': 'LightLineFileformat',
      \ 'filetype': 'LightLineFiletype',
      \ 'fileencoding': 'LightLineFileencoding',
      \ 'mode': 'LightLineMode',
      \ 'fugitive': 'LightLineFugitive',
      \ 'ctrlpmark': 'CtrlPMark',
      \ },
      \ 'tabline': {
      \ 'left': [ ['bufferline'] ],
      \ 'right': [ ['fileencoding'] ]
      \ },
      \ 'component': {
      \ 'bufferline': '%{MyBufferlineRefresh()}' . bufferline#get_status_string('TabLineSel', 'LightLineLeft_tabline_tabsel_1'),
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
"------------------------------------------------------------------------------

" configure deoplete to run to startup, and
" TAB to cycle through suggestions
let g:deoplete#enable_at_startup=1

inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function()
  return (pumvisible() ? "\<C-y>" : "" ) . "\<CR>"
endfunction
inoremap <expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"

let g:deoplete#sources#jedi#show_docstring = 1

let g:clang_use_library = 1
let g:clang_library_path ='/Library/Developer/CommandLineTools/usr/lib'

let g:vim_tags_auto_generate = 0

