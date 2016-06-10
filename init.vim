"=========| Default and basic settings |===========

set nocompatible
filetype plugin on

syntax enable
set ruler
set ignorecase
set nolazyredraw
set noerrorbells
set novisualbell
set ttyfast
set encoding=utf8
set background=dark
set omnifunc=syntaxcomplete#Complete
set linebreak
set cursorline
set copyindent
set noswapfile
set mouse=a
if has("mouse_sgr")
    set ttymouse=sgr
end
set cursorline!


" Set for Rails development
" Using ctrl-t and ctrl-d for indent. while in insert
set shiftwidth=4
set laststatus=2

" Better colors in iTerm2. In terminal.app need to turn this off
set termguicolors

" =========| Plugged settings and plugins |========= 
call plug#begin('~/.config/nvim/plugged')
" Keep Plug commands between plug#begin/end.
Plug 'itchyny/lightline.vim'
Plug 'scrooloose/nerdtree'
Plug 'Rip-Rip/clang_complete'
Plug 'fatih/vim-go'
Plug 'NLKNguyen/papercolor-theme'
Plug 'sjl/badwolf'
Plug 'joshdick/onedark.vim'
Plug 'shirataki/lightline-onedark'
Plug '844196/lightline-badwolf.vim'
Plug 'baskerville/bubblegum'
Plug 'frankier/neovim-colors-solarized-truecolor-only'
Plug 'ajh17/VimCompletesMe'
call plug#end()

" =========| Color settings and schemes |========= 
" Turn highlighted string backgrounds for nofrils theme
" let g:nofrils_strbackgrounds=0
" let g:gruvbox_contrast_dark='hard'
colorscheme badwolf

" =========| Some configured paths for |=========
" =========| Clang-complete and YCM    |=========
" This path is needed for YCM


" =========| Some of my keybindings |=========  
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
"#######################################################
"#		end of my keybindings		       #
"#######################################################



" =============| Lightline |===============
let g:lightline = {
	    \ 'colorscheme': 'badwolf',
	    \ 'active': {
	    \   'left': [ [ 'mode', 'paste'],
	    \             [ 'fugitive', 'filename', 'modified', 'ctrlpmark' ],
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
	    \   'ctrlpmark': 'CtrlPMark',
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

" tab completition for NeoCompleteMe, ditched
" you complete me since it is too clunky and heavy"
let b:vcm_tab_complete = 'tags'

