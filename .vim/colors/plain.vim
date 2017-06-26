" vim:sw=8:ts=8
"
" act like t_Co=0 but use (256) color on just a few things
"

set background=dark

hi clear
if exists("syntax_on")
	syntax reset
endif

let colors_name = "plain"

hi Comment		cterm=NONE		ctermfg=66
hi Constant		cterm=NONE		ctermfg=NONE
hi CursorLine           cterm=None              ctermbg=236
hi CursorLineNr		cterm=bold		ctermfg=238
hi DiffAdd		cterm=bold		ctermfg=NONE
hi DiffChange		cterm=bold		ctermfg=NONE
hi DiffDelete		cterm=bold		ctermfg=NONE
hi DiffText		cterm=reverse		ctermfg=NONE
hi Directory		cterm=bold		ctermfg=NONE
hi Error		cterm=reverse		ctermfg=9	ctermbg=15
hi ErrorMsg		cterm=reverse		ctermfg=9	ctermbg=15
hi FoldColumn		cterm=standout		ctermfg=NONE
hi Folded		cterm=standout		ctermfg=NONE
hi Identifier		cterm=NONE		ctermfg=NONE
hi Ignore		cterm=bold		ctermfg=NONE
hi IncSearch		cterm=reverse		ctermfg=NONE
hi LineNr		cterm=NONE		ctermfg=236
hi MatchParen		cterm=bold		ctermfg=none	ctermbg=185
hi ModeMsg		cterm=bold		ctermfg=NONE
hi MoreMsg		cterm=bold		ctermfg=NONE
hi NonText		cterm=bold		ctermfg=NONE
hi SignColumn           ctermbg=NONE
hi PreProc		cterm=NONE		ctermfg=NONE
hi Question		cterm=standout		ctermfg=NONE
hi Search		cterm=reverse		ctermfg=NONE
hi Special		cterm=bold		ctermfg=NONE
hi SpecialKey		cterm=bold		ctermfg=NONE
hi Statement		cterm=bold		ctermfg=NONE
hi StatusLine		cterm=bold,reverse	ctermfg=236     ctermbg=249
hi StatusLineNC		cterm=reverse		ctermfg=236     ctermbg=247
hi Title		cterm=bold		ctermfg=NONE
hi Todo			cterm=bold,standout	ctermfg=185	ctermbg=0
hi Type			cterm=bold		ctermfg=NONE
hi Underlined		cterm=NONE		ctermfg=NONE
hi VertSplit		cterm=reverse		ctermfg=NONE
hi Visual		cterm=reverse		ctermfg=NONE
hi VisualNOS		cterm=bold,NONE	        ctermfg=NONE
hi WarningMsg		cterm=standout		ctermfg=NONE
hi WildMenu		cterm=standout		ctermfg=NONE

hi ColorColumn							ctermbg=236

" for highlighting stray spaces/tabs (requires match statements in vimrc)
hi ExtraWhitespace	cterm=reverse		ctermfg=185	ctermbg=NONE

" mostly for nerdtree
hi VertSplit		cterm=bold		ctermfg=237	ctermbg=NONE
