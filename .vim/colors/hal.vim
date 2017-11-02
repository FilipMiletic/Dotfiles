" Modification of Xero's blaquemagick, bit lower contrast and changed color

set background=dark
hi clear

if exists("syntax_on")
  syntax reset
endif

let g:colors_name="hal"

hi ColorColumn  term=NONE cterm=NONE ctermfg=NONE ctermbg=235
hi Comment      term=NONE cterm=italic ctermfg=237  ctermbg=NONE
hi Constant     term=NONE cterm=NONE ctermfg=215  ctermbg=NONE
hi Cursor       term=NONE cterm=NONE ctermfg=242  ctermbg=NONE
hi CursorLine   term=NONE cterm=NONE ctermfg=NONE ctermbg=234
hi DiffAdd      term=NONE cterm=NONE ctermfg=215  ctermbg=NONE
hi DiffChange   term=NONE cterm=NONE ctermfg=NONE ctermbg=16
hi DiffDelete   term=NONE cterm=NONE ctermfg=251  ctermbg=16
hi DiffText     term=NONE cterm=NONE ctermfg=251  ctermbg=215
hi Directory    term=NONE cterm=NONE ctermfg=215  ctermbg=16
hi Error        term=NONE cterm=NONE ctermfg=238  ctermbg=66
hi ErrorMsg     term=NONE cterm=NONE ctermfg=66   ctermbg=16
hi FoldColumn   term=NONE cterm=NONE ctermfg=238  ctermbg=NONE
hi Folded       term=NONE cterm=NONE ctermfg=238  ctermbg=NONE
hi Function     term=NONE cterm=NONE ctermfg=251  ctermbg=NONE
hi Identifier   term=NONE cterm=NONE ctermfg=66   ctermbg=NONE
hi IncSearch    term=NONE cterm=NONE ctermfg=132  ctermbg=238
hi NonText      term=NONE cterm=NONE ctermfg=215  ctermbg=NONE
hi Normal       term=NONE cterm=NONE ctermfg=249  ctermbg=NONE
hi PreProc      term=NONE cterm=NONE ctermfg=66   ctermbg=NONE
hi Search       term=NONE cterm=NONE ctermfg=162  ctermbg=239
hi Special      term=NONE cterm=NONE ctermfg=66   ctermbg=NONE
hi SpecialKey   term=NONE cterm=NONE ctermfg=215  ctermbg=NONE
hi Statement    term=NONE cterm=NONE ctermfg=251  ctermbg=NONE
hi StatusLine   term=NONE cterm=bold ctermfg=242  ctermbg=236
hi StatusLineNC term=NONE cterm=NONE ctermfg=240  ctermbg=NONE
hi String       term=NONE cterm=NONE ctermfg=66   ctermbg=NONE
hi TabLineSel   term=NONE cterm=NONE ctermfg=251  ctermbg=NONE
hi Todo         term=NONE cterm=NONE ctermfg=251  ctermbg=66
hi Type         term=NONE cterm=NONE ctermfg=242  ctermbg=NONE
hi VertSplit    term=NONE cterm=NONE ctermfg=237  ctermbg=235
hi Visual       term=NONE cterm=NONE ctermfg=16   ctermbg=215
hi WarningMsg   term=NONE cterm=NONE ctermfg=215  ctermbg=NONE
hi LineNr       term=NONE cterm=NONE ctermbg=235  ctermfg=239
hi CursorLineNr term=NONE cterm=NONE ctermbg=237  ctermfg=16
hi Pmenu        term=NONE cterm=NONE ctermfg=249  ctermbg=16
hi PmenuSel     term=NONE cterm=NONE ctermfg=238  ctermbg=66
hi PmenuSbar    term=NONE cterm=NONE ctermfg=238  ctermbg=66
hi PmenuThumb   term=NONE cterm=NONE ctermfg=238  ctermbg=66
hi Underlined   term=underline cterm=underline ctermfg=NONE   ctermbg=NONE

hi! link diffAdded       DiffAdd
hi! link diffRemoved     DiffDelete
hi! link diffChanged     DiffChange
hi! link Title           Normal
hi! link MoreMsg         Normal
hi! link Question        DiffChange
hi! link TabLine         StatusLineNC
hi! link TabLineFill     StatusLineNC
hi! link VimHiGroup      VimGroup
