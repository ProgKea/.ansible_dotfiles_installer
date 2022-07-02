let s:cpo_save=&cpo
set cpo&vim
inoremap <silent> <C-N> <Cmd>call v:lua.cmp.utils.keymap.set_map(7)
inoremap <silent> <C-D> <Cmd>call v:lua.cmp.utils.keymap.set_map(6)
inoremap <silent> <C-E> <Cmd>call v:lua.cmp.utils.keymap.set_map(5)
inoremap <silent> <C-Space> <Cmd>call v:lua.cmp.utils.keymap.set_map(4)
inoremap <silent> <C-F> <Cmd>call v:lua.cmp.utils.keymap.set_map(3)
inoremap <silent> <C-P> <Cmd>call v:lua.cmp.utils.keymap.set_map(1)
cnoremap <silent> <Plug>(TelescopeFuzzyCommandSearch) e "lua require('telescope.builtin').command_history { default_text = [=[" . escape(getcmdline(), '"') . "]=] }"
noremap! <silent> <Plug>luasnip-expand-repeat <Cmd>lua require'luasnip'.expand_repeat()
noremap! <silent> <Plug>luasnip-delete-check <Cmd>lua require'luasnip'.unlink_current_if_deleted()
inoremap <silent> <Plug>luasnip-jump-prev <Cmd>lua require'luasnip'.jump(-1)
inoremap <silent> <Plug>luasnip-jump-next <Cmd>lua require'luasnip'.jump(1)
inoremap <silent> <Plug>luasnip-prev-choice <Cmd>lua require'luasnip'.change_choice(-1)
inoremap <silent> <Plug>luasnip-next-choice <Cmd>lua require'luasnip'.change_choice(1)
inoremap <silent> <Plug>luasnip-expand-snippet <Cmd>lua require'luasnip'.expand()
inoremap <silent> <Plug>luasnip-expand-or-jump <Cmd>lua require'luasnip'.expand_or_jump()
inoremap <C-W> u
inoremap <C-U> u
nnoremap  ggVG
nnoremap 	 :tabnext
nnoremap  :bp
nnoremap  :lua require('telescope.builtin').git_files()
nnoremap  ps :lua require('telescope.builtin').grep_string({ search = vim.fn.input("Grep For > ")})
nnoremap  ph :Telescope help_tags
nnoremap  pb :Telescope buffers
nnoremap  pw :lua require('telescope.builtin').grep_string { search = vim.fn.expand("<cword>") }
nnoremap  f :lua require('telescope.builtin').find_files()
nnoremap  x :silent !chmod +x %
nnoremap  m :w:!make
nnoremap  u :UndotreeShow
nnoremap  pt :Lexplore
nnoremap  pv :Ex
xnoremap # y?\V"
omap <silent> % <Plug>(MatchitOperationForward)
xmap <silent> % <Plug>(MatchitVisualForward)
nmap <silent> % <Plug>(MatchitNormalForward)
xnoremap * y/\V"
vnoremap J :m '>+1gv=gv
vnoremap K :m '<-2gv=gv
nnoremap S :%s//g<Left><Left>
nnoremap Y y$
omap <silent> [% <Plug>(MatchitOperationMultiBackward)
xmap <silent> [% <Plug>(MatchitVisualMultiBackward)
nmap <silent> [% <Plug>(MatchitNormalMultiBackward)
omap <silent> ]% <Plug>(MatchitOperationMultiForward)
xmap <silent> ]% <Plug>(MatchitVisualMultiForward)
nmap <silent> ]% <Plug>(MatchitNormalMultiForward)
xmap a% <Plug>(MatchitVisualTextObject)
xmap gx <Plug>NetrwBrowseXVis
nmap gx <Plug>NetrwBrowseX
omap <silent> g% <Plug>(MatchitOperationBackward)
xmap <silent> g% <Plug>(MatchitVisualBackward)
nmap <silent> g% <Plug>(MatchitNormalBackward)
nnoremap qo :copen
nnoremap qp :cp
nnoremap qn :cn
nnoremap te :tabedit
nnoremap <C-P> :lua require('telescope.builtin').git_files()
nnoremap <S-Tab> :tabprev
nnoremap <C-A> ggVG
nnoremap <C-O> :bp
nnoremap <Plug>PlenaryTestFile :lua require('plenary.test_harness').test_directory(vim.fn.expand("%:p"))
snoremap <silent> <Plug>luasnip-jump-prev <Cmd>lua require'luasnip'.jump(-1)
snoremap <silent> <Plug>luasnip-jump-next <Cmd>lua require'luasnip'.jump(1)
snoremap <silent> <Plug>luasnip-prev-choice <Cmd>lua require'luasnip'.change_choice(-1)
snoremap <silent> <Plug>luasnip-next-choice <Cmd>lua require'luasnip'.change_choice(1)
snoremap <silent> <Plug>luasnip-expand-snippet <Cmd>lua require'luasnip'.expand()
snoremap <silent> <Plug>luasnip-expand-or-jump <Cmd>lua require'luasnip'.expand_or_jump()
noremap <silent> <Plug>luasnip-expand-repeat <Cmd>lua require'luasnip'.expand_repeat()
noremap <silent> <Plug>luasnip-delete-check <Cmd>lua require'luasnip'.unlink_current_if_deleted()
xnoremap <silent> <Plug>NetrwBrowseXVis :call netrw#BrowseXVis()
nnoremap <silent> <Plug>NetrwBrowseX :call netrw#BrowseX(netrw#GX(),netrw#CheckIfRemote(netrw#GX()))
xmap <silent> <Plug>(MatchitVisualTextObject) <Plug>(MatchitVisualMultiBackward)o<Plug>(MatchitVisualMultiForward)
onoremap <silent> <Plug>(MatchitOperationMultiForward) :call matchit#MultiMatch("W",  "o")
onoremap <silent> <Plug>(MatchitOperationMultiBackward) :call matchit#MultiMatch("bW", "o")
xnoremap <silent> <Plug>(MatchitVisualMultiForward) :call matchit#MultiMatch("W",  "n")m'gv``
xnoremap <silent> <Plug>(MatchitVisualMultiBackward) :call matchit#MultiMatch("bW", "n")m'gv``
nnoremap <silent> <Plug>(MatchitNormalMultiForward) :call matchit#MultiMatch("W",  "n")
nnoremap <silent> <Plug>(MatchitNormalMultiBackward) :call matchit#MultiMatch("bW", "n")
onoremap <silent> <Plug>(MatchitOperationBackward) :call matchit#Match_wrapper('',0,'o')
onoremap <silent> <Plug>(MatchitOperationForward) :call matchit#Match_wrapper('',1,'o')
xnoremap <silent> <Plug>(MatchitVisualBackward) :call matchit#Match_wrapper('',0,'v')m'gv``
xnoremap <silent> <Plug>(MatchitVisualForward) :call matchit#Match_wrapper('',1,'v'):if col("''") != col("$") | exe ":normal! m'" | endifgv``
nnoremap <silent> <Plug>(MatchitNormalBackward) :call matchit#Match_wrapper('',0,'n')
nnoremap <silent> <Plug>(MatchitNormalForward) :call matchit#Match_wrapper('',1,'n')
inoremap <silent>  <Cmd>call v:lua.cmp.utils.keymap.set_map(6)
inoremap <silent>  <Cmd>call v:lua.cmp.utils.keymap.set_map(5)
inoremap <silent>  <Cmd>call v:lua.cmp.utils.keymap.set_map(3)
inoremap <silent>  <Cmd>call v:lua.cmp.utils.keymap.set_map(2)
inoremap <silent>  <Cmd>call v:lua.cmp.utils.keymap.set_map(7)
inoremap <silent>  <Cmd>call v:lua.cmp.utils.keymap.set_map(1)
inoremap  u
inoremap  u
let &cpo=s:cpo_save
unlet s:cpo_save
set backspace=start,eol,indent
set clipboard=unnamedplus
set expandtab
set fileencodings=utf-8,sjis,euc-jp,latin
set guicursor=
set helplang=en
set nohlsearch
set ignorecase
set isfname=#,$,%,+,,,-,.,/,48-57,=,@,@-@,_,~
set laststatus=3
set lazyredraw
set runtimepath=~/.config/nvim,/etc/xdg/nvim,~/.local/share/nvim/site,~/.local/share/nvim/site/pack/packer/start/packer.nvim,~/.local/share/nvim/site/pack/*/start/*,/usr/local/share/nvim/site,/usr/share/nvim/site,/usr/local/share/nvim/runtime,/usr/local/share/nvim/runtime/pack/dist/opt/matchit,/usr/local/lib/nvim,~/.local/share/nvim/site/pack/*/start/*/after,/usr/share/nvim/site/after,/usr/local/share/nvim/site/after,~/.local/share/nvim/site/after,/etc/xdg/nvim/after,~/.config/nvim/after
set scrolloff=8
set shiftwidth=4
set shortmess=lntixTOcofF
set smartcase
set smartindent
set softtabstop=4
set noswapfile
set synmaxcol=180
set tabstop=4
set termguicolors
set title
set undodir=~/.vim/undodir
set undofile
set updatetime=50
set window=29
" vim: set ft=vim :
