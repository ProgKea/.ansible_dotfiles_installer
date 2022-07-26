local Remap = require("kikunvim.keymap")
local nnoremap = Remap.nnoremap
local vnoremap = Remap.vnoremap
local inoremap = Remap.inoremap
local xnoremap = Remap.xnoremap
local nmap = Remap.nmap

-- Move lines
vnoremap("J", "<cmd>m '>+1<CR>gv=gv")
vnoremap("K", "<cmd>m '<-2<CR>gv=gv")

nmap("<C-a>", "gg<S-v>G")

-- Open current directory
nmap("te", "<cmd>tabedit")
nmap("<S-Tab>", "<cmd>tabprev<Return>")
nmap("<Tab>", "<cmd>tabnext<Return>")

-- Replace all is aliased to S.
nnoremap("S", "<cmd>%s//g<Left><Left>")

nnoremap("Y", "y$")
nnoremap("<leader>x", "<cmd>silent !chmod +x %<CR>")
nnoremap("<leader><leader>x", "<cmd>source %<CR>")

nnoremap("<leader>jf", "<cmd>lua require('telescope.builtin').git_files()<CR>")
nnoremap("<Leader>f", "<cmd>lua require('telescope.builtin').find_files()<CR>")
nnoremap("<leader>jl", "<cmd>Ex ~<CR>")
nnoremap("<leader>jr", "<cmd>lua require('telescope.builtin').grep_string()<CR>")
nnoremap("<Leader>k", "<cmd>Telescope buffers<CR>")
nnoremap("<Leader>jh", "<cmd>Telescope help_tags<CR>")
-- nnoremap <leader>gc <cmd>lua require('telescope').git_branches()<CR>
-- nnoremap <leader>gw <cmd>lua require('telescope').extensions.git_worktree.git_worktrees()<CR>
-- nnoremap <leader>gm <cmd>lua require('telescope').extensions.git_worktree.create_git_worktree()<CR>

-- Quickfixlist
nnoremap("qn", "<cmd>cn<CR>")
nnoremap("qp", "<cmd>cp<CR>")
nnoremap("qo", "<cmd>copen<CR>")

nnoremap("<C-l>", "<cmd>normal! zz<CR>")

-- navigation
nnoremap("<leader>o", "<cmd>only<CR>")
nnoremap("<leader>q", "<cmd>close<CR>")
nnoremap("<leader>ö", "<cmd>vsplit<CR>")
nnoremap("<leader>h", "<cmd>split<CR>")
nnoremap("<leader>0", "<cmd>normal! <C-w>w<CR>")
nnoremap("<leader>9", "<cmd>normal! <C-w>W<CR>")
nnoremap("<C-o>", "<cmd>normal! <C-^><CR>")

nnoremap("<leader>jv", "<cmd>Ex<CR>")
nnoremap("<leader>jt", "<cmd>Lexplore<CR>")

nnoremap("<leader>ms", ":!")
nnoremap("<M-x>", ":") -- thats how you know he uses emacs
nnoremap("<leader>mm", ":Dispatch ")
nnoremap("<leader>mr", "<cmd>Dispatch!<CR>")
