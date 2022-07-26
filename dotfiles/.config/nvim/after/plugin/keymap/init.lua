local Remap = require("kikunvim.keymap")
local nnoremap = Remap.nnoremap
local vnoremap = Remap.vnoremap
local inoremap = Remap.inoremap
local xnoremap = Remap.xnoremap
local nmap = Remap.nmap

-- Move lines
vnoremap("J", ":m '>+1<CR>gv=gv")
vnoremap("K", ":m '<-2<CR>gv=gv")

nnoremap("<C-o>", ":bp<CR>")

nmap("<C-a>", "gg<S-v>G")

-- Open current directory
nmap("te", ":tabedit")
nmap("<S-Tab>", ":tabprev<Return>")
nmap("<Tab>", ":tabnext<Return>")

-- Replace all is aliased to S.
nnoremap("S", ":%s//g<Left><Left>")

nnoremap("Y", "y$")
nnoremap("<leader>x", ":silent !chmod +x %<CR>")
nnoremap("<leader><leader>x", ":source %<CR>")

nnoremap("<leader>jl", ":lua require('telescope.builtin').git_files()<CR>")
nnoremap("<Leader>f", ":lua require('telescope.builtin').find_files()<CR>")
nnoremap("<leader>jr", ":lua require('telescope.builtin').grep_string()<CR>")
nnoremap("<Leader>k", ":Telescope buffers<CR>")
nnoremap("<Leader>jh", ":Telescope help_tags<CR>")
-- nnoremap <leader>gc :lua require('telescope').git_branches()<CR>
-- nnoremap <leader>gw :lua require('telescope').extensions.git_worktree.git_worktrees()<CR>
-- nnoremap <leader>gm :lua require('telescope').extensions.git_worktree.create_git_worktree()<CR>

-- Quickfixlist
nnoremap("qn", ":cn<CR>")
nnoremap("qp", ":cp<CR>")
nnoremap("qo", ":copen<CR>")

-- tmux compile
nnoremap("<leader>mm", "<cmd>w !tmux display-popup -E -y 100\\% -w 100\\% -h 10\\% tmux-compile.sh<CR>")
nnoremap("<leader>mr", "<cmd>w !tmux run-shell 'tmux-compile.sh -r'<CR>")

nnoremap("<C-l>", ":normal! zz<CR>")

-- window navigation
nnoremap("<leader>o", ":only<CR>")
nnoremap("<leader>q", ":close<CR>")
nnoremap("<leader>ö", ":vsplit<CR>")
nnoremap("<leader>h", ":split<CR>")

nnoremap("<leader>jv", ":Ex<CR>")
nnoremap("<leader>jt", ":Lexplore<CR>")
