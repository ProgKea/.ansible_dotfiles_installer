local map = vim.api.nvim_set_keymap
local map_opts = {noremap = true, silent = true}

map("", "<C-x>h", ":lua require'tmux'.move_left()<cr>", map_opts)
map("", "<C-x>j", ":lua require'tmux'.move_bottom()<cr>", map_opts)
map("", "<C-x>k", ":lua require'tmux'.move_top()<cr>", map_opts)
map("", "<C-x>l", ":lua require'tmux'.move_right()<cr>", map_opts)

map("", "<C-x><C-h>", ":lua require'tmux'.resize_left()<cr>", map_opts)
map("", "<C-x><C-j>", ":lua require'tmux'.resize_bottom()<cr>", map_opts)
map("", "<C-x><C-k>", ":lua require'tmux'.resize_top()<cr>", map_opts)
map("", "<C-x><C-l>", ":lua require'tmux'.resize_right()<cr>", map_opts)

map("", "<C-x><C-b>", ":only<cr>", map_opts)
map("", "<C-x>.", ":close<cr>", map_opts)

map("", "z", ":vsplit<cr>", map_opts)
map("", "ö", ":split<cr>", map_opts)
