local augroup = vim.api.nvim_create_augroup
NeovimGroup = augroup('KikuNvim', {})

require("kikunvim.set")
require("kikunvim.theme")
require("kikunvim.packer")

local autocmd = vim.api.nvim_create_autocmd
local yank_group = augroup('HighlightYank', {})

autocmd('TextYankPost', {
    group = yank_group,
    pattern = '*',
    callback = function()
        vim.highlight.on_yank({
            higroup = 'IncSearch',
            timeout = 40,
        })
    end,
})
