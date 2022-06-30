local nnoremap = require("kikunvim.keymap").nnoremap
local inoremap = require("kikunvim.keymap").inoremap

nnoremap("<leader>ld", function() vim.lsp.buf.definition() end)
nnoremap("<leader>li", function() vim.lsp.buf.implementation() end)
nnoremap("<leader>lsh", function() vim.lsp.buf.signature_help() end)
nnoremap("<leader>lr", function() vim.lsp.buf.references() end)
nnoremap("<leader>r", function() vim.lsp.buf.rename() end)
nnoremap("<leader>lh", function() vim.lsp.buf.hover() end)
nnoremap("<leader>lc", function() vim.lsp.buf.code_action() end)
nnoremap("<leader>ls", function() vim.diagnostics() end)
nnoremap("<leader>lo", function() vim.diagnostic.open_float() end)
nnoremap("<leader>ln", function() vim.diagnostic.goto_next() end)
nnoremap("<leader>lp", function() vim.diagnostic.goto_prev() end)
nnoremap("<leader>lf", function() vim.cmd(":Neoformat") end)
nnoremap("<leader>lq", function() vim.diagnostic.setqflist() end)

-- LuaSnip
inoremap("<Tab>", function() require("luasnip").jump(1) end)
inoremap("<S-Tab>", function() require("luasnip").jump(-1) end)
