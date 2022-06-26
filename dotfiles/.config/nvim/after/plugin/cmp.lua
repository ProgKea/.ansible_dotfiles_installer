local cmp = require'cmp'

local source_mapping = {
	buffer = "[Buffer]",
	nvim_lsp = "[LSP]",
	nvim_lua = "[Lua]",
	path = "[Path]",
}
local lspkind = require("lspkind")

cmp.setup({
  snippet = {
    expand = function(args)
      require('luasnip').lsp_expand(args.body)
    end,
  },
  mapping = {
    ['<C-p>'] = cmp.mapping.select_prev_item(),
    ['<C-n>'] = cmp.mapping.select_next_item(),
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.close(),
    ['<CR>'] = cmp.mapping.confirm({
      behavior = cmp.ConfirmBehavior.Replace,
      select = true
    }),
  },
  formatting = {
      format = function(entry, vim_item)
          vim_item.kind = lspkind.presets.default[vim_item.kind]
          local menu = source_mapping[entry.source.name]
          vim_item.menu = menu
          return vim_item
      end,
  },
  sources = cmp.config.sources({
      { name = 'nvim_lsp' },

      { name = 'buffer' },

      { name = "luasnip" },
  }),
})

require("luasnip.loaders.from_vscode").lazy_load()
