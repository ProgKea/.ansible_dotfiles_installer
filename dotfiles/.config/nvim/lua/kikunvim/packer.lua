return require("packer").startup(function()
  use("wbthomason/packer.nvim")

  use("nvim-lua/popup.nvim")
  use("nvim-lua/plenary.nvim")

  use("TimUntersberger/neogit")

  -- Appearance
  use("folke/tokyonight.nvim")
  use("gruvbox-community/gruvbox")
  use("nvim-treesitter/nvim-treesitter", {
    run = ":TSUpdate"
  })
  use("kyazdani42/nvim-web-devicons")

  -- Lsp
  use("L3MON4D3/LuaSnip")
  use("saadparwaiz1/cmp_luasnip")
  use("rafamadriz/friendly-snippets")
  use("neovim/nvim-lspconfig")
  use("glepnir/lspsaga.nvim")
  use("hrsh7th/cmp-nvim-lsp")
  use("hrsh7th/cmp-buffer")
  use("hrsh7th/nvim-cmp")
  use("onsails/lspkind-nvim")

  use("ThePrimeagen/harpoon")
  use("nvim-telescope/telescope.nvim")
  use("nvim-telescope/telescope-fzy-native.nvim")
  use("sbdchd/neoformat")
  use("mbbill/undotree")
end)

--use("windwp/nvim-autopairs")
