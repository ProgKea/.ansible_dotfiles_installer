-- Use a protected call so we don't error out on first use
local status_ok, packer = pcall(require, "packer")
if not status_ok then
  return
end

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
  use("neovim/nvim-lspconfig")
  use("glepnir/lspsaga.nvim")
  use("L3MON4D3/LuaSnip")
  use("hrsh7th/cmp-nvim-lsp")
  use("hrsh7th/cmp-buffer")
  use("hrsh7th/nvim-cmp")
  use("onsails/lspkind-nvim")

  use("ThePrimeagen/harpoon")
  use("windwp/nvim-autopairs")
  use("nvim-telescope/telescope.nvim")
  use("nvim-telescope/telescope-fzy-native.nvim")
  use("sbdchd/neoformat")
  use("mbbill/undotree")
end)
