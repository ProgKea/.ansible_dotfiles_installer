return require("packer").startup(function()
    use("wbthomason/packer.nvim")
    use("nvim-lua/popup.nvim")
    use("nvim-lua/plenary.nvim")
    use("TimUntersberger/neogit")

    -- Appearance
    use("folke/tokyonight.nvim")
    use("nvim-treesitter/nvim-treesitter", {
        run = ":TSUpdate"
    })

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
    use("sbdchd/neoformat")

    -- Speed boosts
    use("tpope/vim-dispatch")
    use("ThePrimeagen/harpoon")
    use("nvim-telescope/telescope.nvim")
    use("nvim-telescope/telescope-fzy-native.nvim")
    use("windwp/nvim-autopairs")
    use {
        'numToStr/Comment.nvim',
        config = function()
            require('Comment').setup()
        end
    }
end)

-- dap
-- use("mfussenegger/nvim-dap")
-- use("rcarriga/nvim-dap-ui")
-- use("theHamsta/nvim-dap-virtual-text")

-- use("williamboman/mason.nvim")
-- use("williamboman/mason-lspconfig.nvim")
