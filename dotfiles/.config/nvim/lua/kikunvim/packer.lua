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
    use("williamboman/mason.nvim")
    use("williamboman/mason-lspconfig.nvim")
    use("glepnir/lspsaga.nvim")
    use("hrsh7th/cmp-nvim-lsp")
    use("hrsh7th/cmp-buffer")
    use("hrsh7th/nvim-cmp")
    use("onsails/lspkind-nvim")

    use("ThePrimeagen/harpoon")
    use("nvim-telescope/telescope.nvim")
    use("nvim-telescope/telescope-fzy-native.nvim")
    use("sbdchd/neoformat")
    use("windwp/nvim-autopairs")
    use {
        'numToStr/Comment.nvim',
        config = function()
            require('Comment').setup()
        end
    }
    -- use({
    --     "aserowy/tmux.nvim",
    --     config = function()
    --         require("tmux").setup({
    --             copy_sync = {
    --                 enable = false,
    --                 sync_clipboard = false,
    --                 sync_registers = false,
    --                 sync_deletes = false,
    --                 sync_unnamed = false,
    --             },
    --             navigation = {
    --                 enable_default_keybindings = false,
    --                 cycle_navigation = false,
    --             },
    --             resize = {
    --                 enable_default_keybindings = false,
    --                 resize_step_x = 4,
    --                 resize_step_y = 4,
    --             }
    --         })
    --     end
    -- })

    -- Languages
    use("neovimhaskell/haskell-vim")

    -- dap
    use("mfussenegger/nvim-dap")
    use("rcarriga/nvim-dap-ui")
    use("theHamsta/nvim-dap-virtual-text")
end)
