require("lspconfig").clangd.setup{}
require("lspconfig").pyright.setup{}
require("lspconfig").texlab.setup{
    filetypes = {"tex", "bib", "markdown"}
}
require("lspconfig").cssls.setup{}
require("lspconfig").hls.setup{}
require("lspconfig").sumneko_lua.setup{}
require("lspconfig").grammarly.setup{}
require ("lspconfig").tsserver.setup{}
require("lspconfig").rust_analyzer.setup{
    cmd = { "rustup", "run", "nightly", "rust-analyzer" }
}
require("lspconfig").gopls.setup({
    cmd = { "gopls", "serve" },
    settings = {
        gopls = {
            analyses = {
                unusedparams = true,
            },
            staticcheck = true,
        },
    },
})
