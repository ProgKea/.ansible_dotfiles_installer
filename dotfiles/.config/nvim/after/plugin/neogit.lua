local neogit = require("neogit")
local nnoremap = require("kikunvim.keymap").nnoremap

neogit.setup {
  disable_commit_confirmation = true,
}

nnoremap("<leader>gs", function()
    neogit.open({ kind = "replace" })
end);
