vim.opt.guicursor = ""
vim.opt.termguicolors = true

vim.g.mapleader = " "

vim.opt.updatetime = 50
vim.opt.nu = true
vim.opt.relativenumber = true
vim.opt.fileencodings = "utf-8,sjis,euc-jp,latin"
vim.opt.encoding = "utf-8"
vim.opt.title = true
vim.opt.autoindent = true
vim.opt.backup = false
vim.opt.swapfile = false
vim.opt.undodir = os.getenv("HOME") .. "/.vim/undodir"
vim.opt.undofile = true
vim.opt.showcmd = true
vim.opt.cmdheight=1
vim.opt.laststatus=3
vim.opt.scrolloff=8
vim.opt.expandtab = true
vim.opt.clipboard = vim.opt.clipboard + "unnamedplus"
vim.opt.signcolumn = "yes"

vim.opt.isfname:append("@-@")
vim.opt.hidden = true
vim.opt.errorbells = false

vim.opt.hlsearch = false
vim.opt.incsearch = true

vim.opt.lazyredraw = true
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.lazyredraw = true
vim.opt.synmaxcol = 180
vim.opt.smarttab = true
vim.opt.colorcolumn = "80"

vim.opt.shortmess:append("c")
vim.opt.isfname:append("@-@")

-- indents
vim.opt.shiftwidth = 4
vim.opt.softtabstop = 4
vim.opt.tabstop = 4
vim.opt.expandtab = true
vim.opt.ai = true
vim.opt.si = true
vim.opt.wrap = false
vim.opt.backspace = "start,eol,indent"

-- disable mouse
vim.opt.mouse = "c"

-- netrw
vim.g.netrw_browse_split = 0
vim.g.netrw_banner = 0
vim.g.netrw_winsize = 25
vim.g.netrw_altv = 1
