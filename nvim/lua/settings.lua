-- global options
vim.o.autoread = true
vim.o.dir = '/tmp'
vim.o.hlsearch = true
vim.o.ignorecase = true
vim.o.incsearch = true
vim.o.laststatus = 2
vim.o.scrolloff = 12
vim.o.smartcase = true
vim.o.swapfile = false

-- window options
vim.wo.number = true

-- default buffer options
vim.bo.expandtab = true
vim.bo.shiftwidth = 2
vim.bo.tabstop = 2

-- global key mappings
vim.api.nvim_set_keymap('n', '<Space>', '', {})
vim.g.mapleader = ' '
