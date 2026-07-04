vim.o.termguicolors = true
vim.o.syntax = 'on'
vim.o.errorbells = false
vim.o.smartcase = true
vim.o.showmode = false
vim.bo.swapfile = false
vim.o.backup = false
vim.o.undodir = vim.fn.stdpath('config') .. '/undodir'
vim.o.undofile = true
vim.o.incsearch = true
vim.o.hidden = true
vim.o.completeopt='menuone,noinsert,noselect'
vim.bo.autoindent = true
vim.bo.smartindent = true
vim.o.tabstop = 2
vim.o.softtabstop = 2
vim.o.shiftwidth = 2
vim.o.expandtab = true
vim.wo.number = true
vim.wo.relativenumber = true
vim.wo.signcolumn = 'yes'
vim.wo.wrap = false
vim.g.mapleader = ' '

vim.g.OmniSharp_server_use_mono = 1

require('keybinds')
require('lazyconf')
require('lsp')
require('style')
require('leapkeys')
require('completion')
require('aerialConf')
require('treesittersetup')
require('tsmsetup')
require('foldsetup')
require('tsfilebrowser')

vim.api.nvim_create_autocmd("VimEnter", {
  callback = function()
    local gpg_group = vim.api.nvim_create_augroup("GPGNoSwap", { clear = true })

    vim.api.nvim_create_autocmd({"BufReadPost", "BufNewFile"}, {
      pattern = "*.gpg",
      group = gpg_group,
      callback = function()
        local opt = vim.opt_local
        opt.swapfile = false
        opt.backup = false
        opt.writebackup = false
        opt.undofile = false
        print("Secure GPG buffer activated!")
      end,
    })
  end
})

