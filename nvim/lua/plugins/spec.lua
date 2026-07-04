return {
  { "neovim-treesitter/treesitter-parser-registry" },
  { "neovim-treesitter/nvim-treesitter" },
  { "sheerun/vim-polyglot" },

  { "romus204/tree-sitter-manager.nvim" },

  {
    "kevinhwang91/nvim-ufo",
    dependencies =
    { 
      { 'kevinhwang91/promise-async' }
    }
  },

  { "neovim/nvim-lspconfig" },

  { "nvim-lua/popup.nvim" },
  { "jremmen/vim-ripgrep" },
  { "ellisonleao/gruvbox.nvim" },
  { "ThePrimeagen/harpoon" },
  { "christoomey/vim-tmux-navigator" },

  { "stevearc/aerial.nvim" },

  { "hrsh7th/nvim-cmp" },
  { "hrsh7th/cmp-nvim-lsp" },
  { "hrsh7th/vim-vsnip" },
  { "hrsh7th/cmp-vsnip" },

  { "sindrets/diffview.nvim" },

  { "tpope/vim-fugitive" },
  
  { "christoomey/vim-conflicted" },

  { "junegunn/gv.vim" },

  { "jamessan/vim-gnupg" },

  { "https://codeberg.org/andyg/leap.nvim" },

  {
    "MrcJkb/haskell-tools.nvim",
    branch = "main",
    dependencies =
    {
      { "nvim-lua/plenary.nvim" },
      { "nvim-telescope/telescope.nvim" }
    },
  },

  {
      "nvim-telescope/telescope-file-browser.nvim",
      dependencies =
      {
        { "nvim-telescope/telescope.nvim" },
        { "nvim-lua/plenary.nvim" }
      }
  },

  { "rmagatti/goto-preview" },

  { "chrisbra/unicode.vim" }
}

