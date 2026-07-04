require("aerial").setup({
  -- optionally use on_attach to set keymaps when aerial has attached to a buffer
  backends = { "lsp", "treesitter", "markdown", "man" },

  on_attach = function(bufnr)
    -- Jump forwards/backwards with '{' and '}'
    vim.keymap.set("n", "{", "<cmd>AerialPrev<CR>", { buffer = bufnr })
    vim.keymap.set("n", "}", "<cmd>AerialNext<CR>", { buffer = bufnr })
  end,

  layout = {
    max_width = 0.5,
    width = 100,
    min_width = 100
  },

  disable_max_lines = 50000,
})
-- You probably also want to set a keymap to toggle aerial
vim.keymap.set("n", "<leader>ae", "<cmd>AerialToggle!<CR>")

