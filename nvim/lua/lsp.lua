local lspconfig = require'lspconfig'
local function custom_on_attach(client)
  print('Attaching to ' .. client.name)
end
local default_config = {
  on_attach = custom_on_attach,
}
-- setup language servers here
lspconfig.tsserver.setup(default_config)
lspconfig.pyright.setup(default_config)
lspconfig.volar.setup(default_config)
lspconfig.csharp_ls.setup({
    root_dir = function(startpath)
        return lspconfig.util.root_pattern("*.sln")(startpath)
            or lspconfig.util.root_pattern("*.csproj")(startpath)
            or lspconfig.util.root_pattern("*.fsproj")(startpath)
            or lspconfig.util.root_pattern(".git")(startpath)
    end,
    on_attach = custom_on_attach,
    capabilities = capabilities,
})

-- Global mappings.
-- See `:help vim.diagnostic.*` for documentation on any of the below functions
vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next)
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist)

-- Use LspAttach autocommand to only map the following keys
-- after the language server attaches to the current buffer
vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('UserLspConfig', {}),
  callback = function(ev)
    -- Enable completion triggered by <c-x><c-o>
    vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

    -- Buffer local mappings.
    -- See `:help vim.lsp.*` for documentation on any of the below functions
    local opts = { buffer = ev.buf }
    vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
    vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
    vim.keymap.set('n', 'gt', vim.lsp.buf.type_definition, opts)
    vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
    vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
    vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, opts)
    vim.keymap.set('i', '<C-k>', vim.lsp.buf.signature_help, opts)
    vim.keymap.set('n', '<leader>wa', vim.lsp.buf.add_workspace_folder, opts)
    vim.keymap.set('n', '<leader>wr', vim.lsp.buf.remove_workspace_folder, opts)
    vim.keymap.set('n', 'gw', vim.lsp.buf.document_symbol, opts)
    vim.keymap.set('n', 'gW', vim.lsp.buf.workspace_symbol, opts)
    vim.keymap.set('n', '<leader>wl', function()
      print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
    end, opts)
    vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename, opts)
    vim.keymap.set('n', '<leader>af', vim.lsp.buf.code_action, opts)
    vim.keymap.set('n', '<leader>fe', vim.diagnostic.open_float, opts)
    vim.keymap.set('n', '<leader>f', function()
      vim.lsp.buf.format { async = true }
    end, opts)

    vim.keymap.set("n", "[c", function()
      require("treesitter-context").go_to_context()
    end, { silent = true })

    -- goto preview
    vim.keymap.set('n', 'gpd', function()
      require("goto-preview").goto_preview_definition()
    end, { silent = true })
    vim.keymap.set('n', 'gpt', function()
      require("goto-preview").goto_preview_type_definition()
    end, { silent = true })
    vim.keymap.set('n', 'gpi', function()
      require("goto-preview").goto_preview_implementation()
    end, { silent = true })
    vim.keymap.set('n', 'gpr', function()
      require("goto-preview").goto_preview_references()
    end, { silent = true })
    vim.keymap.set('n', 'gP', function()
      require("goto-preview").close_all_win()
    end, { silent = true })

  end,
})
