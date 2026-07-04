vim.api.nvim_create_autocmd('FileType', {
    pattern = { 'rust', 'python', 'typescript', 'vue', 'csharp' },
    callback = function()
      vim.treesitter.start()
      vim.wo.foldexpr = 'v:lua.vim.treesitter.foldexpr()'
      vim.wo.foldmethod = 'expr'
      vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
    end
})

