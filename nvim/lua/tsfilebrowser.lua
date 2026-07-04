-- You don't need to set any of these options.
-- IMPORTANT!: this is only a showcase of how you can set default options!

local fb_actions = require("telescope").extensions.file_browser.actions
require("telescope").setup({
  extensions = {
    file_browser = {
      mappings = {
        ["n"] = {
          ["H"] = fb_actions.toggle_respect_gitignore
        },
      },
    },
  },
})

-- To get telescope-file-browser loaded and working with telescope,
-- you need to call load_extension, somewhere after setup function:
require("telescope").load_extension "file_browser"
