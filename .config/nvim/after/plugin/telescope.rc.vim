if !exists('g:loaded_telescope') | finish | endif

nnoremap <silent> <C-x>f <cmd>Telescope find_files<cr>
nnoremap <silent> <C-x>g <cmd>Telescope live_grep<cr>
nnoremap <silent> <C-x>b <cmd>Telescope buffers<cr>
nnoremap <silent> <C-x>t <cmd>Telescope help_tags<cr>

lua << EOF
local actions = require('telescope.actions')
-- Global remapping
------------------------------
require('telescope').setup{
  defaults = {
    mappings = {
      n = {
        ["q"] = actions.close
      },
    },
  }
}
EOF

