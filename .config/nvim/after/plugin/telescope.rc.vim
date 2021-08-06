if !exists('g:loaded_telescope') | finish | endif

nnoremap <silent> <C-k>f <cmd>Telescope find_files<cr>
nnoremap <silent> <C-k>g <cmd>Telescope live_grep<cr>
nnoremap <silent> <C-k>b <cmd>Telescope buffers<cr>
nnoremap <silent> <C-k>t <cmd>Telescope help_tags<cr>

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

