if !exists('g:loaded_telescope') | finish | endif

nnoremap <silent> <C-k>f <cmd>lua require('telescope.builtin').find_files()<cr>
nnoremap <silent> <C-k>g <cmd>lua require('telescope.builtin').live_grep()<cr>
nnoremap <silent> <C-k>l <cmd>lua require('telescope.builtin').file_browser()<cr>
nnoremap <silent> <C-k>b <cmd>lua require('telescope.builtin').buffers()<cr>
nnoremap <silent> <C-k>t <cmd>lua require('telescope.builtin').help_tags()<cr>
nnoremap <silent> <C-k>; <cmd>lua require('telescope.builtin').resume()<cr>
nnoremap <silent> <C-k>e <cmd>lua require('telescope.builtin').diagnostics()<cr>

lua << EOF
function telescope_buffer_dir()
  return vim.fn.expand('%:p:h')
end

local telescope = require('telescope')
local actions = require('telescope.actions')

telescope.setup{
  defaults = {
    mappings = {
      n = {
        ["q"] = actions.close
      },
    },
  }
}
EOF
