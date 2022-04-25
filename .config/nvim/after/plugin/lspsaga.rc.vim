if !exists('g:loaded_lspsaga') | finish | endif

lua << EOF
local saga = require 'lspsaga'

saga.init_lsp_saga {
  error_sign = '',
  warn_sign = '',
  hint_sign = '',
  infor_sign = '',
  border_style = "round",
}

EOF

nnoremap <silent> [g :Lspsaga diagnostic_jump_next<CR>
nnoremap <silent> ]g :Lspsaga diagnostic_jump_prev<CR>
nnoremap <silent> K <Cmd>Lspsaga hover_doc<CR>
"nnoremap <silent> K <Cmd>lua require('lspsaga.hover').render_hover_doc()<CR>
inoremap <silent> <C-k>h <Cmd>Lspsaga signature_help<CR>
nnoremap <silent> <C-k>f <Cmd>Lspsaga lsp_finder<CR>
nnoremap <silent> <C-k>p <Cmd>Lspsaga preview_definition<CR>
nnoremap <silent> <C-k>r <Cmd>Lspsaga rename<CR>
nnoremap <silent> <A-r> :Lspsaga open_floaterm<CR>
tnoremap <silent> <A-r> <C-\><C-n>:Lspsaga close_floaterm<CR>
