local status, saga = pcall(require, "lspsaga")
if (not status) then return end

saga.init_lsp_saga {
  server_filetype_map = {
    typescript = 'typescript'
  }
}

local opts = { noremap = true, silent = true }
vim.keymap.set('n', ']g', '<Cmd>Lspsaga diagnostic_jump_next<CR>', opts)
vim.keymap.set('n', '[g', '<Cmd>Lspsaga diagnostic_jump_prev<CR>', opts)
vim.keymap.set('n', 'K', '<Cmd>Lspsaga hover_doc<CR>', opts)
vim.keymap.set('n', '<C-k>d', '<Cmd>Lspsaga lsp_finder<CR>', opts)
--vim.keymap.set('i', '<C-k>h', '<Cmd>Lspsaga signature_help<CR>', opts)
vim.keymap.set('i', '<C-k>h', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
vim.keymap.set('n', '<C-k>p', '<Cmd>Lspsaga peek_definition<CR>', opts)
vim.keymap.set('n', '<C-k>r', '<Cmd>Lspsaga rename<CR>', opts)
