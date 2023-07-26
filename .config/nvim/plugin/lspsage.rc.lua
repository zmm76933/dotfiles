local status, saga = pcall(require, "lspsaga")
if (not status) then return end

saga.setup({
  ui = {
    border = 'rounded',
  },
  symbol_in_winbar = {
    enable = false
  },
  lightbulb = {
    enable = false
  },
  outline = {
    layout = 'float'
  }
})

local diagnostic = require("lspsaga.diagnostic")
local opts = { noremap = true, silent = true }
vim.keymap.set('n', ']g', '<Cmd>Lspsaga diagnostic_jump_next<CR>', opts)
vim.keymap.set('n', '[g', '<Cmd>Lspsaga diagnostic_jump_prev<CR>', opts)
vim.keymap.set('n', '<C-k>l', '<Cmd>Lspsaga show_line_diagnostics<CR>', opts)
vim.keymap.set('n', 'K', '<Cmd>Lspsaga hover_doc<CR>', opts)
vim.keymap.set('n', '<C-k>d', '<Cmd>Lspsaga finder<CR>', opts)
vim.keymap.set('n', '<C-k>t', '<Cmd>Lspsaga goto_type_definition<CR>', opts)
--vim.keymap.set('i', '<C-k>h', '<Cmd>Lspsaga signature_help<CR>', opts)
vim.keymap.set('i', '<C-k>h', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
vim.keymap.set('n', '<C-k>p', '<Cmd>Lspsaga peek_definition<CR>', opts)
vim.keymap.set('n', '<C-k>r', '<Cmd>Lspsaga rename<CR>', opts)

-- code action
vim.keymap.set({ "n", "v" }, "<leader>ca", "<cmd>Lspsaga code_action<CR>")
