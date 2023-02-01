local status, saga = pcall(require, "lspsaga")
if (not status) then return end

saga.setup({
  ui = {
    winblend = 10,
    border = 'rounded',
    colors = {
      normal_bg = '#002b36'
    }
  }
})

local diagnostic = require("lspsaga.diagnostic")
local opts = { noremap = true, silent = true }
vim.keymap.set('n', ']d', diagnostic.goto_next, opts)
vim.keymap.set('n', '[d', diagnostic.goto_prev, opts)
vim.keymap.set('n', 'gl', diagnostic.show_diagnostics, opts)
vim.keymap.set('n', ']g', '<Cmd>Lspsaga diagnostic_jump_next<CR>', opts)
vim.keymap.set('n', '[g', '<Cmd>Lspsaga diagnostic_jump_prev<CR>', opts)
vim.keymap.set('n', 'K', '<Cmd>Lspsaga hover_doc<CR>', opts)
vim.keymap.set('n', '<C-k>d', '<Cmd>Lspsaga lsp_finder<CR>', opts)
--vim.keymap.set('i', '<C-k>h', '<Cmd>Lspsaga signature_help<CR>', opts)
vim.keymap.set('i', '<C-k>h', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
vim.keymap.set('n', '<C-k>p', '<Cmd>Lspsaga peek_definition<CR>', opts)
vim.keymap.set('n', '<C-k>r', '<Cmd>Lspsaga rename<CR>', opts)

-- code action
local codeaction = require("lspsaga.codeaction")
vim.keymap.set("n", "<leader>ca", function() codeaction:code_action() end, { silent = true })
vim.keymap.set("v", "<leader>ca", function()
  vim.fn.feedkeys(vim.api.nvim_replace_termcodes("<C-U>", true, false, true))
  codeaction:range_code_action()
end, { silent = true })
