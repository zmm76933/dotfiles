local discipline = require("zmm76933.discipline")

discipline.cowboy()

local keymap = vim.keymap
local opts = { noremap = true, silent = true }

keymap.set("n", "x", '"_x')

-- Select all
keymap.set("n", "<A-a>", "gg<S-v>G")

-- Save with root permission (not working for now)
vim.api.nvim_create_user_command("W", "w !sudo tee > /dev/null %", {})

-- Disable continuations
keymap.set("n", "<Leader>o", "o<Esc>^Da", opts)
keymap.set("n", "<Leader>O", "O<Esc>^Da", opts)

-- Resize window
keymap.set("n", "<C-w><left>", "<C-w><")
keymap.set("n", "<C-w><right>", "<C-w>>")
keymap.set("n", "<C-w><up>", "<C-w>+")
keymap.set("n", "<C-w><down>", "<C-w>-")

-- Diagnostics
keymap.set("n", "<C-l>", function()
  vim.diagnostic.goto_next()
end, opts)

keymap.set("n", "<leader>r", function()
  require("zmm76933.hsl").replaceHexWithHSL()
end)

keymap.set("n", "<leader>i", function()
  require("zmm76933.lsp").toggleInlayHints()
end)
