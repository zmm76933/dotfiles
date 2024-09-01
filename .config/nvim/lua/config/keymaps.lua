local discipline = require("zmm76933.discipline")

discipline.cowboy()

local keymap = vim.keymap
local opts = { noremap = true, silent = true }

-- Emacs keybindings
keymap.set("i", "<C-f>", "<right>")
keymap.set("i", "<C-b>", "<left>")

-- Do things without affecting the registers
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
keymap.set("n", "<leader>r", function()
  require("zmm76933.hsl").replaceHexWithHSL()
end)

keymap.set("n", "<leader>i", function()
  require("zmm76933.lsp").toggleInlayHints()
end)

-- Unset keymap
keymap.del("i", "<A-j>")
keymap.del("i", "<A-k>")
keymap.del("n", "<A-j>")
keymap.del("n", "<A-k>")
keymap.del("v", "<A-j>")
keymap.del("v", "<A-k>")
keymap.del("n", "<C-h>")
keymap.del("n", "<C-j>")
keymap.del("n", "<C-k>")
keymap.del("n", "<C-l>")

-- Unset keymap in terminal
vim.cmd([[
  tunmap <C-h>
  tunmap <C-j>
  tunmap <C-k>
  tunmap <C-l>
]])

-- Add border in terminal
local Util = require("lazyvim.util")
local map = vim.keymap.set
local lazyterm = function()
  Util.terminal(nil, { cwd = Util.root(), border = "rounded" })
end
map("n", "<c-/>", lazyterm, { desc = "Terminal (Root Dir)" })
map("n", "<c-_>", lazyterm, { desc = "which_key_ignore" })
