local discipline = require("zmm76933.discipline")

discipline.cowboy()

local keymap = vim.keymap
local opts = { noremap = true, silent = true }

keymap.set("n", "x", '"_x')

-- Select all
keymap.set("n", "<A-a>", "gg<S-v>G")

-- Save with root permission (not working for now)
vim.api.nvim_create_user_command("W", "w !sudo tee > /dev/null %", {})

-- Tab
keymap.set("n", "\\te", ":tabedit<Space>")
keymap.set("n", "\\tn", ":tabnew<CR>")
keymap.set("n", "\\tc", ":tabclose<CR>")

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
	require("zmm76933.utils").replaceHexWithHSL()
end)
