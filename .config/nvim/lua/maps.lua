local keymap = vim.keymap

-- Select all
keymap.set('n', '<A-a>', 'gg<S-v>G')

-- Save with root permission (not working for now)
vim.api.nvim_create_user_command('W', 'w !sudo tee > /dev/null %', {})

-- Tab
keymap.set('n', '\\te', ':tabedit<Space>')
keymap.set('n', '\\tc', ':tabclose<CR>')

-- Resize window
keymap.set('n', '<C-w><left>', '<C-w><')
keymap.set('n', '<C-w><right>', '<C-w>>')
keymap.set('n', '<C-w><up>', '<C-w>+')
keymap.set('n', '<C-w><down>', '<C-w>-')
