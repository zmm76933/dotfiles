vim.opt.clipboard:prepend { 'unnamed', 'unnamedplus' }

-- Improve Vim/Neovim experience with input methods
vim.api.nvim_set_var('im_select_command', '/mnt/c/ProgramData/zenhan/zenhan.exe')
vim.api.nvim_set_var('im_select_default', 0)

-- Use windows clipboard to copy and to paste
vim.api.nvim_set_var('clipboard', {
  name = 'win32yank-wsl',
  copy = {
    ['+'] = '/mnt/c/ProgramData/win32yank/win32yank.exe -i --crlf',
    ['*'] = '/mnt/c/ProgramData/win32yank/win32yank.exe -i --crlf'
  },
  paste = {
    ['+'] = '/mnt/c/ProgramData/win32yank/win32yank.exe -o --lf',
    ['*'] = '/mnt/c/ProgramData/win32yank/win32yank.exe -o --lf'
  },
  cache_enabled = 0,
})
