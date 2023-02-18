vim.opt.clipboard:prepend { 'unnamed', 'unnamedplus' }

-- Improve Vim/Neovim experience with input methods
require('im_select').setup {

  -- IM will be set to `default_im_select` in `normal` mode(`EnterVim` or `InsertLeave`)
  -- For Windows/WSL, default: "1033", aka: English US Keyboard
  -- For macOS, default: "com.apple.keylayout.ABC", aka: US
  -- You can use `im-select` in cli to get the IM's name you preferred
  default_im_select = '0',

  -- Set to 1 if you don't want restore IM status when `InsertEnter`
  disable_auto_restore = 0,

  -- Can be binary's name or binary's full path,
  -- e.g. 'im-select' or '/usr/local/bin/im-select'
  -- For Windows/WSL, default: "im-select.exe"
  -- For macOS, default: "im-select"
  default_command = '/mnt/c/ProgramData/zenhan/zenhan.exe'
}

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
