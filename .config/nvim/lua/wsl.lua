vim.opt.clipboard:prepend { 'unnamed', 'unnamedplus' }

-- Improve Vim/Neovim experience with input methods
require('im_select').setup {

  -- IM will be set to `default_im_select` in `normal` mode
  -- For Windows/WSL, default: "1033", aka: English US Keyboard
  -- For macOS, default: "com.apple.keylayout.ABC", aka: US
  -- For Linux, default: "keyboard-us"
  -- You can use `im-select` or `fcitx5-remote -n` to get the IM's name you preferred
  default_im_select = '0',

  -- Can be binary's name or binary's full path,
  -- e.g. 'im-select' or '/usr/local/bin/im-select'
  -- For Windows/WSL, default: "im-select.exe"
  -- For macOS, default: "im-select"
  -- For Linux, default: "fcitx5-remote"
  default_command = '/mnt/c/ProgramData/zenhan/zenhan.exe',

  -- Restore the default input method state when the following events are triggered
  set_default_events = { "VimEnter", "FocusGained", "InsertLeave", "CmdlineLeave" },

  -- Restore the previous used input method state when the following events are triggered
  -- if you don't want to restore previous used im in Insert mode,
  -- e.g. deprecated `disable_auto_restore = 1`, just let it empty `set_previous_events = {}`
  set_previous_events = {},

  -- Show notification about how to install executable binary when binary is missing
  keep_quiet_on_no_binary = false
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
