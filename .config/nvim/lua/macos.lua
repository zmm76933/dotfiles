vim.opt.clipboard:append { 'unnamedplus' }

-- Improve Vim/Neovim experience with input methods
require('im_select').setup {
  -- IM will be set to `default_im_select` in `normal` mode(`EnterVim` or `InsertLeave`)
  -- For Windows, default: "1003", aka: English US Keyboard
  -- You can use `im-select` in cli to get the IM name of you preferred
  default_im_select = 'jp.sourceforge.inputmethod.aquaskk.Ascii',

  -- Set to 1 if you don't want restore IM status when `InsertEnter`
  disable_auto_restore = 1,
}
