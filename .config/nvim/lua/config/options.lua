vim.g.mapleader = " "
vim.g.maplocalleader = "\\"
vim.g.autoformat = false

vim.opt.encoding = "utf-8"
vim.opt.fileencoding = "utf-8"

vim.opt.number = true

vim.opt.title = true
vim.opt.autoindent = true
vim.opt.smartindent = true
vim.opt.hlsearch = true
vim.opt.backup = false
vim.opt.showcmd = true
vim.opt.cmdheight = 1
vim.opt.laststatus = 3
vim.opt.expandtab = true
vim.opt.scrolloff = 10
vim.opt.shell = "fish"
vim.opt.backupskip = { "/tmp/*", "/private/tmp/*" }
vim.opt.inccommand = "split"
vim.opt.ignorecase = true -- Case insensitive searching UNLESS /C or capital in search
vim.opt.smarttab = true
vim.opt.breakindent = true
vim.opt.shiftwidth = 2
vim.opt.tabstop = 2
vim.opt.wrap = false -- No Wrap lines
vim.opt.backspace = { "start", "eol", "indent" }
vim.opt.path:append({ "**" }) -- Finding files - Search down into subfolders
vim.opt.wildignore:append({ "*/node_modules/*" })
vim.opt.splitbelow = true -- Put new windows below current
vim.opt.splitright = true -- Put new windows right of current
vim.opt.splitkeep = "cursor"
vim.opt.relativenumber = false
vim.opt.clipboard:append({ "unnamedplus" })
vim.opt.iskeyword:remove("_")

-- Undercurl
vim.cmd([[let &t_Cs = "\e[4:3m"]])
vim.cmd([[let &t_Ce = "\e[4:0m"]])

-- Add asterisks in block comments
vim.opt.formatoptions:append({ "r" })

vim.cmd([[au BufNewFile,BufRead *.astro setf astro]])
vim.cmd([[au BufNewFile,BufRead Podfile setf ruby]])

if vim.fn.has("nvim-0.8") == 1 then
  vim.opt.cmdheight = 0
end

-- File types
vim.filetype.add({
  extension = {
    mdx = "mdx",
  },
})

vim.g.lazyvim_prettier_needs_config = true
vim.g.lazyvim_picker = "telescope"
vim.g.lazyvim_cmp = "blink.cmp"

-- LSP Server to use for Python.
-- Set to "basedpyright" to use basedpyright instead of pyright.
vim.g.lazyvim_python_lsp = "pyright"
-- Set to "ruff_lsp" to use the old LSP implementation version.
vim.g.lazyvim_python_ruff = "ruff"
-- Set python3 provider
vim.api.nvim_set_var("python3_host_prog", os.getenv("HOME") .. "/.asdf/shims/python")

-- LSP Server to use for Ruby.
-- Set to "solargraph" to use solargraph instead of ruby_lsp.
vim.g.lazyvim_ruby_lsp = "ruby_lsp"
vim.g.lazyvim_ruby_formatter = "rubocop"

--  OSC52 over ssh
vim.opt.clipboard:append("unnamedplus")
local function paste()
  return {
    vim.fn.split(vim.fn.getreg(""), "\n"),
    vim.fn.getregtype(""),
  }
end

if os.getenv("SSH_TTY") == nil then
  if vim.fn.has("wsl") == 1 then
    vim.api.nvim_set_var("clipboard", {
      name = "win32yank-wsl",
      copy = {
          ["+"] = "/mnt/c/ProgramData/win32yank/win32yank.exe -i --crlf",
          ["*"] = "/mnt/c/ProgramData/win32yank/win32yank.exe -i --crlf",
      },
      paste = {
          ["+"] = "/mnt/c/ProgramData/win32yank/win32yank.exe -o --lf",
          ["*"] = "/mnt/c/ProgramData/win32yank/win32yank.exe -o --lf",
      },
      cache_enabled = 0,
    })
  end
else
  vim.g.clipboard = {
    name = "OSC 52",
    copy = {
      ["+"] = require('vim.ui.clipboard.osc52').copy('+'),
      ["*"] = require('vim.ui.clipboard.osc52').copy('*'),
    },
    paste = {
      ["+"] = paste,
      ["*"] = paste,
    },
  }
end
