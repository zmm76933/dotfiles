require('base')
require('highlights')
require('maps')
require('plugins')

local has = function(x)
  return vim.fn.has(x) == 1
end
local is_mac = has "macunix"
local is_win = has "win32"
local is_wsl = (function()
  local output = vim.fn.systemlist "uname -r"
  return not not string.find(output[1] or "", "WSL")
end)()
local is_linux = not is_wsl and not is_mac


if is_mac then
  require('macos')
end
if is_win then
  require('windows')
end
if is_wsl then
  require('wsl')
end
