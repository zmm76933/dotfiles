require('base')
require('highlights')
require('maps')
require('plugins')

local has = function(x)
  return vim.fn.has(x) == 1
end
local is_mac = has "macunix"
local is_linux = has "unix"
local is_win = has "win32"
local is_wsl = has "wsl"

if is_mac then
  require('macos')
end
if is_linux == 1 then
  require('linux')
end
if is_win then
  require('windows')
end
if is_wsl then
  require('wsl')
end
