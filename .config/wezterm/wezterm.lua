-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This will hold the configuration.
local config = wezterm.config_builder()

-- colors
config.color_scheme = "nord"
-- font
config.font = wezterm.font("Hack Nerd Font Mono")
config.font_size = 12.0
-- This is where you actually apply your config choices

-- For example, changing the color scheme:
config.color_scheme = 'Tokyo Night'

config.hide_tab_bar_if_only_one_tab = true

-- IME settings
config.use_ime = true
config.macos_forward_to_ime_modifier_mask = 'SHIFT|CTRL'

-- and finally, return the configuration to wezterm
return config
