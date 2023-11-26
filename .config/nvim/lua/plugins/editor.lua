return {
	{
		enabled = false,
		"folke/flash.nvim",
		---@type Flash.Config
		opts = {
			search = {
				forward = true,
				multi_window = false,
				wrap = false,
				incremental = true,
			},
		},
	},

	{
		"echasnovski/mini.hipatterns",
		event = "BufReadPre",
		opts = {
			highlighters = {
				hsl_color = {
					pattern = "hsl%(%d+,? %d+,? %d+%)",
					group = function(_, match)
						local utils = require("craftzdog.utils")
						local h, s, l = match:match("hsl%((%d+),? (%d+),? (%d+)%)")
						h, s, l = tonumber(h), tonumber(s), tonumber(l)
						local hex_color = utils.hslToHex(h, s, l)
						return MiniHipatterns.compute_hex_color_group(hex_color, "bg")
					end,
				},
			},
		},
	},

	{
		"dinhhuy258/git.nvim",
		event = "BufReadPre",
		opts = {
			keymaps = {
				-- Open blame window
				blame = "<Leader>gb",
				-- Open file/folder in git repository
				browse = "<Leader>go",
			},
		},
	},

	{
		"telescope.nvim",
		dependencies = {
			{
				"nvim-telescope/telescope-fzf-native.nvim",
				build = "make",
			},
			"nvim-telescope/telescope-file-browser.nvim",
		},
		keys = {
			{
				"<leader>fP",
				function()
					require("telescope.builtin").find_files({
						cwd = require("lazy.core.config").options.root,
					})
				end,
				desc = "Find Plugin File",
			},
			{
				"<localleader>f",
				function()
					local builtin = require("telescope.builtin")
					builtin.find_files({
						no_ignore = false,
						hidden = true,
					})
				end,
				desc = "Lists files in your current working directory, respects .gitignore",
			},
			{
				"<localleader>g",
				function()
					local builtin = require("telescope.builtin")
					builtin.live_grep()
				end,
				desc = "Search for a string in your current working directory and get results live as you type, respects .gitignore",
			},
			{
				"<localleader>b",
				function()
					local builtin = require("telescope.builtin")
					builtin.buffers()
				end,
				desc = "Lists open buffers",
			},
			{
				"<localleader>t",
				function()
					local builtin = require("telescope.builtin")
					builtin.help_tags()
				end,
				desc = "Lists available help tags and opens a new window with the relevant help info on <cr>",
			},
			{
				"<localleader>;",
				function()
					local builtin = require("telescope.builtin")
					builtin.resume()
				end,
				desc = "Resume the previous telescope picker",
			},
			{
				"<localleader>e",
				function()
					local builtin = require("telescope.builtin")
					builtin.diagnostics()
				end,
				desc = "Lists Diagnostics for all open buffers or a specific buffer",
			},
			{
				"<localleader>s",
				function()
					local builtin = require("telescope.builtin")
					builtin.treesitter()
				end,
				desc = "Lists Function names, variables, from Treesitter",
			},
			{
				"<localleader>l",
				function()
					local telescope = require("telescope")

					local function telescope_buffer_dir()
						return vim.fn.expand("%:p:h")
					end

					telescope.extensions.file_browser.file_browser({
						path = "%:p:h",
						cwd = telescope_buffer_dir(),
						respect_gitignore = false,
						hidden = true,
						grouped = true,
						previewer = false,
						initial_mode = "normal",
						layout_config = { height = 40 },
					})
				end,
				desc = "Open File Browser with the path of the current buffer",
			},
		},
		config = function(_, opts)
			local telescope = require("telescope")
			local actions = require("telescope.actions")
			local fb_actions = require("telescope").extensions.file_browser.actions

			opts.defaults = vim.tbl_deep_extend("force", opts.defaults, {
				wrap_results = true,
				layout_strategy = "horizontal",
				layout_config = { prompt_position = "top" },
				sorting_strategy = "ascending",
				winblend = 0,
				mappings = {
					n = {},
				},
			})
			opts.pickers = {
				diagnostics = {
					theme = "ivy",
					initial_mode = "normal",
					layout_config = {
						preview_cutoff = 9999,
					},
				},
			}
			opts.extensions = {
				file_browser = {
					theme = "dropdown",
					-- disables netrw and use telescope-file-browser in its place
					hijack_netrw = true,
					mappings = {
						-- your custom insert mode mappings
						["i"] = {
							["<C-w>"] = function()
								vim.cmd("normal vbd")
							end,
						},
						["n"] = {
							-- your custom normal mode mappings
							["N"] = fb_actions.create,
							["h"] = fb_actions.goto_parent_dir,
							["/"] = function()
								vim.cmd("startinsert")
							end,
							["<C-u>"] = function(prompt_bufnr)
								for i = 1, 10 do
									actions.move_selection_previous(prompt_bufnr)
								end
							end,
							["<C-d>"] = function(prompt_bufnr)
								for i = 1, 10 do
									actions.move_selection_next(prompt_bufnr)
								end
							end,
							["<PageUp>"] = actions.preview_scrolling_up,
							["<PageDown>"] = actions.preview_scrolling_down,
						},
					},
				},
			}
			telescope.setup(opts)
			require("telescope").load_extension("fzf")
			require("telescope").load_extension("file_browser")
		end,
	},

	{
		"windwp/nvim-autopairs",
		config = function()
			require("nvim-autopairs").setup({ map_c_h = true })
		end,
	},

	{
		enabled = false,
		"echasnovski/mini.pairs",
	},

	{
		"keaising/im-select.nvim",
		config = function()
			require("im_select").setup({
				-- For Windows/WSL, default: "1033", aka: English US Keyboard
				-- For macOS, default: "com.apple.keylayout.ABC", aka: US
				-- For Linux, default:
				--               "keyboard-us" for Fcitx5
				--               "1" for Fcitx
				--               "xkb:us::eng" for ibus
				-- You can use `im-select` or `fcitx5-remote -n` to get the IM's name
				default_im_select = "jp.sourceforge.inputmethod.aquaskk.Ascii",

				-- Can be binary's name or binary's full path,
				-- e.g. 'im-select' or '/usr/local/bin/im-select'
				-- For Windows/WSL, default: "im-select.exe"
				-- For macOS, default: "im-select"
				-- For Linux, default: "fcitx5-remote" or "fcitx-remote" or "ibus"
				default_command = "im-select",

				-- Restore the default input method state when the following events are triggered
				set_default_events = { "VimEnter", "FocusGained", "InsertLeave", "CmdlineLeave" },

				-- Restore the previous used input method state when the following events
				-- are triggered, if you don't want to restore previous used im in Insert mode,
				-- e.g. deprecated `disable_auto_restore = 1`, just let it empty
				-- as `set_previous_events = {}`
				set_previous_events = {},

				-- Show notification about how to install executable binary when binary is missing
				keep_quiet_on_no_binary = false,

				-- Async run `default_command` to switch IM or not
				async_switch_im = true,
			})
		end,
	},
}
