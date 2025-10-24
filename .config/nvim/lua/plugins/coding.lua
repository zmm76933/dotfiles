return {
  -- Incremental rename
  {
    "smjonas/inc-rename.nvim",
    cmd = "IncRename",
    config = true,
  },

  -- Go forward/backward with square brackets
  {
    "nvim-mini/mini.bracketed",
    event = "BufReadPost",
    config = function()
      local bracketed = require("mini.bracketed")
      bracketed.setup({
        file = { suffix = "" },
        window = { suffix = "" },
        quickfix = { suffix = "" },
        yank = { suffix = "" },
        treesitter = { suffix = "n" },
      })
    end,
  },

  -- Better increase/descrease
  {
    "monaqa/dial.nvim",
    -- stylua: ignore
    config = function()
      local augend = require("dial.augend")
      require("dial.config").augends:register_group({
        default = {
          augend.integer.alias.decimal,
          augend.integer.alias.hex,
          augend.date.alias["%Y/%m/%d"],
          augend.constant.alias.bool,
          augend.semver.alias.semver,
          augend.constant.new({ elements = { "let", "const" } }),
        },
      })
    end,
  },

  {
    "simrat39/symbols-outline.nvim",
    keys = { { "<leader>cs", "<cmd>SymbolsOutline<cr>", desc = "Symbols Outline" } },
    cmd = "SymbolsOutline",
    opts = {
      position = "right",
    },
  },

  {
      "kylechui/nvim-surround",
      config = function()
          require("nvim-surround").setup({
              -- Configuration here, or leave empty to use defaults
          })
      end
  },

  {
    "stevearc/conform.nvim",
    optional = true,
    opts = {
      formatters_by_ft = {
        ruby = { formatter },
        eruby = { "erb-format" },
      },
    },
  },
}
