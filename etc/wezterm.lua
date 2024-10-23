
-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This will hold the configuration.
local config = wezterm.config_builder()

-- This is where you actually apply your config choices

-- For example, changing the color scheme:
config.color_scheme = 'Github'
config.font = wezterm.font 'JetBrains Mono'

-- M-. cycles through last arg history
config.send_composed_key_when_right_alt_is_pressed = false


config.keys = {
  {
    key = 'r',
    mods = 'CMD|SHIFT',
    action = wezterm.action.ReloadConfiguration,
  },

-- iterm-like splits
{
  key = 'd',
  mods = 'CMD',
  action = wezterm.action.SplitPane { direction = "Right" }
},


{
  key = 'd',
  mods = 'CMD|SHIFT',
  action = wezterm.action.SplitPane { direction = "Down" }
},


-- ace-window-like pane selection
  {
    key = 'o',
    mods = 'ALT',
    action = wezterm.action.PaneSelect {
      alphabet = '1234567890',
    },
  }

}

-- and finally, return the configuration to wezterm
return config
