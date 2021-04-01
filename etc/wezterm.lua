local wezterm = require 'wezterm';
return {
   color_scheme = "Builtin Solarized Light",
   keys = {
      {key="-", mods="SUPER",
       action=wezterm.action{SplitVertical={domain="CurrentPaneDomain"}}},
      {key="|", mods="SUPER|SHIFT",
       action=wezterm.action{SplitHorizontal={domain="CurrentPaneDomain"}}},
      { key = "Z", mods="SUPER|SHIFT", action="TogglePaneZoomState" },
      { key = "LeftArrow", mods="SUPER",
        action=wezterm.action{ActivatePaneDirection="Left"}},
      { key = "RightArrow", mods="SUPER",
        action=wezterm.action{ActivatePaneDirection="Right"}},
      { key = "UpArrow", mods="SUPER",
        action=wezterm.action{ActivatePaneDirection="Up"}},
      { key = "DownArrow", mods="SUPER",
        action=wezterm.action{ActivatePaneDirection="Down"}},

   }
}
