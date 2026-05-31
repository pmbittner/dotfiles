local HOME   = os.getenv("HOME")
local USRBIN = HOME .. "/bin"

local mod = "SUPER"

local MAIN_MONITOR = "DP-2"
local LEFT_MONITOR = "DP-4"

local terminal = "kitty"
local ranger   = "kitty --hold --session launch-ranger.kitty"
local explorer = "thunar"
local browser  = "firefox"
local emacs    = "emacsclient -c -a 'emacs'"

local WALLPAPER_HOME = HOME .. "/Media/Wallpaper/"
hl.env("WALLPAPER_HOME", WALLPAPER_HOME) -- used by $USRBIN/wallpaper.sh

hl.config({
    general = {
        gaps_in  = 5,
        gaps_out = 20,

        border_size = 2,

        col = {
            active_border   = { colors = {"rgba(33ccffee)", "rgba(00ff99ee)"}, angle = 45 },
            inactive_border = "rgba(595959aa)",
        },

        -- Set to true to enable resizing windows by clicking and dragging on borders and gaps
        resize_on_border = true,

        -- Please see https://wiki.hypr.land/Configuring/Advanced-and-Cool/Tearing/ before you turn this on
        allow_tearing = false,

        layout = "dwindle",
    },

    xwayland = {
        enabled = true
    },

    misc = {
        animate_manual_resizes = true,
        disable_autoreload = true
    },

    quirks = {
        prefer_hdr = 1
    },

    decoration = {
        rounding       = 10,
        rounding_power = 2,

        -- Change transparency of focused and unfocused windows
        active_opacity   = 1.0,
        inactive_opacity = 1.0,

        shadow = {
            enabled      = true,
            range        = 4,
            render_power = 3,
            color        = 0xee1a1a1a,
        },

        blur = {
            enabled   = true,
            size      = 3,
            passes    = 1,
            vibrancy  = 0.1696,
        },
    },

    animations = {
        enabled = true,
    },
})

hl.config({input = {
    kb_layout  = "de",
    kb_variant = "",
    kb_model   = "",
    kb_options = "caps:escape",
    kb_rules   = "",

    follow_mouse = 1,

    sensitivity = 0, -- -1.0 - 1.0, 0 means no modification.
    force_no_accel = 1,
    numlock_by_default = true,

    touchpad = {
        natural_scroll = true,
    },
}})

hl.monitor({
    output = MAIN_MONITOR,
    mode = "1920x1080@144.00Hz",
    position = "0x0",
    scale = 1,
})
hl.monitor({
    output = LEFT_MONITOR,
    mode = "1920x1080@144.00Hz",
    position = "auto-center-left",
    scale = 1,
})

-- Workspaces
hl.workspace_rule({
    workspace = "1",
    monitor = MAIN_MONITOR,
    default = true,
    persistent = true,
    layout = "master"
})
for i = 2, 5, 1 do
  hl.workspace_rule({
      workspace = tostring(i),
      monitor = MAIN_MONITOR,
      persistent = true,
  })
end

hl.workspace_rule({
    workspace = "name:F",
    monitor = LEFT_MONITOR,
    default = true,
    persistent = true,
})

-- Window rules
hl.window_rule({
    match = {
        class = "firefox",
    },
    no_blur = true,
    no_dim  = true,
    opaque  = true,
    workspace = "F silent"
})
-- Set opacity to 1.0 active, 0.5 inactive and 0.8 fullscreen for kitty
hl.window_rule({
  match   = { class = "kitty" },
  opacity = "1.0 override 0.85 override 0.8 override",
})

-- Autostart
hl.on("hyprland.start", function ()
  hl.exec_cmd("waybar")
  hl.exec_cmd("sh " .. USRBIN .. "/reset-dynamic-emacs-args.sh")
  hl.exec_cmd("pgrep emacs > /dev/null || emacs --daemon")
  hl.exec_cmd("awww-daemon")
  -- exec-once = blueman-applet # systray app for Bluetooth
  -- exec-once = udiskie --no-automount --smart-tray # front-end that allows to manage removable media
  -- exec-once = nm-applet --indicator # systray app for Network/Wifi
end)

-- Window/Session actions
hl.bind(mod .." + q", hl.dsp.window.close(hl.get_active_window))
hl.bind(mod .." + ESCAPE", hl.dsp.exec_cmd("sh " .. USRBIN .."/wlogout-once.sh"))

-- Next desktop
hl.bind(mod .." + w", hl.dsp.exec_cmd("sh " .. USRBIN .."/wallpaper.sh"))

-- Application shortcuts
hl.bind(mod .." + SHIFT + r", hl.dsp.exec_cmd("hyprctl reload"))
hl.bind(mod .." + e", hl.dsp.exec_cmd(emacs))
hl.bind(mod .." + t", hl.dsp.exec_cmd(terminal))
hl.bind(mod .." + r", hl.dsp.exec_cmd(ranger))
hl.bind(mod .." + d", hl.dsp.exec_cmd(explorer))
hl.bind(mod .." + f", hl.dsp.exec_cmd(browser))

-- Switch workspaces
hl.bind(mod .. " + h", hl.dsp.focus({workspace = "e-1"}))
hl.bind(mod .. " + l", hl.dsp.focus({workspace = "e+1"}))

---- Move/Change window focus
hl.bind(mod .. " + CTRL + h", hl.dsp.focus({direction = "left"}))
hl.bind(mod .. " + CTRL + l", hl.dsp.focus({direction = "right"}))
--
---- Resize windows
--hl.binde = mod+Shift, Right, resizeactive, 30 0
--hl.binde = mod+Shift, Left, resizeactive, -30 0
--hl.binde = mod+Shift, Up, resizeactive, 0 -30
--hl.binde = mod+Shift, Down, resizeactive, 0 30
--
---- Move focused window to a relative workspace
hl.bind(mod .. " + SHIFT + h", hl.dsp.window.move({workspace = "e-1"}))
hl.bind(mod .. " + SHIFT + l", hl.dsp.window.move({workspace = "e+1"}))

---- Move focused window around the current workspace
hl.bind(mod .. " + SHIFT + CTRL + h", hl.dsp.window.move({direction = "left"}))
hl.bind(mod .. " + SHIFT + CTRL + l", hl.dsp.window.move({direction = "right"}))
hl.bind(mod .. " + SHIFT + CTRL + k", hl.dsp.window.move({direction = "up"}))
hl.bind(mod .. " + SHIFT + CTRL + j", hl.dsp.window.move({direction = "down"}))
