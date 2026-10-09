local HOME   = os.getenv("HOME")
local USRBIN = HOME .. "/bin"

local mod = "SUPER"


local terminal = "kitty"
local ranger   = "kitty --hold --session launch-ranger.kitty"
local explorer = "thunar"
local browser  = "firefox"
local emacs    = "emacsclient -c -a 'emacs'"
local launcher = "rofi -show drun"

-- Values that the NixOS config also knows (monitors, keyboard, theme colors).
-- On NixOS, home-manager generates nix/generated.lua next to this file from
-- the Nix config (see nix/modules/home/hyprland.nix). On machines without it,
-- the defaults below are used. Change the values in Nix, not here; keep the
-- defaults in sync only if this file is used on other machines.
local defaults = {
    monitors = {
        main = "DP-4",
        left = "DP-2",
    },
    keyboard = {
        layout  = "de",
        variant = "",
        options = "caps:escape",
    },
    colors = { -- One Light
        border_active   = "rgba(4078f2ff)",
        border_inactive = "rgba(a0a1a7ff)",
        shadow          = 0x40383a42,
    },
}

local found, generated = pcall(require, "nix.generated")
if not found or type(generated) ~= "table" then
    generated = {}
end

-- Use the generated value of a section if present, otherwise the default.
local function setting(section)
    return setmetatable(generated[section] or {}, { __index = defaults[section] })
end
local monitors = setting("monitors")
local keyboard = setting("keyboard")
local colors   = setting("colors")

local MAIN_MONITOR = monitors.main
local LEFT_MONITOR = monitors.left

local WALLPAPER_HOME = HOME .. "/Media/Wallpaper/"
hl.env("WALLPAPER_HOME", WALLPAPER_HOME) -- used by $USRBIN/wallpaper.sh

hl.config({
    general = {
        gaps_in  = 5,
        gaps_out = 20,

        border_size = 2,

        col = {
            active_border   = colors.border_active,
            inactive_border = colors.border_inactive,
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
            color        = colors.shadow,
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
    kb_layout  = keyboard.layout,
    kb_variant = keyboard.variant,
    kb_model   = "",
    kb_options = keyboard.options,
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
for i = 2, 5 do
    hl.workspace_rule({
        workspace  = tostring(i),
        monitor    = MAIN_MONITOR,
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
    workspace = "name:F silent"
})
-- Set opacity to 1.0 active, 0.85 inactive and 0.8 fullscreen for kitty
hl.window_rule({
    match   = { class = "kitty" },
    opacity = "1.0 override 0.85 override 0.8 override",
})

-- Float and center the settings windows opened from waybar and the wifi menu:
-- blueman (bluetooth), pavucontrol (sound), nm-connection-editor (wifi).
-- On NixOS blueman's class is the wrapper name ".blueman-manager-wrapped";
-- check unknown classes with `hyprctl clients`.
hl.window_rule({
    match  = { class = [[^(\.blueman-manager-wrapped|blueman-manager|org\.pulseaudio\.pavucontrol|pavucontrol|nm-connection-editor)$]] },
    float  = true,
    center = true,
})

-- Ignore windows requesting to get maximized. In particular, kitty does this on
-- every start. The reason is that hyprland tells every window to be maximized
-- so that they are rendered without title bars as a trick. Kitty remembers that
-- it was maximized the last time it was closed, and when it is started again,
-- it asks to become maximized. This rule ignores that request.
hl.window_rule({
    name = "suppress-maximize-events",
    match = { class = ".*" },
    suppress_event = "maximize",
})


-- Autostart
hl.on("hyprland.start", function ()
    hl.exec_cmd("waybar")
    hl.exec_cmd(USRBIN .. "/reset-dynamic-emacs-args.sh")
    hl.exec_cmd("pgrep emacs > /dev/null || emacs --daemon")
    hl.exec_cmd("awww-daemon")
    hl.exec_cmd(USRBIN .. "/theme.sh restore") -- last chosen theme
end)

-- Window/Session actions
hl.bind(mod .. " + q", hl.dsp.window.close())
hl.bind(mod .. " + ESCAPE", hl.dsp.exec_cmd(USRBIN .. "/wlogout-once.sh"))

-- Next desktop
hl.bind(mod .. " + w", hl.dsp.exec_cmd(USRBIN .. "/wallpaper.sh"))

-- Application shortcuts
hl.bind(mod .. " + SHIFT + r", hl.dsp.exec_cmd("hyprctl reload"))
hl.bind(mod .. " + e", hl.dsp.exec_cmd(emacs))
hl.bind(mod .. " + t", hl.dsp.exec_cmd(terminal))
hl.bind(mod .. " + r", hl.dsp.exec_cmd(ranger))
hl.bind(mod .. " + d", hl.dsp.exec_cmd(explorer))
hl.bind(mod .. " + f", hl.dsp.exec_cmd(browser))
hl.bind(mod .. " + SPACE", hl.dsp.exec_cmd(launcher))
hl.bind(mod .. " + n", hl.dsp.exec_cmd("networkmanager_dmenu")) -- wifi menu

-- Navigation follows vim keys. The modifiers stand for:
--   mod                 workspaces (h/l) and windows in stack order (j/k)
--   mod + CTRL          focus a window in a direction
--   mod + ALT           resize the active window
--   mod + SHIFT         move the active window to another workspace
--   mod + SHIFT + CTRL  move the active window in a direction
-- Single letters with mod launch programs (see "Application shortcuts").

-- Switch workspaces
hl.bind(mod .. " + h", hl.dsp.focus({workspace = "e-1"}))
hl.bind(mod .. " + l", hl.dsp.focus({workspace = "e+1"}))

-- Cycle through the windows of the current workspace (like xmonad)
hl.bind(mod .. " + j", hl.dsp.window.cycle_next({next = true}))
hl.bind(mod .. " + k", hl.dsp.window.cycle_next({next = false}))

-- Jump to / move the active window to workspaces 1-5
for i = 1, 5 do
    hl.bind(mod .. " + " .. i,         hl.dsp.focus({workspace = i}))
    hl.bind(mod .. " + SHIFT + " .. i, hl.dsp.window.move({workspace = i}))
end

-- Move window focus
hl.bind(mod .. " + CTRL + h", hl.dsp.focus({direction = "left"}))
hl.bind(mod .. " + CTRL + j", hl.dsp.focus({direction = "down"}))
hl.bind(mod .. " + CTRL + k", hl.dsp.focus({direction = "up"}))
hl.bind(mod .. " + CTRL + l", hl.dsp.focus({direction = "right"}))

-- Resize the active window (repeats while held)
local RESIZE_STEP = 30
hl.bind(mod .. " + ALT + h", hl.dsp.window.resize({x = -RESIZE_STEP, y = 0, relative = true}), {repeating = true})
hl.bind(mod .. " + ALT + j", hl.dsp.window.resize({x = 0, y = RESIZE_STEP,  relative = true}), {repeating = true})
hl.bind(mod .. " + ALT + k", hl.dsp.window.resize({x = 0, y = -RESIZE_STEP, relative = true}), {repeating = true})
hl.bind(mod .. " + ALT + l", hl.dsp.window.resize({x = RESIZE_STEP, y = 0,  relative = true}), {repeating = true})

-- Move the active window to a relative workspace
hl.bind(mod .. " + SHIFT + h", hl.dsp.window.move({workspace = "e-1"}))
hl.bind(mod .. " + SHIFT + l", hl.dsp.window.move({workspace = "e+1"}))

-- Move the active window around the current workspace
hl.bind(mod .. " + SHIFT + CTRL + h", hl.dsp.window.move({direction = "left"}))
hl.bind(mod .. " + SHIFT + CTRL + j", hl.dsp.window.move({direction = "down"}))
hl.bind(mod .. " + SHIFT + CTRL + k", hl.dsp.window.move({direction = "up"}))
hl.bind(mod .. " + SHIFT + CTRL + l", hl.dsp.window.move({direction = "right"}))

-- Hold mod and drag with the left mouse button to move a window.
-- A tiled window becomes floating while it is dragged.
hl.bind(mod .. " + mouse:272", hl.dsp.window.drag(), {mouse = true})
