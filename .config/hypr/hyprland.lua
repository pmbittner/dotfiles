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
    output = "DP-2",
    mode = "1920x1080@144.00Hz",
    position = "0x0",
    scale = 1,
})
hl.monitor({
    output = "DP-4",
    mode = "1920x1080@144.00Hz",
    position = "auto-center-left",
    scale = 1,
})

-- Main modifier
local mod = "SUPER"

-- Assign apps
local terminal = "kitty"
local explorer = "kitty --hold --session launch-ranger.kitty"
local browser  = "firefox"
local emacs    = "emacsclient -c -a 'emacs'"

-- Window/Session actions
hl.bind(mod .." + q", hl.dsp.window.close(hl.get_active_window))

-- Application shortcuts
hl.bind(mod .." + SHIFT + r", hl.dsp.exec_cmd("hyprctl reload"))
hl.bind(mod .." + e", hl.dsp.exec_cmd(emacs))
hl.bind(mod .." + t", hl.dsp.exec_cmd(terminal))
hl.bind(mod .." + d", hl.dsp.exec_cmd(explorer))
hl.bind(mod .." + f", hl.dsp.exec_cmd(browser))

-- Switch workspaces
-- hl.bind(mod .." + h", hl.work
--hl.bind(mod .." + l", workspace, r+1
--hl.bind(mod .." + k", workspace, 1
--hl.bind(mod .." + j", workspace, empty
--
---- Move/Change window focus
--hl.bind = mod+Ctrl, h, movefocus, l
--hl.bind = mod+Ctrl, l, movefocus, r
--hl.bind = mod+Ctrl, k, movefocus, u
--hl.bind = mod+Ctrl, j, movefocus, d
--
---- Resize windows
--hl.binde = mod+Shift, Right, resizeactive, 30 0
--hl.binde = mod+Shift, Left, resizeactive, -30 0
--hl.binde = mod+Shift, Up, resizeactive, 0 -30
--hl.binde = mod+Shift, Down, resizeactive, 0 30
--
---- Move focused window to a workspace
--hl.bind = mod+Shift, 1, movetoworkspace, 1
--hl.bind = mod+Shift, 2, movetoworkspace, 2
--hl.bind = mod+Shift, 3, movetoworkspace, 3
--hl.bind = mod+Shift, 4, movetoworkspace, 4
--hl.bind = mod+Shift, 5, movetoworkspace, 5
--hl.bind = mod+Shift, 6, movetoworkspace, 6
--hl.bind = mod+Shift, 7, movetoworkspace, 7
--hl.bind = mod+Shift, 8, movetoworkspace, 8
--hl.bind = mod+Shift, 9, movetoworkspace, 9
--hl.bind = mod+Shift, 0, movetoworkspace, 10
--
---- Move focused window to a relative workspace
--hl.bind = mod+Shift, h, movetoworkspace, r-1
--hl.bind = mod+Shift, l, movetoworkspace, r+1
--hl.bind = mod+Shift, k, movetoworkspace, 0
--hl.bind = mod+Shift, j, movetoworkspace, empty
--
---- Move focused window around the current workspace
--hl.bind = mod+Shift+Ctrl, H, movewindow, l
--hl.bind = mod+Shift+Ctrl, L, movewindow, r
--hl.bind = mod+Shift+Ctrl, K, movewindow, u
--hl.bind = mod+Shift+Ctrl, J, movewindow, d
