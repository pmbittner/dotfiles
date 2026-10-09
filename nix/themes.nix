# The desktop themes that can be switched at runtime (see docs/STYLE.md).
#
# Each theme is a base16 scheme. `scheme` is either the name of a scheme from
# the base16-schemes package, i.e. its file name without `.yaml` (all names:
# https://github.com/tinted-theming/schemes, base16/ directory), or a path to
# an own scheme file, e.g. `scheme = ./themes/mine.yaml;`.
# `polarity` decides GTK's light/dark mode and the icon variant.
#
# The order of `themes` is the order in the menu and when cycling. `default`
# is active after a rebuild until another theme is chosen.
{
  default = "one-light";

  themes = [
    { name = "one-light";               label = "One Light";               scheme = "one-light";               polarity = "light"; }
    { name = "one-dark";                label = "One Dark";                scheme = "onedark";                 polarity = "dark"; }
    { name = "one-ocean";               label = "One Ocean";               scheme = "da-one-ocean";            polarity = "dark"; }
    { name = "tokyo-night-light";       label = "Tokyo Night Light";       scheme = "tokyo-night-light";       polarity = "light"; }
    { name = "tokyo-night-storm";       label = "Tokyo Night Storm";       scheme = "tokyo-night-storm";       polarity = "dark"; }
    { name = "tokyo-night-moon";        label = "Tokyo Night Moon";        scheme = "tokyo-night-moon";        polarity = "dark"; }
    { name = "catppuccin-latte";        label = "Catppuccin Latte";        scheme = "catppuccin-latte";        polarity = "light"; }
    { name = "catppuccin-frappe";       label = "Catppuccin Frappe";       scheme = "catppuccin-frappe";       polarity = "dark"; }
    { name = "catppuccin-macchiato";    label = "Catppuccin Macchiato";    scheme = "catppuccin-macchiato";    polarity = "dark"; }
    { name = "catppuccin-mocha";        label = "Catppuccin Mocha";        scheme = "catppuccin-mocha";        polarity = "dark"; }
    { name = "everforest-dark-soft";    label = "Everforest Dark Soft";    scheme = "everforest-dark-soft";    polarity = "dark"; }
    { name = "tomorrow-night-eighties"; label = "Tomorrow Night Eighties"; scheme = "tomorrow-night-eighties"; polarity = "dark"; }
  ];
}
