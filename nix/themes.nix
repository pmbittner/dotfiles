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
    { name = "one-light";         label = "One Light";         scheme = "one-light";         polarity = "light"; }
    { name = "one-dark";          label = "One Dark";          scheme = "onedark";           polarity = "dark"; }
    { name = "tokyo-night-storm"; label = "Tokyo Night Storm"; scheme = "tokyo-night-storm"; polarity = "dark"; }
    { name = "catppuccin-latte";  label = "Catppuccin Latte";  scheme = "catppuccin-latte";  polarity = "light"; }
  ];
}
