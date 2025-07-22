# Hex logo

# Loading font
showtext::showtext_auto()
showtext::font_add("Consolas", regular = "consola.ttf")

# Logo with name
hexSticker::sticker(
  package = "KiwiFlow",
  subplot = "resources/kiwi.png",
  h_fill = "#E8CB98",
  h_color = "#36367B",
  s_width = 0.8,
  s_height = 0.8,
  s_y = 1.15,
  s_x = 1,
  p_color = "black",
  p_y = 0.5,
  p_family = "Consolas", p_size = 5.5,
  filename = "app/static/logo_name.svg"
)

# Logo without name
hexSticker::sticker(
  package = "KiwiFlow",
  subplot = "resources/kiwi.png",
  h_fill = "#E8CB98",
  h_color = "#36367B",
  s_width = 1,
  s_height = 1,
  s_y = 1,
  s_x = 1,
  p_color = "transparent",
  p_y = 0.5,
  p_family = "Consolas", p_size = 5.5,
  filename = "app/static/logo_noname.svg"
)
