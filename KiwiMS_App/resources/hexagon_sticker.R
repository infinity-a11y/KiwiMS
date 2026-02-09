# Instructions to make Hex logo
# THIS FILE AND ITS OPEN-SOURCE COMPONENTS ARE NOT A COMPONENT OF THE KiwiMS APP
# It requires hexSticker AND showtext packages which are not compatible with GPL-3 license and therefore not part of the application

# # Loading font
# showtext_auto()
# font_add("Consolas", regular = "consola.ttf")

# # Logo with name
# sticker(
#   package = "KiwiMS",
#   subplot = "resources/kiwi.png",
#   h_fill = "#E8CB98",
#   h_color = "#36367B",
#   s_width = 0.8,
#   s_height = 0.8,
#   s_y = 1.15,
#   s_x = 1,
#   p_color = "black",
#   p_y = 0.5,
#   p_family = "Consolas", p_size = 5.5,
#   filename = "app/static/logo_name.svg"
# )

# # Logo without name
# sticker(
#   package = "KiwiMS",
#   subplot = "resources/kiwi.png",
#   h_fill = "#E8CB98",
#   h_color = "#36367B",
#   s_width = 1,
#   s_height = 1,
#   s_y = 1,
#   s_x = 1,
#   p_color = "transparent",
#   p_y = 0.5,
#   p_family = "Consolas", p_size = 5.5,
#   filename = "app/static/logo_noname.svg"
# )
