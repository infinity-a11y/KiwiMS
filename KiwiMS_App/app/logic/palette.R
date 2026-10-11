# app/logic/palette.R

# Static colour system of the conversion results.
#
# Every colour on a results plot or table comes from here, so a compound, a
# concentration or a sample looks the same in every view and export. Nothing
# is picked by the user any more.
#
# Each palette comes in two variants:
# - "light" is drawn on the light surfaces: the grey plot cards of the app
#   (CIE L* 58-70) and the white background of a light export. Its colours
#   are dark to medium and saturated (L* 28-57), at least 24 CIEDE2000 away
#   from either card grey, also under simulated red-green colour blindness,
#   and at least 3:1 in contrast against white.
# - "dark" is drawn on the #121212 background of a dark export: the same hues
#   with the lightness mirrored, so a colour keeps its identity between the
#   two exports.
#
# Categorical colours are ordered so that every prefix is as distinct as
# possible: the first five stay apart under deuteranopia and protanopia too
# (CIEDE2000 >= 14), the first twelve under normal vision (>= 10). Six light
# accents follow (lighter than the twelve, which sets them apart for colour
# blind eyes too); all eighteen keep CIEDE2000 >= 9. That is about as many
# colours as stay apart at all: past eighteen levels the colours repeat and
# other channels carry the identity - marker symbol (level_tier()), labels on
# the batch heatmap tiles, legend click to hide or isolate a level.
# The accents suit areas and outlined markers; thin lines and samples keep to
# the twelve saturated colours (base_colors).
#
# Greys are kept out of the categorical palette: they mean something
# (controls, unpicked compounds, unbound protein; see status_colors()).

# Dark export background, also the reference the dark variants are built on
#' @export
dark_background <- "#121212"

#' @export
categorical_palette <- list(
  light = c(
    "#2563EB", # blue
    "#EA580C", # orange
    "#BE185D", # raspberry
    "#3730A3", # indigo
    "#991B1B", # maroon
    "#047857", # emerald
    "#A16207", # ochre
    "#6D28D9", # violet
    "#0369A1", # cerulean
    "#A21CAF", # fuchsia
    "#4D7C0F", # olive
    "#92400E", # brown
    "#F285D0", # light accents: pink
    "#FE7B89", # coral
    "#FF9845", # apricot
    "#85C3FF", # sky
    "#00E4D1", # aqua
    "#00ECFF" # cyan
  ),
  dark = c(
    "#4586FF",
    "#EF5C16",
    "#F9598D",
    "#ACB9FF",
    "#FF8C7F",
    "#3C9B78",
    "#BE7D31",
    "#B887FF",
    "#4598D3",
    "#E062ED",
    "#699936",
    "#E2885C",
    # Deep twins of the light accents, the dark export's second round
    "#A43686",
    "#B52F48",
    "#AE4300",
    "#2563C8",
    "#008575",
    "#007E9E"
  )
)

# How many of the categorical colours are the saturated first round, the
# ones that also hold up as thin lines
#' @export
base_colors <- 12L

# Anchor stops of the ordered and continuous scales, interpolated in CIELAB.
# - concentration: violet -> magenta -> red -> orange -> amber. Low to high
#   concentration; plasma-like, without the yellow end that vanishes on white
#   and on the grey cards.
# - time: blue -> teal -> green -> lime, early to late; viridis-like, again
#   without yellow, and starting where the concentration scale does not.
# - sample: the categorical hues in hue order, for results with more samples
#   than categorical colours.
# - binding, correct, unmatched: heatmap fills from "nothing" to "all".
# - count: the peak counts of the deconvolution plate, navy -> violet -> red.
#   Every stop is dark enough for the white count labels (>= 4.5:1), so it
#   is the same in both themes.
ramp_anchors <- list(
  concentration = list(
    light = c("#4A0097", "#90007F", "#C41844", "#E14F00", "#E98000"),
    dark = c("#9164EF", "#DF5EC8", "#FF667E", "#FF8838", "#FFB900")
  ),
  time = list(
    light = c("#1D3FBF", "#0060B0", "#006F80", "#00783F", "#4C8C00"),
    dark = c("#6F86FF", "#3AA6F5", "#20C5C8", "#4FD889", "#C2EA4E")
  ),
  sample = list(
    light = c(
      "#2563EB",
      "#6D28D9",
      "#A21CAF",
      "#BE185D",
      "#EA580C",
      "#A16207",
      "#4D7C0F",
      "#047857",
      "#0369A1"
    ),
    dark = c(
      "#4586FF",
      "#B887FF",
      "#E062ED",
      "#F9598D",
      "#EF5C16",
      "#BE7D31",
      "#699936",
      "#3C9B78",
      "#4598D3"
    )
  ),
  count = list(
    light = c("#101A5C", "#33288E", "#6E2E9C", "#A8336F", "#C9452C"),
    dark = c("#101A5C", "#33288E", "#6E2E9C", "#A8336F", "#C9452C")
  ),
  binding = list(
    light = c("#EEEFF8", "#B9BBE6", "#7A7CCB", "#4A4BA6", "#26266E"),
    dark = c("#22243A", "#383A6E", "#5A5CB0", "#8F91E6", "#D2D3FF")
  ),
  correct = list(
    light = c("#ECF6EE", "#A7D9B2", "#4CAF6A", "#15803D", "#0B4A24"),
    dark = c("#14281B", "#1E5A33", "#2F9152", "#5FCB84", "#BDF2CE")
  ),
  unmatched = list(
    light = c("#FCEDEC", "#F2B1AC", "#E0675F", "#B91C1C", "#6E1414"),
    dark = c("#2E1716", "#6A2420", "#B23A33", "#EB7068", "#FFC9C4")
  )
)

# Fixed meanings
# - muted: everything set aside - compounds not picked in the Overview,
#   groups without a value. Recessive but still visible on the grey cards.
# - control: the 0 concentration, outside the concentration scale.
# - unbound: the unbound share of the protein in the distribution donut.
# - correct / unmatched: the two hit-rate metrics.
# - unassigned: the cross marking spectrum peaks no hit explains. Lighter
#   than the black protein marker, yet clear of both card greys.
# - accent: the app's accent colour.
status_palette <- list(
  light = list(
    muted = "#5B6170",
    control = "#2F3640",
    unbound = "#3B3B42",
    correct = "#15803D",
    unmatched = "#DC2626",
    unassigned = "#454B57",
    accent = "#7777F9"
  ),
  dark = list(
    muted = "#9AA0AD",
    control = "#D4D4D8",
    unbound = "#C9C9D1",
    correct = "#4ADE80",
    unmatched = "#F87171",
    unassigned = "#C4C8D0",
    accent = "#9D9DFF"
  )
)

# Marker symbols of the concentrations, highest first (control apart), and
# the symbols a categorical level switches to past the twelfth colour.
#' @export
concentration_symbols <- c(
  "circle",
  "triangle-up",
  "square",
  "star-triangle-down",
  "diamond",
  "triangle-down",
  "cross",
  "x",
  "hexagram",
  "hourglass"
)

#' @export
control_symbol <- "circle-cross"

# No diamond: in the spectra it marks the protein
tier_symbols <- c(
  "circle",
  "square",
  "triangle-up",
  "triangle-down",
  "pentagon",
  "hexagram",
  "star",
  "hourglass"
)

tier_patterns <- c("", "/", "\\", "x", ".", "-", "|", "+")

tier_dashes <- c("solid", "dash", "dot", "dashdot", "longdash", "longdashdot")

# "dark" for the dark export theme, "light" for everything else
#' @export
palette_variant <- function(theme = "light") {
  if (identical(theme, "dark")) "dark" else "light"
}

#' @export
status_colors <- function(theme = "light") {
  status_palette[[palette_variant(theme)]]
}

# The first n categorical colours, repeating past eighteen
#' @export
categorical_colors <- function(n, theme = "light") {
  pal <- categorical_palette[[palette_variant(theme)]]
  if (n < 1) {
    return(character(0))
  }
  pal[(seq_len(n) - 1L) %% length(pal) + 1L]
}

# Repeat round of each of n categorical levels: 0 for the first eighteen, 1
# for the next eighteen, ... Plots that can, switch symbol, pattern or dash.
#' @export
level_tier <- function(n) {
  (seq_len(n) - 1L) %/% length(categorical_palette$light)
}

#' @export
tier_symbol <- function(tier) {
  tier_symbols[tier %% length(tier_symbols) + 1L]
}

#' @export
tier_pattern <- function(tier) {
  tier_patterns[tier %% length(tier_patterns) + 1L]
}

#' @export
tier_dash <- function(tier) {
  tier_dashes[tier %% length(tier_dashes) + 1L]
}

ramp_function <- function(ramp, theme) {
  anchors <- ramp_anchors[[ramp]][[palette_variant(theme)]]
  if (is.null(anchors)) {
    stop("Unknown colour ramp: ", ramp)
  }
  grDevices::colorRamp(anchors, space = "Lab")
}

# Colours at positions (0 to 1) along a ramp
#' @export
ramp_at <- function(pos, ramp, theme = "light") {
  if (!length(pos)) {
    return(character(0))
  }
  pos <- pmin(pmax(as.numeric(pos), 0), 1)
  rgb <- ramp_function(ramp, theme)(pos)
  grDevices::rgb(rgb[, 1], rgb[, 2], rgb[, 3], maxColorValue = 255)
}

# n colours spread evenly over a ramp; a single one sits in its middle
#' @export
ramp_colors <- function(n, ramp, theme = "light") {
  if (n < 1) {
    return(character(0))
  }
  ramp_at(if (n == 1) 0.5 else seq(0, 1, length.out = n), ramp, theme)
}

# Colours of n samples in their order: the categorical colours while they
# suffice, otherwise spread over the sample ramp
#' @export
sample_colors <- function(n, theme = "light") {
  if (n <= base_colors) {
    categorical_colors(n, theme)
  } else {
    ramp_colors(n, "sample", theme)
  }
}

# Plotly colorscale of a ramp
#' @export
ramp_colorscale <- function(ramp, theme = "light", n = 9) {
  pos <- seq(0, 1, length.out = n)
  cols <- ramp_at(pos, ramp, theme)
  lapply(seq_len(n), function(i) list(pos[i], cols[i]))
}

# Light tint of colours, for table rows read with black text: mixed with
# white, keeping the hue
#' @export
tint <- function(colors, amount = 0.62) {
  if (!length(colors)) {
    return(colors)
  }
  rgb <- grDevices::col2rgb(colors)
  mixed <- rgb + (255 - rgb) * amount
  out <- grDevices::rgb(mixed[1, ], mixed[2, ], mixed[3, ], maxColorValue = 255)
  names(out) <- names(colors)
  out
}

# "rgba()" string of a colour with alpha
#' @export
color_alpha <- function(color, alpha) {
  rgb <- grDevices::col2rgb(color)
  sprintf("rgba(%d,%d,%d,%.2f)", rgb[1, ], rgb[2, ], rgb[3, ], alpha)
}
