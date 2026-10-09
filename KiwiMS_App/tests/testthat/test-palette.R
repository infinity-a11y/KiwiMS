# Unit tests of the static colour system of the conversion results: the
# palettes (app/logic/palette.R) and the result colour key
# (color_key()/key_colors()/key_symbols() in conversion_functions.R).

box::use(
  app/logic/palette[
    categorical_colors,
    categorical_palette,
    concentration_symbols,
    control_symbol,
    dark_background,
    level_tier,
    ramp_at,
    ramp_colors,
    ramp_colorscale,
    status_colors,
    tier_dash,
    tier_pattern,
    tier_symbol,
    tint,
  ],
  app/logic/conversion_functions[
    batch_plate_heatmap,
    color_key,
    concentration_symbol_map,
    key_colors,
    key_order,
    key_symbols,
    key_tiers,
    key_variable,
    proteoform_colors,
    stats_scatter,
    stats_violin,
    transform_hits,
  ],
)

# The grey plot cards of the app, darkest and lightest spots (sampled from a
# screenshot), and the backgrounds of the two export themes
card_greys <- c("#888B96", "#A8ABA9")

de2000 <- function(a, b) {
  lab <- function(x) farver::convert_colour(t(grDevices::col2rgb(x)), "rgb", "lab")
  farver::compare_colour(lab(a), lab(b), "lab", method = "cie2000")
}

wcag_contrast <- function(a, b) {
  lum <- function(x) {
    v <- t(grDevices::col2rgb(x)) / 255
    v <- ifelse(v <= 0.04045, v / 12.92, ((v + 0.055) / 1.055)^2.4)
    as.vector(v %*% c(0.2126, 0.7152, 0.0722))
  }
  la <- lum(a)
  lb <- lum(b)
  (pmax(la, lb) + 0.05) / (pmin(la, lb) + 0.05)
}

min_pair <- function(cols) {
  d <- de2000(cols, cols)
  diag(d) <- Inf
  min(d)
}

hex_ok <- function(x) all(grepl("^#[0-9A-Fa-f]{6}$", x))

# A result table in display form with n samples, compounds and concentrations
fake_hits <- function(
  samples = paste0("S", 1:4),
  compounds = c("A", "B"),
  proteins = "P",
  conc = c(0, 2.5, 10, 40),
  time = c(0, 5, 10, 60)
) {
  n <- length(samples)
  data.frame(
    `Sample ID` = samples,
    Protein = rep_len(proteins, n),
    `Cmp Name` = rep_len(compounds, n),
    `Conc. [μM]` = rep_len(conc, n),
    `Time [min]` = rep_len(time, n),
    truncSample_ID = paste0("s", seq_len(n)),
    check.names = FALSE
  )
}

# Palettes ----

test_that("every palette colour is a plain hex colour", {
  for (v in c("light", "dark")) {
    expect_length(categorical_palette[[v]], 12)
    expect_true(hex_ok(categorical_palette[[v]]))
    expect_true(hex_ok(unlist(status_colors(v))))
    for (r in c("concentration", "time", "sample", "binding", "correct", "unmatched")) {
      expect_true(hex_ok(ramp_colors(11, r, v)), info = paste(r, v))
    }
  }
})

test_that("the light colours stand out on the grey cards and on white", {
  light <- c(
    categorical_palette$light,
    ramp_colors(11, "concentration"),
    ramp_colors(11, "time"),
    ramp_colors(9, "sample")
  )
  for (g in card_greys) {
    expect_gte(min(de2000(light, g)), 20)
  }
  expect_gte(min(de2000(light, "#FFFFFF")), 30)
  # Graphical objects: at least 3:1 against a white export
  expect_gte(min(wcag_contrast(categorical_palette$light, "#FFFFFF")), 3)
  # The fixed meanings too, the muted grey included
  status <- unlist(status_colors("light"))
  for (g in card_greys) {
    expect_gte(min(de2000(status, g)), 15)
  }
})

test_that("the dark colours stand out on the dark export", {
  dark <- c(
    categorical_palette$dark,
    ramp_colors(11, "concentration", "dark"),
    ramp_colors(11, "time", "dark"),
    unlist(status_colors("dark"))
  )
  expect_gte(min(de2000(dark, dark_background)), 40)
  expect_gte(min(wcag_contrast(categorical_palette$dark, dark_background)), 4.5)
})

test_that("the categorical colours stay apart", {
  for (v in c("light", "dark")) {
    pal <- categorical_palette[[v]]
    expect_gte(min_pair(pal), 9)
    expect_gte(min_pair(pal[1:5]), 17)
    expect_gte(min_pair(pal[1:2]), 45)
  }
  # No grey among them: greys carry fixed meanings
  chroma <- farver::convert_colour(
    t(grDevices::col2rgb(categorical_palette$light)),
    "rgb",
    "lch"
  )[, 2]
  expect_gte(min(chroma), 30)
})

test_that("light and dark variants keep the hue of each colour", {
  hue <- function(x) farver::convert_colour(t(grDevices::col2rgb(x)), "rgb", "oklch")[, 3]
  d <- abs(hue(categorical_palette$light) - hue(categorical_palette$dark))
  d <- pmin(d, 360 - d)
  expect_lte(max(d), 10)
})

test_that("categorical colours repeat past twelve, with a tier per round", {
  expect_equal(categorical_colors(0), character(0))
  expect_equal(categorical_colors(3), categorical_palette$light[1:3])
  expect_equal(categorical_colors(3, "dark"), categorical_palette$dark[1:3])
  # Anything but "dark" is the light variant
  expect_equal(categorical_colors(2, NULL), categorical_palette$light[1:2])
  c30 <- categorical_colors(30)
  expect_equal(c30[13], c30[1])
  expect_equal(c30[25], c30[1])
  expect_equal(level_tier(30), rep(0:2, c(12, 12, 6)))
  # Colour and tier together tell apart many levels
  sym <- tier_symbol(level_tier(96))
  expect_equal(anyDuplicated(paste(categorical_colors(96), sym)), 0)
  expect_equal(tier_symbol(0), "circle")
  expect_equal(tier_pattern(0), "")
  expect_equal(tier_dash(0), "solid")
  # Rounds past the channel's length cycle rather than run out
  expect_true(nzchar(tier_symbol(100)))
})

test_that("ramps run from one end to the other and are ordered", {
  for (r in c("concentration", "time", "binding")) {
    cols <- ramp_colors(7, r)
    expect_equal(cols[1], ramp_at(0, r))
    expect_equal(cols[7], ramp_at(1, r))
    # Neighbours differ
    steps <- vapply(1:6, function(i) de2000(cols[i], cols[i + 1])[1, 1], 0)
    expect_true(all(steps > 4), info = r)
  }
  expect_equal(ramp_colors(1, "time"), ramp_at(0.5, "time"))
  expect_equal(ramp_colors(0, "time"), character(0))
  # Positions outside 0-1 are clamped
  expect_equal(ramp_at(c(-1, 2), "time"), ramp_at(c(0, 1), "time"))
  cs <- ramp_colorscale("binding")
  expect_length(cs, 9)
  expect_equal(cs[[1]][[1]], 0)
  expect_equal(cs[[9]][[1]], 1)
  expect_error(ramp_colors(3, "nope"))
})

test_that("tints are light enough for black text and keep their names", {
  t <- tint(c(a = "#2563EB", b = "#991B1B"))
  expect_named(t, c("a", "b"))
  expect_true(all(wcag_contrast(t, "#000000") >= 7))
  expect_equal(tint(character(0)), character(0))
})

# Result colour key ----

test_that("the key reads the stored and the display form alike", {
  shown <- fake_hits()
  stored <- data.frame(
    Sample = shown$`Sample ID`,
    Protein = shown$Protein,
    Compound = shown$`Cmp Name`,
    `Concentration [μM]` = shown$`Conc. [μM]`,
    `Time [min]` = shown$`Time [min]`,
    check.names = FALSE
  )
  a <- color_key(shown)
  b <- color_key(stored)
  for (f in c("entities", "proteins", "compounds", "samples", "concentrations", "times")) {
    expect_equal(a[[f]], b[[f]], info = f)
  }
  expect_equal(a$entities, c("P", "A", "B"))
  expect_equal(a$concentrations, c(0, 2.5, 10, 40))
})

test_that("an empty or bare table gives an empty key that still colours", {
  key <- color_key(data.frame())
  expect_length(key$entities, 0)
  expect_length(key$samples, 0)
  # Unknown levels still get colours of their own
  cols <- key_colors(key, "Compounds", c("X", "Y"))
  expect_equal(unname(cols), categorical_palette$light[1:2])
  expect_equal(key_colors(key, "Compounds", character(0)), stats::setNames(character(0), character(0)))
  expect_equal(unname(key_colors(key, "Concentration", c(0, 5))[1]), status_colors()$control)
})

test_that("a compound keeps its colour in every subset and order", {
  key <- color_key(fake_hits(compounds = c("C", "A", "B")))
  all <- key_colors(key, "Compounds")
  expect_named(all, c("A", "B", "C"))
  expect_equal(key_colors(key, "Compounds", c("C", "A")), all[c("C", "A")])
  expect_equal(key_colors(key, "Compounds", "B"), all["B"])
  # The light and dark variants map the same compound to the same slot
  slot <- function(x, v) match(x, categorical_palette[[v]])
  expect_equal(
    slot(key_colors(key, "Compounds", "C"), "light"),
    slot(key_colors(key, "Compounds", "C", theme = "dark"), "dark")
  )
})

test_that("proteins and compounds never share a colour", {
  key <- color_key(fake_hits(proteins = c("P1", "P2"), compounds = c("A", "B")))
  prot <- key_colors(key, "Protein")
  cmp <- key_colors(key, "Compounds")
  expect_length(intersect(prot, cmp), 0)
  expect_equal(key_variable("Cmp Name"), "compound")
  expect_equal(key_variable("Conc. [nM]"), "concentration")
  expect_equal(key_variable("Concentration [μM]"), "concentration")
  expect_equal(key_variable("Time [s]"), "time")
  expect_true(is.na(key_variable("Well")))
  expect_true(is.na(key_variable(NA)))
})

test_that("missing and unknown levels are handled", {
  key <- color_key(fake_hits())
  muted <- status_colors()$muted
  cols <- key_colors(key, "Compounds", c("A", NA, "N/A", "Z"))
  expect_equal(unname(cols[2:3]), c(muted, muted))
  # Z is unknown to the key: a colour after the known ones, not A's
  expect_false(cols[[4]] %in% key_colors(key, "Compounds"))
  # A variable the key does not cover is muted
  expect_equal(unname(key_colors(key, "Well", c("A1", "B2"))), c(muted, muted))
})

test_that("concentrations: control apart, the others ordered by value", {
  key <- color_key(fake_hits(conc = c(0, 2.5, 10, 40)))
  cols <- key_colors(key, "Concentration")
  expect_named(cols, c("0", "2.5", "10", "40"))
  expect_equal(unname(cols[["0"]]), status_colors()$control)
  expect_equal(unname(cols[["2.5"]]), ramp_at(0, "concentration"))
  expect_equal(unname(cols[["40"]]), ramp_at(1, "concentration"))
  # Stable in a subset, as character or numeric, and in another order
  expect_equal(unname(key_colors(key, "Concentration", "10")), unname(cols[["10"]]))
  expect_equal(unname(key_colors(key, "Concentration", 10)), unname(cols[["10"]]))
  expect_equal(key_colors(key, "Concentration", c("40", "0")), cols[c("40", "0")])
  # Missing and non-numeric values are muted
  na <- key_colors(key, "Concentration", c(NA, "N/A"))
  expect_equal(unname(na), rep(status_colors()$muted, 2))
  # A single dosed concentration takes the middle of the ramp
  one <- color_key(fake_hits(conc = c(0, 5)))
  expect_equal(unname(key_colors(one, "Concentration", "5")), ramp_at(0.5, "concentration"))
})

test_that("ten dosed concentrations plus the control are all distinct", {
  conc <- c(0, 0.3125, 0.625, 1.25, 2.5, 5, 10, 20, 40, 80, 160)
  key <- color_key(fake_hits(samples = paste0("S", 1:11), conc = conc))
  cols <- key_colors(key, "Concentration")
  expect_equal(anyDuplicated(cols), 0)
  sym <- key_symbols(key)
  expect_equal(anyDuplicated(sym), 0)
  expect_equal(unname(sym[["160"]]), "circle")
  expect_equal(unname(sym[["0"]]), control_symbol)
  expect_false(control_symbol %in% concentration_symbols)
})

test_that("concentration symbols are fixed by the result, not the subset", {
  key <- color_key(fake_hits(conc = c(0, 2.5, 10, 40)))
  expect_equal(unname(key_symbols(key, c("10", "2.5"))), concentration_symbols[2:3])
  expect_equal(unname(key_symbols(key, "40")), "circle")
  # Past ten concentrations the symbols cycle rather than fail
  many <- color_key(fake_hits(samples = paste0("S", 1:13), conc = 1:13))
  expect_length(key_symbols(many), 13)
  expect_true(all(key_symbols(many) %in% concentration_symbols))
  # The map built from a list of concentrations follows the same rules
  map <- concentration_symbol_map(c("2.5", "0", "40"))
  expect_equal(unname(map[["40"]]), "circle")
  expect_equal(unname(map[["0"]]), control_symbol)
})

test_that("times are ordered by value", {
  key <- color_key(fake_hits(time = c(60, 5, 0, 10)))
  cols <- key_colors(key, "Time")
  expect_named(cols, c("0", "5", "10", "60"))
  expect_equal(unname(cols[["0"]]), ramp_at(0, "time"))
  expect_equal(unname(cols[["60"]]), ramp_at(1, "time"))
  expect_equal(
    key_order(key, "Time", c("60", "5", "10")),
    c("5", "10", "60")
  )
})

test_that("samples: categorical up to twelve, a ramp beyond, natural order", {
  few <- color_key(fake_hits(samples = c("S10", "S2", "S1")))
  expect_equal(few$samples, c("S1", "S2", "S10"))
  expect_equal(
    unname(key_colors(few, "Samples")),
    categorical_palette$light[1:3]
  )

  ids <- sprintf("2026_A_%dmin_R%d", rep(c(1, 5, 10, 60), 96), rep(1:96, each = 4))
  many <- color_key(fake_hits(samples = ids))
  expect_length(many$samples, 384)
  cols <- key_colors(many, "Samples")
  expect_length(cols, 384)
  expect_true(hex_ok(cols))
  expect_equal(unname(cols[1]), ramp_at(0, "sample"))
  expect_equal(unname(cols[384]), ramp_at(1, "sample"))
  # The same sample, the same colour in any subset
  pick <- sample(ids, 20)
  expect_equal(key_colors(many, "Samples", pick), cols[pick])
})

test_that("short sample IDs map to the same colour as the full ones", {
  hits <- fake_hits()
  key <- color_key(hits)
  full <- key_colors(key, "Samples", hits$`Sample ID`)
  short <- key_colors(key, "Samples", hits$truncSample_ID, trunc = TRUE)
  expect_named(short, hits$truncSample_ID)
  expect_equal(unname(short), unname(full))
  expect_named(key_colors(key, "Samples", trunc = TRUE), hits$truncSample_ID)
})

test_that("forty compounds: colours repeat, the tier tells them apart", {
  cmps <- sprintf("CMP-%02d", 1:40)
  key <- color_key(fake_hits(samples = paste0("S", 1:40), compounds = cmps))
  cols <- key_colors(key, "Compounds")
  tiers <- key_tiers(key, "Compounds", names(cols))
  # P comes first in the entity sequence
  expect_equal(unname(tiers[c("CMP-01", "CMP-11", "CMP-12", "CMP-40")]), c(0L, 0L, 1L, 3L))
  expect_equal(anyDuplicated(paste(cols, tier_symbol(tiers))), 0)
})

test_that("key_order sorts like the key", {
  key <- color_key(fake_hits(compounds = c("B", "A")))
  expect_equal(key_order(key, "Compounds", c("Z", "B", "A")), c("A", "B", "Z"))
  expect_equal(key_order(key, "Conc", c("10", "2.5", "0")), c("0", "2.5", "10"))
})

test_that("proteoform colours: cool first, by mass, theme-aware", {
  light <- proteoform_colors(c(21816.8, 21638.8))
  expect_named(light, c("21638.8", "21816.8"))
  expect_equal(unname(light[1]), categorical_palette$light[1])
  dark <- proteoform_colors(c(21816.8, 21638.8), "dark")
  expect_equal(unname(dark[1]), categorical_palette$dark[1])
  expect_equal(anyDuplicated(proteoform_colors(1:9)), 0)
})

# Builders with the key ----

stats_table <- function(n_groups = 3, n = 30) {
  data.frame(
    Sample = paste0("S", seq_len(n)),
    Protein = "P",
    Compound = rep_len(sprintf("C%02d", seq_len(n_groups)), n),
    `Total % Binding` = seq(0, 1, length.out = n),
    `% Correct` = seq(50, 100, length.out = n),
    `% Unmatched` = seq(50, 0, length.out = n),
    `Concentration [μM]` = rep_len(c(0, 5, 10), n),
    `Time [min]` = rep_len(c(5, 60, 10), n),
    Well = paste0(rep(LETTERS[1:3], each = 10), rep(1:10, 3))[seq_len(n)],
    `Measured Mw Protein [Da]` = 1000,
    `Compound Mw [Da]` = 100,
    check.names = FALSE
  )
}

test_that("the stats scatter colours groups by the key and shapes past twelve", {
  hs <- stats_table(n_groups = 20, n = 40)
  key <- color_key(hs)
  built <- plotly::plotly_build(stats_scatter(hs, group_by = "Compound", key = key))$x$data
  traces <- Filter(function(t) identical(t$mode, "markers"), built)
  names_ <- vapply(traces, function(t) t$name, "")
  cols <- vapply(traces, function(t) t$marker$color, "")
  syms <- vapply(traces, function(t) t$marker$symbol, "")
  expect_equal(names_, sprintf("C%02d", 1:20))
  expect_equal(unname(cols), unname(key_colors(key, "Compound", names_)))
  expect_equal(anyDuplicated(paste(cols, syms)), 0)
  # Grouped by concentration: ordered, control in its grey
  built <- plotly::plotly_build(stats_scatter(
    hs,
    group_by = "Concentration [μM]",
    key = key
  ))$x$data
  traces <- Filter(function(t) identical(t$mode, "markers"), built)
  expect_equal(vapply(traces, function(t) t$name, ""), c("0", "5", "10"))
  expect_equal(traces[[1]]$marker$color, status_colors()$control)
})

test_that("the stats violin mutes the groups without a value", {
  hs <- stats_table()
  hs$Compound[1:3] <- NA
  built <- plotly::plotly_build(stats_violin(hs, group_by = "Compound", key = color_key(hs)))$x$data
  unknown <- Filter(function(t) identical(t$name, "Unknown"), built)
  expect_gte(length(unknown), 1)
  col <- unknown[[1]]$line$color %||% unknown[[1]]$marker$color
  muted <- grDevices::col2rgb(status_colors()$muted)
  expect_true(grepl(sprintf("rgba\\(%d,%d,%d", muted[1], muted[2], muted[3]), col))
})

test_that("the batch heatmap takes the key colours and orders times by value", {
  hs <- stats_table()
  key <- color_key(hs)
  for (v in c("Compound", "Protein", "Concentration", "Time", "Total % Binding", "% Correct", "% Unmatched")) {
    for (theme in c("light", "dark")) {
      expect_s3_class(
        plotly::plotly_build(batch_plate_heatmap(hs, v, key = key, theme = theme)),
        "plotly"
      )
    }
  }
  built <- plotly::plotly_build(batch_plate_heatmap(hs, "Time", key = key))$x$data
  legend <- Filter(function(t) identical(t$legendgroup, "categories"), built)
  expect_equal(vapply(legend, function(t) t$name, ""), c("5", "10", "60"))
  expect_equal(
    vapply(legend, function(t) t$marker$color, ""),
    unname(key_colors(key, "Time", c("5", "10", "60")))
  )
})
