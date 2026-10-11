# Unit tests of the static colour system of the conversion results: the
# palettes (app/logic/palette.R) and the result colour key
# (color_key()/key_colors()/key_symbols() in conversion_functions.R).

box::use(
  app/logic/palette[
    base_colors,
    categorical_colors,
    categorical_palette,
    concentration_symbols,
    control_symbol,
    dark_background,
    level_tier,
    ramp_at,
    ramp_colors,
    ramp_colorscale,
    sample_colors,
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
    default_color_role,
    overview_colors,
    sample_level_colors,
    sample_view_colors,
    key_colors,
    key_marker_symbols,
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
    expect_length(categorical_palette[[v]], 18)
    expect_true(hex_ok(categorical_palette[[v]]))
    expect_true(hex_ok(unlist(status_colors(v))))
    for (r in c("concentration", "time", "sample", "count", "binding", "correct", "unmatched")) {
      expect_true(hex_ok(ramp_colors(11, r, v)), info = paste(r, v))
    }
  }
})

test_that("the light colours stand out on the grey cards and on white", {
  base <- categorical_palette$light[seq_len(base_colors)]
  light <- c(
    base,
    ramp_colors(11, "concentration"),
    ramp_colors(11, "time"),
    ramp_colors(9, "sample")
  )
  for (g in card_greys) {
    expect_gte(min(de2000(light, g)), 20)
  }
  expect_gte(min(de2000(light, "#FFFFFF")), 30)
  # Graphical objects: at least 3:1 against a white export
  expect_gte(min(wcag_contrast(base, "#FFFFFF")), 3)
  # The light accents fill areas and outlined markers: clear of the greys,
  # and light enough for black text
  accents <- categorical_palette$light[-seq_len(base_colors)]
  for (g in c(card_greys, "#FFFFFF")) {
    expect_gte(min(de2000(accents, g)), 20)
  }
  expect_gte(min(wcag_contrast(accents, "#000000")), 7)
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
  expect_gte(min(de2000(dark, dark_background)), 35)
  base <- categorical_palette$dark[seq_len(base_colors)]
  expect_gte(min(de2000(base, dark_background)), 40)
  expect_gte(min(wcag_contrast(base, dark_background)), 4.5)
  # The deep twins of the accents are areas: 3:1 like graphical objects
  expect_gte(min(wcag_contrast(categorical_palette$dark, dark_background)), 3)
})

test_that("the categorical colours stay apart", {
  for (v in c("light", "dark")) {
    pal <- categorical_palette[[v]]
    expect_gte(min_pair(pal), 9)
    expect_gte(min_pair(pal[seq_len(base_colors)]), 9)
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
  expect_lte(max(d[seq_len(base_colors)]), 10)
  # The deep twins of the accents give up a little hue to stay in gamut
  expect_lte(max(d), 20)
})

test_that("categorical colours repeat past eighteen, with a tier per round", {
  expect_equal(categorical_colors(0), character(0))
  expect_equal(categorical_colors(3), categorical_palette$light[1:3])
  expect_equal(categorical_colors(3, "dark"), categorical_palette$dark[1:3])
  # Anything but "dark" is the light variant
  expect_equal(categorical_colors(2, NULL), categorical_palette$light[1:2])
  c40 <- categorical_colors(40)
  expect_equal(anyDuplicated(c40[1:18]), 0)
  expect_equal(c40[19], c40[1])
  expect_equal(c40[37], c40[1])
  expect_equal(level_tier(40), rep(0:2, c(18, 18, 4)))
  # Colour and tier together tell apart many levels
  sym <- tier_symbol(level_tier(140))
  expect_equal(anyDuplicated(paste(categorical_colors(140), sym)), 0)
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

test_that("the count ramp keeps the white count labels readable", {
  for (v in c("light", "dark")) {
    cols <- ramp_colors(25, "count", v)
    expect_gte(min(wcag_contrast(cols, "#FFFFFF")), 4.5)
    for (g in card_greys) {
      expect_gte(min(de2000(cols, g)), 20)
    }
  }
})

test_that("samples share one rule in every interface", {
  expect_equal(sample_colors(5), categorical_palette$light[1:5])
  expect_equal(sample_colors(12, "dark"), categorical_palette$dark[1:12])
  expect_equal(sample_colors(13), ramp_colors(13, "sample"))
  expect_equal(sample_colors(0), character(0))
  key <- color_key(fake_hits(samples = paste0("S", 1:20)))
  expect_equal(unname(key_colors(key, "Samples")), sample_colors(20))
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
  expect_equal(unname(tiers[c("CMP-01", "CMP-17", "CMP-18", "CMP-40")]), c(0L, 0L, 1L, 2L))
  expect_equal(anyDuplicated(paste(cols, tier_symbol(tiers))), 0)
})

test_that("compound markers change shape past the colours, never into the protein diamond", {
  expect_false("diamond" %in% tier_symbol(0:20))
  cmps <- sprintf("CMP-%02d", 1:40)
  key <- color_key(fake_hits(samples = paste0("S", 1:40), compounds = cmps))
  sym <- key_marker_symbols(key, "Compounds", cmps)
  expect_named(sym, cmps)
  # P is the first entity, so CMP-17 is the eighteenth colour
  expect_equal(
    unname(sym[c("CMP-01", "CMP-17", "CMP-18", "CMP-40")]),
    c("circle", "circle", "square", "triangle-up")
  )
})

# What to colour ----

test_that("views open on the experiment's main factor", {
  roles <- c("protein", "compound", "concentration", "time", "sample")
  # Kinetics of one compound: the concentrations
  kin <- color_key(fake_hits(compounds = "A", conc = c(0, 2.5, 10, 40)))
  expect_equal(default_color_role(kin, roles), "concentration")
  # A screen at one concentration: the compounds
  screen <- color_key(fake_hits(compounds = c("A", "B"), conc = c(0, 10)))
  expect_equal(default_color_role(screen, roles), "compound")
  # One compound, one concentration, several proteins: the proteins
  prot <- color_key(fake_hits(proteins = c("P", "Q"), compounds = "A", conc = 10, time = 60))
  expect_equal(default_color_role(prot, roles), "protein")
  # Nothing varies but the samples
  one <- color_key(fake_hits(compounds = "A", conc = 10, time = 60))
  expect_equal(default_color_role(one, roles), "sample")
  # Only what a view offers is chosen
  expect_equal(default_color_role(kin, c("compound", "sample")), "sample")
  expect_equal(default_color_role(prot, c("compound", "concentration", "sample")), "sample")
  # Time decides where concentrations do not
  times <- color_key(fake_hits(compounds = "A", conc = 10, time = c(5, 10, 60, 120)))
  expect_equal(default_color_role(times, roles), "time")
})

test_that("samples keep the table order, and know their own variables", {
  hits <- fake_hits(samples = c("S10", "S2", "S1", "S3"), conc = c(40, 0, 10, 2.5))
  # By name without an order...
  expect_equal(color_key(hits)$samples, c("S1", "S2", "S3", "S10"))
  # ... in the table's order with one
  table_order <- hits$`Sample ID`[order(hits$`Conc. [μM]`)]
  key <- color_key(hits, sample_order = table_order)
  expect_equal(key$samples, c("S2", "S3", "S1", "S10"))
  info <- key$sample_info
  expect_equal(info$concentration[info$sample == "S10"], 40)
  expect_equal(info$compound[info$sample == "S2"], "B")
  # Samples by their concentration take the concentration's colour
  cols <- sample_level_colors(key, "Concentration", c("S10", "S2"))
  expect_named(cols, c("S10", "S2"))
  expect_equal(unname(cols), unname(key_colors(key, "Concentration", c(40, 0))))
  # Short IDs work the same
  short <- sample_level_colors(key, "Concentration", c("s1", "s2"), trunc = TRUE)
  expect_equal(unname(short), unname(key_colors(key, "Concentration", c(40, 0))))
  # By sample, their own colour; protein and time alike
  expect_equal(sample_level_colors(key, "Samples", "S3"), key_colors(key, "Samples", "S3"))
  expect_equal(
    unname(sample_level_colors(key, "Protein", "S3")),
    unname(key_colors(key, "Protein", "P"))
  )
})

test_that("the Overview and Sample View colour samples by the variable picked", {
  hits <- fake_hits(compounds = "A", conc = c(0, 2.5, 10, 40))
  hits$`Tot. Binding [%]` <- c(0, 20, 50, 90)
  key <- color_key(hits)
  by_conc <- overview_colors(hits, "P", "A", "Concentration", key = key)
  expect_setequal(names(by_conc), hits$`Sample ID`)
  expect_equal(
    unname(by_conc[hits$`Sample ID`]),
    unname(key_colors(key, "Concentration", hits$`Conc. [μM]`))
  )
  one <- sample_view_colors(hits, key, "Time", "S4")
  expect_equal(unname(one), unname(key_colors(key, "Time", 60)))
  cmp <- sample_view_colors(hits, key, "Compounds", "S4")
  expect_equal(cmp, key_colors(key, "Compounds", "A"))
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

test_that("many compounds on the batch heatmap: labelled tiles, one legend group each", {
  hs <- stats_table(n_groups = 22, n = 30)
  key <- color_key(hs)
  built <- plotly::plotly_build(batch_plate_heatmap(hs, "Compound", key = key))$x$data
  levels_ <- sprintf("C%02d", 1:22)
  tiles <- Filter(function(t) identical(t$type, "heatmap") && !is.null(t$legendgroup), built)
  chips <- Filter(function(t) identical(t$type, "scatter") && grepl("^level-", t$legendgroup %||% ""), built)
  expect_length(tiles, 22)
  expect_length(chips, 22)
  # Each level's tiles and chip share a legend group, so a click on the chip
  # hides or isolates the tiles
  expect_equal(
    vapply(tiles, `[[`, "", "legendgroup"),
    vapply(chips, `[[`, "", "legendgroup")
  )
  # Past the colours, numbers on the tiles, named in the legend
  expect_true(all(vapply(tiles, `[[`, "", "texttemplate") == "%{text}"))
  # Each label only on its own wells
  expect_equal(
    vapply(tiles, function(t) paste(setdiff(unique(as.vector(t$text)), ""), collapse = ","), ""),
    as.character(1:22)
  )
  expect_equal(vapply(chips, `[[`, "", "name"), paste0(1:22, " \u00b7 ", levels_))
  expect_equal(
    vapply(chips, function(t) t$marker$color, ""),
    unname(key_colors(key, "Compound", levels_))
  )
  # Up to eighteen the colours suffice: no numbers
  few <- stats_table(n_groups = 18, n = 30)
  built <- plotly::plotly_build(batch_plate_heatmap(few, "Compound", key = color_key(few)))$x$data
  tiles <- Filter(function(t) identical(t$type, "heatmap") && !is.null(t$legendgroup), built)
  expect_true(all(vapply(tiles, function(t) is.null(t$texttemplate), TRUE)))
  # The hover sits on the plate underneath, one trace for every well
  # (below it only the tiles of the empty wells, which take no hover)
  base <- Filter(
    function(t) {
      identical(t$type, "heatmap") &&
        is.null(t$legendgroup) &&
        !identical(t$hoverinfo, "skip")
    },
    built
  )
  expect_length(base, 1)
  expect_identical(base[[1]]$hoverinfo, "text")
})

test_that("the batch heatmap draws the whole 96- or 384-well plate", {
  plate_dims <- function(hs) {
    built <- plotly::plotly_build(
      batch_plate_heatmap(hs, "Total % Binding", key = color_key(hs))
    )$x$data
    base <- Filter(function(t) identical(t$type, "heatmap"), built)[[1]]
    z <- base$z
    if (is.list(z)) z <- do.call(rbind, z)
    c(nrow(z), ncol(z))
  }

  # A few wells of one row: still the full 96-well plate, not that row
  few <- stats_table(n = 8)
  expect_equal(plate_dims(few), c(8L, 12L))

  # One well beyond H12 makes it a 384-well plate
  big <- few
  big$Well[1] <- "I13"
  expect_equal(plate_dims(big), c(16L, 24L))
})

test_that("the batch heatmap keeps its tiles square and its marks in plate units", {
  hs <- stats_table(n = 12)
  hs$`Compound Mw [Da]`[1] <- NA # A1: no compound binding
  hs$`Compound Mw [Da]`[12] <- NA # B2: no protein peak either
  hs$`Measured Mw Protein [Da]`[12] <- NA
  lay <- plotly::plotly_build(
    batch_plate_heatmap(hs, "Total % Binding", key = color_key(hs))
  )$x$layout
  # scaleanchor holds only between axes of one type: the rows by position,
  # labelled by letter, not a category axis against the linear columns
  expect_identical(lay$yaxis$scaleanchor, "x")
  expect_false(identical(lay$yaxis$type, "category"))
  expect_equal(unlist(lay$yaxis$tickvals), 1:8)
  expect_equal(unlist(lay$yaxis$ticktext), LETTERS[1:8])
  # The marks in plate units within their tile, not in pixels
  shapes <- lay$shapes
  expect_true(all(vapply(shapes, function(s) is.null(s$xsizemode), TRUE)))
  ring <- Filter(function(s) identical(s$type, "circle"), shapes)
  expect_length(ring, 1)
  expect_equal(c(ring[[1]]$x0, ring[[1]]$x1), c(1 - 0.3, 1 + 0.3))
  expect_equal(c(ring[[1]]$y0, ring[[1]]$y1), c(1 - 0.3, 1 + 0.3))
  cross <- Filter(function(s) identical(s$type, "line"), shapes)
  expect_length(cross, 2)
  expect_true(all(vapply(cross, function(s) abs(s$x0 - 2) < 0.5 && abs(s$y0 - 2) < 0.5, TRUE)))
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
  legend <- Filter(function(t) identical(t$type, "scatter") && grepl("^level-", t$legendgroup %||% ""), built)
  expect_equal(vapply(legend, function(t) t$name, ""), c("5", "10", "60"))
  expect_equal(
    vapply(legend, function(t) t$marker$color, ""),
    unname(key_colors(key, "Time", c("5", "10", "60")))
  )
})
