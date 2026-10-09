# Edge-case kit, baseline (tests/edge_cases/baseline/README.md): the series
# the kit runs on and the plain declaration every category is compared
# against.

test_that("the peak list holds the whole MLKL series", {
  ctx <- kit_ctx()
  expect_length(ctx$samples, 122)
  expect_equal(sum(vapply(ctx$result$deconvolution, function(s) nrow(s$peaks), 1L)), 466)
  # The design read off the names: 7 concentrations, two series
  expect_equal(sort(unique(ctx$conc)), c(0, 2.5, 5, 10, 20, 40, 80))
  expect_equal(sort(unique(ctx$rep)), c("R1", "R2"))
  expect_false(anyNA(ctx$time))
})

test_that("the baseline files are written and load like uploads", {
  expect_kit_loads("baseline")
})

test_that("B0: the baseline declaration passes without a hint", {
  d <- kit_check("baseline/proteins_baseline", "baseline/compounds_baseline", "baseline/config_baseline")
  expect_equal(d$status, "PASS")
  expect_null(d$message)
})

test_that("B0: binding of the baseline", {
  r <- kit_case("baseline/proteins_baseline", "baseline/compounds_baseline", "baseline/config_baseline")

  card <- kit_card(r, "BI-8925")
  expect_rounded(card[["min"]], 8.29, 2)
  expect_rounded(card[["max"]], 100, 2)
  expect_rounded(card[["mean"]], 73.19, 2)
  expect_rounded(card[["sd"]], 23.61, 2)
  expect_equal(card[["samples"]], 120)
  expect_rounded(kit_all_samples(r), 70.245, 3)

  # The reference sample: one complex peak of the main form, the second form
  # unbound
  ref <- kit_sample_hits(r)
  expect_equal(nrow(ref), 2)
  expect_equal(ref$`Peak [Da]`, c(21903.5, 21816))
  expect_rounded(ref$Intensity[1], 17.43, 2)
  expect_rounded(r$total[[kit_reference_sample]], 12.04, 2)

  # No hit: the R1 control, and 2o5_1min_R1 whose complex sits 4.34 Da off
  expect_setequal(
    kit_no_hits(r),
    c("2026-09-18_MULI+BI-8925_0_0min_R1", "2026-09-18_MULI+BI-8925_2o5_1min_R1")
  )
  # The R2 control reads binding although no compound was added: its 21,903 Da
  # peak lies within the tolerance of the complex
  expect_rounded(r$total[["2026-09-18_MULI+BI-8925_0_0min_R2"]], 9.20, 2)
})

test_that("B0: kinetics of the baseline", {
  r <- kit_case("baseline/proteins_baseline", "baseline/compounds_baseline", "baseline/config_baseline")

  expect_equal(r$complexes, "MLKL + BI-8925")
  expect_equal(r$default, "MLKL + BI-8925")
  k <- r$kinetics[["MLKL + BI-8925"]]
  expect_equal(k$n, 122)
  expect_rounded(k$ratio, 334.1)
  expect_rounded(k$ci[[1]], 300.7)
  expect_rounded(k$ci[[2]], 378.6)
  expect_equal(k$status, "saturated")
  expect_equal(k$warnings, "Plateaus differ")
  expect_rounded(k$series[["R1"]], 327.9)
  expect_rounded(k$series[["R2"]], 341.0)

  # Proteoforms tab: the fitted concentrations only, so 120 samples, not 122
  expect_equal(k$reference, 21638.84)
  main <- k$proteoforms[["21638.84"]]
  second <- k$proteoforms[["21816.84"]]
  expect_rounded(main$ratio, 357.6)
  expect_rounded(second$ratio, 231.2)
  expect_equal(c(second$n_limit, second$n), c(59, 120))
  expect_false(second$flagged)
})
