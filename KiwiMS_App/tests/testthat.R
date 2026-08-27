# Entrypoint for `R CMD check`-style runs.  Day to day the tests are run with
# `rhino::test_r()` from the application root, which calls
# `testthat::test_dir("tests/testthat")` directly.
library(testthat)

test_dir("testthat")
