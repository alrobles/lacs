# Tests specifically for Task 2 — validate_abstracts() and get_abstracts() validation behaviour.
# Label-terminology tests (canonical spelling acceptance/rejection) live in
# test-label-terminology.R.  These tests focus on the structural validation
# contract: required columns, class value enforcement, return shape, and
# the propagation of errors through get_abstracts().

make_valid_df <- function(...) {
  base <- data.frame(
    doi      = c("10.1/a", "10.1/b"),
    title    = c("Title A", "Title B"),
    abstract = c("Abstract A", "Abstract B"),
    class    = c("positive", "unknown"),
    stringsAsFactors = FALSE
  )
  modifyList(base, list(...))
}

# ── validate_abstracts() ──────────────────────────────────────────────────────

test_that("validate_abstracts returns a data.frame", {
  result <- validate_abstracts(make_valid_df())
  expect_s3_class(result, "data.frame")
})

test_that("validate_abstracts returns exactly the four canonical columns", {
  result <- validate_abstracts(make_valid_df())
  expect_equal(names(result), c("doi", "title", "abstract", "class"))
})

test_that("validate_abstracts drops extra columns from input", {
  df <- make_valid_df()
  df$extra_col <- "should be dropped"
  result <- validate_abstracts(df)
  expect_equal(names(result), c("doi", "title", "abstract", "class"))
  expect_false("extra_col" %in% names(result))
})

test_that("validate_abstracts errors when given NULL", {
  expect_error(validate_abstracts(NULL), "Missing required columns")
})

test_that("validate_abstracts error lists all missing columns at once", {
  df <- data.frame(doi = "10.1/a", stringsAsFactors = FALSE)
  err <- tryCatch(validate_abstracts(df), error = function(e) conditionMessage(e))
  expect_match(err, "title")
  expect_match(err, "abstract")
  expect_match(err, "class")
})

test_that("validate_abstracts error names the specific missing column", {
  df <- data.frame(
    title    = "T",
    abstract = "A",
    class    = "positive",
    stringsAsFactors = FALSE
  )
  expect_error(validate_abstracts(df), "doi")
})

test_that("validate_abstracts error names the invalid class value", {
  df <- make_valid_df(class = c("positive", "bad_value"))
  err <- tryCatch(validate_abstracts(df), error = function(e) conditionMessage(e))
  expect_match(err, "bad_value")
  expect_match(err, "positive")
  expect_match(err, "unknown")
})

test_that("validate_abstracts preserves row count for valid input", {
  df <- make_valid_df()
  result <- validate_abstracts(df)
  expect_equal(nrow(result), nrow(df))
})

# ── get_abstracts() ───────────────────────────────────────────────────────────

test_that("get_abstracts returns an object of class 'abstracts'", {
  result <- get_abstracts(make_valid_df())
  expect_s3_class(result, "abstracts")
})

test_that("get_abstracts propagates missing-column error from validate_abstracts", {
  df <- data.frame(doi = "10.1/a", title = "T", abstract = "A",
                   stringsAsFactors = FALSE)
  expect_error(get_abstracts(df), "Missing required columns")
})

test_that("get_abstracts propagates invalid-class error from validate_abstracts", {
  df <- make_valid_df(class = c("positive", "INVALID"))
  expect_error(get_abstracts(df), "Invalid class values")
})

test_that("get_abstracts result contains all four canonical columns", {
  result <- get_abstracts(make_valid_df())
  expect_equal(names(result)[seq_along(c("doi", "title", "abstract", "class"))],
               c("doi", "title", "abstract", "class"))
})
