# Tests for Task 3 — new_abstracts() constructor behavior.

test_that("new_abstracts returns an object of class 'abstracts'", {
  result <- new_abstracts(
    doi      = "10.1/a",
    title    = "Title A",
    abstract = "Abstract A",
    class    = "positive"
  )
  expect_s3_class(result, "abstracts")
})

test_that("'abstracts' is the first class in the class vector", {
  result <- new_abstracts(
    doi      = "10.1/a",
    title    = "Title A",
    abstract = "Abstract A",
    class    = "positive"
  )
  expect_equal(class(result)[1], "abstracts")
})

test_that("new_abstracts also inherits from tbl_df and data.frame", {
  result <- new_abstracts(
    doi      = "10.1/a",
    title    = "Title A",
    abstract = "Abstract A",
    class    = "positive"
  )
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "data.frame")
})

test_that("new_abstracts returns exactly the four canonical columns in order", {
  result <- new_abstracts(
    doi      = c("10.1/a", "10.1/b"),
    title    = c("Title A", "Title B"),
    abstract = c("Abstract A", "Abstract B"),
    class    = c("positive", "unknown")
  )
  expect_equal(names(result), c("doi", "title", "abstract", "class"))
})

test_that("new_abstracts preserves input data exactly", {
  dois      <- c("10.1/a", "10.1/b", "10.1/c")
  titles    <- c("Title A", "Title B", "Title C")
  abstracts <- c("Abstract A", "Abstract B", "Abstract C")
  classes   <- c("positive", "unknown", "positive")
  result <- new_abstracts(doi = dois, title = titles,
                          abstract = abstracts, class = classes)
  expect_equal(result$doi,      dois)
  expect_equal(result$title,    titles)
  expect_equal(result$abstract, abstracts)
  expect_equal(result$class,    classes)
})

test_that("new_abstracts works with empty vectors", {
  result <- new_abstracts()
  expect_s3_class(result, "abstracts")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("doi", "title", "abstract", "class"))
})

test_that("new_abstracts does not validate class values (that is validate_abstracts's job)", {
  # The constructor is a low-level primitive; it must not duplicate validation.
  result <- new_abstracts(
    doi      = "10.1/a",
    title    = "Title A",
    abstract = "Abstract A",
    class    = "any_raw_value"
  )
  expect_s3_class(result, "abstracts")
  expect_equal(result$class, "any_raw_value")
})
