test_that("validate_abstracts accepts canonical labels 'positive' and 'unknown'", {
  df <- data.frame(
    doi      = c("10.1/a", "10.1/b"),
    title    = c("Title A", "Title B"),
    abstract = c("Abstract A", "Abstract B"),
    class    = c("positive", "unknown"),
    stringsAsFactors = FALSE
  )
  result <- validate_abstracts(df)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true(all(result$class %in% c("positive", "unknown")))
})

test_that("validate_abstracts rejects misspelled 'possitive' label", {
  df <- data.frame(
    doi      = "10.1/a",
    title    = "Title A",
    abstract = "Abstract A",
    class    = "possitive",
    stringsAsFactors = FALSE
  )
  expect_error(validate_abstracts(df), "Invalid class values")
})

test_that("validate_abstracts rejects misspelled 'unknow' label", {
  df <- data.frame(
    doi      = "10.1/a",
    title    = "Title A",
    abstract = "Abstract A",
    class    = "unknow",
    stringsAsFactors = FALSE
  )
  expect_error(validate_abstracts(df), "Invalid class values")
})

test_that("validate_abstracts rejects unknown class values", {
  df <- data.frame(
    doi      = "10.1/a",
    title    = "Title A",
    abstract = "Abstract A",
    class    = "other",
    stringsAsFactors = FALSE
  )
  expect_error(validate_abstracts(df), "Invalid class values")
})

test_that("validate_abstracts errors on missing required columns", {
  df <- data.frame(doi = "10.1/a", title = "T", abstract = "A",
                   stringsAsFactors = FALSE)
  expect_error(validate_abstracts(df), "Missing required columns")

  df2 <- data.frame(title = "T", abstract = "A", class = "positive",
                    stringsAsFactors = FALSE)
  expect_error(validate_abstracts(df2), "Missing required columns")
})

test_that("validate_abstracts returns columns in canonical order", {
  df <- data.frame(
    class    = c("positive", "unknown"),
    abstract = c("Abstract A", "Abstract B"),
    title    = c("Title A", "Title B"),
    doi      = c("10.1/a", "10.1/b"),
    stringsAsFactors = FALSE
  )
  result <- validate_abstracts(df)
  expect_equal(names(result), c("doi", "title", "abstract", "class"))
})

test_that("get_y returns 1 for 'positive' and 0 for 'unknown'", {
  df <- data.frame(
    doi      = c("10.1/a", "10.1/b", "10.1/c"),
    title    = c("T1", "T2", "T3"),
    abstract = c("A1", "A2", "A3"),
    class    = c("positive", "unknown", "positive"),
    stringsAsFactors = FALSE
  )
  abstracts <- get_abstracts(df)
  y <- get_y(abstracts)
  expect_equal(y, c(1, 0, 1))
})

test_that("get_y returns 0 for every 'unknown' entry", {
  df <- data.frame(
    doi      = c("10.1/a", "10.1/b"),
    title    = c("T1", "T2"),
    abstract = c("A1", "A2"),
    class    = c("unknown", "unknown"),
    stringsAsFactors = FALSE
  )
  abstracts <- get_abstracts(df)
  expect_equal(get_y(abstracts), c(0, 0))
})

test_that("get_y errors when given a non-abstracts object", {
  expect_error(get_y(data.frame(class = "positive")), "abstracts")
})

test_that("get_abstracts and new_abstracts work with canonical labels", {
  df <- data.frame(
    doi      = "10.1/a",
    title    = "Title A",
    abstract = "Abstract A",
    class    = "positive",
    stringsAsFactors = FALSE
  )
  abstracts <- get_abstracts(df)
  expect_s3_class(abstracts, "abstracts")
  expect_equal(abstracts$class, "positive")
})

test_that("lacsSample data uses canonical label 'positive' not 'possitive'", {
  expect_true(all(lacsSample$class %in% c("positive", "unknown")))
  expect_false(any(lacsSample$class == "possitive"))
})
