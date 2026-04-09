# Tests for Task 5 — abstracts2text() behavior.

make_abstracts_df <- function(n = 5) {
  data.frame(
    doi      = lacsSample$doi[seq_len(n)],
    title    = lacsSample$title[seq_len(n)],
    abstract = lacsSample$abstract[seq_len(n)],
    class    = lacsSample$class[seq_len(n)],
    stringsAsFactors = FALSE
  )
}

test_that("abstracts2text returns character() for NULL input", {
  result <- abstracts2text(NULL)
  expect_equal(result, character())
})

test_that("abstracts2text returns a character vector for an abstracts object", {
  abstracts <- get_abstracts(make_abstracts_df())
  result    <- abstracts2text(abstracts)
  expect_type(result, "character")
  expect_length(result, nrow(abstracts))
})

test_that("abstracts2text returns a character vector for a plain character input", {
  input  <- c("First abstract.", "Second abstract.")
  result <- abstracts2text(input)
  expect_type(result, "character")
  expect_length(result, 2L)
})

test_that("abstracts2text output is UTF-8 encoded", {
  abstracts <- get_abstracts(make_abstracts_df())
  result    <- abstracts2text(abstracts)
  encodings <- Encoding(result)
  # utf8::utf8_encode produces strings with "unknown" or "UTF-8" encoding;
  # neither should be "bytes" or "latin1".
  expect_false(any(encodings == "bytes"))
  expect_false(any(encodings == "latin1"))
})

test_that("abstracts2text errors on invalid input", {
  expect_error(abstracts2text(list(a = "text")),  "abstracts object or a character vector")
  expect_error(abstracts2text(data.frame(x = 1)), "abstracts object or a character vector")
  expect_error(abstracts2text(42L),               "abstracts object or a character vector")
})
