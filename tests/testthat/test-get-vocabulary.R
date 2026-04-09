# Tests for Task 4 — get_vocabulary() filtering behavior.

make_abstracts_df <- function() {
  data.frame(
    doi      = lacsSample$doi[1:50],
    title    = lacsSample$title[1:50],
    abstract = lacsSample$abstract[1:50],
    class    = lacsSample$class[1:50],
    stringsAsFactors = FALSE
  )
}

test_that("get_vocabulary returns an object with a 'term' column", {
  abstracts <- get_abstracts(make_abstracts_df())
  v <- get_vocabulary(abstracts, term_count_min = 2)
  expect_true("term" %in% names(v))
})

test_that("get_vocabulary returns a data.table-compatible object", {
  abstracts <- get_abstracts(make_abstracts_df())
  v <- get_vocabulary(abstracts, term_count_min = 2)
  # text2vec vocabulary is a data.table
  expect_true(is.data.frame(v))
})

test_that("no term starts with a digit", {
  abstracts <- get_abstracts(make_abstracts_df())
  v <- get_vocabulary(abstracts, term_count_min = 2)
  expect_false(any(grepl("^[0-9]", v$term)))
})

test_that("no term ends with a digit (validates fixed regex)", {
  abstracts <- get_abstracts(make_abstracts_df())
  v <- get_vocabulary(abstracts, term_count_min = 2)
  expect_false(any(grepl("[0-9]$", v$term)))
})

test_that("no term is a pure numeric-underscore string (validates fixed ____ filter)", {
  abstracts <- get_abstracts(make_abstracts_df())
  v <- get_vocabulary(abstracts, term_count_min = 2)
  expect_false(any(grepl("^[0-9_]+$", v$term)))
})

test_that("all returned terms are longer than 3 characters", {
  abstracts <- get_abstracts(make_abstracts_df())
  v <- get_vocabulary(abstracts, term_count_min = 2)
  expect_true(all(nchar(v$term) > 3))
})

test_that("term_count_min prunes low-frequency terms", {
  abstracts <- get_abstracts(make_abstracts_df())
  v_loose  <- get_vocabulary(abstracts, term_count_min = 1)
  v_strict <- get_vocabulary(abstracts, term_count_min = 10)
  expect_gt(nrow(v_loose), nrow(v_strict))
})

test_that("English stopwords are excluded from vocabulary", {
  abstracts <- get_abstracts(make_abstracts_df())
  v <- get_vocabulary(abstracts, term_count_min = 1)
  common_stopwords <- c("the", "and", "this", "that", "with")
  # Stopwords are removed before n-gram construction, so none should appear
  # as unigrams in the vocabulary.
  expect_false(any(common_stopwords %in% v$term))
})
