# Tests for Task 5 — get_dtm() behavior.

make_abstracts_df <- function(n = 20) {
  data.frame(
    doi      = lacsSample$doi[seq_len(n)],
    title    = lacsSample$title[seq_len(n)],
    abstract = lacsSample$abstract[seq_len(n)],
    class    = lacsSample$class[seq_len(n)],
    stringsAsFactors = FALSE
  )
}

make_vocab <- function(abstracts) {
  get_vocabulary(abstracts, term_count_min = 2)
}

test_that("get_dtm returns a matrix-like object", {
  abstracts <- get_abstracts(make_abstracts_df())
  vocab     <- make_vocab(abstracts)
  dtm       <- get_dtm(abstracts, vocab, tf_idf = FALSE)
  expect_true(is.matrix(dtm) || inherits(dtm, "sparseMatrix") || inherits(dtm, "dgCMatrix"))
})

test_that("get_dtm row count equals number of input documents (abstracts object)", {
  abstracts <- get_abstracts(make_abstracts_df(20))
  vocab     <- make_vocab(abstracts)
  dtm       <- get_dtm(abstracts, vocab, tf_idf = FALSE)
  expect_equal(nrow(dtm), 20L)
})

test_that("get_dtm column count equals vocabulary size", {
  abstracts <- get_abstracts(make_abstracts_df())
  vocab     <- make_vocab(abstracts)
  dtm       <- get_dtm(abstracts, vocab, tf_idf = FALSE)
  expect_equal(ncol(dtm), nrow(vocab))
})

test_that("get_dtm works when abstracts is a plain character vector", {
  abstracts  <- get_abstracts(make_abstracts_df())
  vocab      <- make_vocab(abstracts)
  char_input <- lacsSample$abstract[seq_len(5)]
  dtm        <- get_dtm(char_input, vocab, tf_idf = FALSE)
  expect_equal(nrow(dtm), 5L)
})

test_that("get_dtm with tf_idf = TRUE differs from tf_idf = FALSE", {
  abstracts <- get_abstracts(make_abstracts_df())
  vocab     <- make_vocab(abstracts)
  dtm_bin   <- get_dtm(abstracts, vocab, tf_idf = FALSE)
  dtm_tfidf <- get_dtm(abstracts, vocab, tf_idf = TRUE)
  # The matrices should have the same shape but different values.
  expect_equal(dim(dtm_bin), dim(dtm_tfidf))
  expect_false(identical(as.matrix(dtm_bin), as.matrix(dtm_tfidf)))
})

test_that("get_dtm errors on invalid input", {
  abstracts <- get_abstracts(make_abstracts_df())
  vocab     <- make_vocab(abstracts)
  expect_error(get_dtm(list(a = 1), vocab), "abstracts object or a character vector")
  expect_error(get_dtm(42L,          vocab), "abstracts object or a character vector")
})
