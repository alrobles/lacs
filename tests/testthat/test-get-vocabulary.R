# Tests for Task 4 — get_vocabulary() filtering behaviour.
#
# These tests use minimal, controlled abstracts to verify each filter rule
# independently and guard against regressions.

# Helper: build an abstracts object from a character vector of abstract texts.
make_vocab_abstracts <- function(texts) {
  n <- length(texts)
  df <- data.frame(
    doi      = paste0("10.1/", seq_len(n)),
    title    = paste0("Title ", seq_len(n)),
    abstract = texts,
    class    = rep(c("positive", "unknown"), length.out = n),
    stringsAsFactors = FALSE
  )
  get_abstracts(df)
}

# ── Return type ───────────────────────────────────────────────────────────────

test_that("get_vocabulary returns a text2vec_vocabulary object", {
  abs_obj <- make_vocab_abstracts(c(
    "microbiology research laboratory analysis",
    "microbiology research laboratory analysis"
  ))
  result <- get_vocabulary(abs_obj, term_count_min = 1)
  expect_s3_class(result, "text2vec_vocabulary")
})

# ── Character-length filter (nchar > 3) ───────────────────────────────────────

test_that("get_vocabulary excludes terms with three or fewer characters", {
  # "cat" (3 chars) and "dog" (3 chars) should be dropped; "cats" (4) kept
  abs_obj <- make_vocab_abstracts(c(
    "cats cat dog long research microbiology laboratory analysis",
    "cats cat dog long research microbiology laboratory analysis"
  ))
  result <- get_vocabulary(abs_obj, term_count_min = 1)
  terms <- result$term
  expect_false("cat" %in% terms)
  expect_false("dog" %in% terms)
  expect_true("cats" %in% terms)
})

# ── Digit-start filter (^[0-9]) ───────────────────────────────────────────────

test_that("get_vocabulary excludes terms that start with a digit", {
  abs_obj <- make_vocab_abstracts(c(
    "2019data microbiology research laboratory analysis population",
    "2019data microbiology research laboratory analysis population"
  ))
  result <- get_vocabulary(abs_obj, term_count_min = 1)
  terms <- result$term
  expect_false(any(grepl("^[0-9]", terms)))
})

# ── Digit-end filter ([0-9]$) — regression guard for the fixed regex ──────────

test_that("get_vocabulary excludes terms that end with a digit", {
  # "sample5" and "result3" end in digits and must be filtered out
  abs_obj <- make_vocab_abstracts(c(
    "sample5 result3 microbiology research laboratory analysis population",
    "sample5 result3 microbiology research laboratory analysis population"
  ))
  result <- get_vocabulary(abs_obj, term_count_min = 1)
  terms <- result$term
  expect_false(any(grepl("[0-9]$", terms)))
})

# ── Stopword filter ───────────────────────────────────────────────────────────

test_that("get_vocabulary excludes common English stopwords", {
  # "from" is a standard English stopword with nchar = 4 (passes the nchar
  # filter), so its absence proves the stopword filter is active.
  abs_obj <- make_vocab_abstracts(c(
    "results from microbiology research laboratory analysis population",
    "results from microbiology research laboratory analysis population"
  ))
  result <- get_vocabulary(abs_obj, term_count_min = 1)
  terms <- result$term
  expect_false("from" %in% terms)
})

# ── n-gram range ──────────────────────────────────────────────────────────────

test_that("get_vocabulary includes bigrams in the vocabulary", {
  # The function uses ngram = c(1L, 5L), so multi-word phrases should appear.
  # "microbiology research" should produce the bigram "microbiology_research".
  abs_obj <- make_vocab_abstracts(c(
    "microbiology research laboratory analysis population study",
    "microbiology research laboratory analysis population study"
  ))
  result <- get_vocabulary(abs_obj, term_count_min = 1)
  terms <- result$term
  expect_true(any(grepl("_", terms)))
})

# ── term_count_min pruning ────────────────────────────────────────────────────

test_that("get_vocabulary respects term_count_min", {
  # "rare" appears only once; "common" appears in both documents.
  abs_obj <- make_vocab_abstracts(c(
    "common microbiology research laboratory rare population",
    "common microbiology research laboratory analysis population"
  ))
  result_strict <- get_vocabulary(abs_obj, term_count_min = 2)
  result_loose  <- get_vocabulary(abs_obj, term_count_min = 1)

  # "rare" appears once → excluded at term_count_min = 2, present at 1
  expect_false("rare" %in% result_strict$term)
  expect_true("rare" %in% result_loose$term)

  # "common" appears twice → present at both thresholds
  expect_true("common" %in% result_strict$term)
  expect_true("common" %in% result_loose$term)
})
