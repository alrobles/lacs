# Fit a plus model to an abstracts object

Fit a plus model to an abstracts object

## Usage

``` r
fit_plus(
  abstracts,
  vocabulary = NULL,
  tf_idf = TRUE,
  alpha = 1,
  sample_use_time = 30,
  learning_rate = 1,
  qq = 0.1
)
```

## Arguments

- abstracts:

  An abstracts object.

- vocabulary:

  A `ext2vec_vocabulary` object from a specific corpus to create a
  document - term matrix.

- tf_idf:

  Logical. Default is true, use a tf_idf in the document - term matrix S

- alpha:

  Plus parameter. The elastic net mixing parameter, with \\0\le\alpha\le
  1\\

- sample_use_time:

  Plus parameter. Number of use time.

- learning_rate:

  Plus parameter. Learning rate. Default is 1.

- qq:

  Plus parameter. quantile threshold

## Value

Return a plus object.(Positive and unlabeled Learning from Unbalanced
cases and Sparse structures, PLUS). This is a cross-validated glmnet
logistics regression optimized for positive and unlabeled learning. This
classification is applied to a dtm matrix created with the vocabulary
and the abstracts. The independent variables are the positive and
unknown classes of the provided `abstract` object.

## Examples

``` r
abstracts <- get_abstracts(lacsSample)
v <- get_vocabulary(abstracts, term_count_min = 2)
s <- sample(seq(nrow(abstracts)), 65, replace = FALSE)
train <- abstracts[s, ]
model <- fit_plus(abstracts = train, vocabulary = v)
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: Too few (< 10) observations per fold for type.measure='auc' in cv.lognet; changed to type.measure='deviance'. Alternatively, use smaller value for nfolds
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Error in lognet(x, is.sparse, y, weights, offset, alpha, nobs, nvars,     jd, vp, cl, ne, nx, nlam, flmin, ulam, thresh, isd, intr,     vnames, maxit, kopt, family, pb): one multinomial or binomial class has 1 or 0 observations; not allowed
model
#> Error: object 'model' not found
```
