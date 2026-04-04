# Create vocabulary from abstracts

Create vocabulary from abstracts

## Usage

``` r
get_vocabulary(
  abstracts,
  term_count_min = 2,
  doc_proportion_min = 0,
  doc_proportion_max = 1
)
```

## Arguments

- abstracts:

  An abstracts object build with get_abstracts function

- term_count_min:

  The minimum number of counts for an specific term in vocabulary

- doc_proportion_min:

  The minimum proportion of documents which should contain term

- doc_proportion_max:

  The maximum proportion of documents which should contain term.

## Value

A data.table with the vocabulary pruned by the term_count

## Examples

``` r
abstracts <- get_abstracts(lacsSample)
v <- get_vocabulary(abstracts, term_count_min = 10)
```
