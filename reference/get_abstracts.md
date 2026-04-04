# abstracts model helper function

abstracts model helper function

## Usage

``` r
get_abstracts(x = NULL)
```

## Arguments

- x:

  A data frame with abstracts. Should contain doi, title, abstracts and
  a class column. The class should be possitive or unknown.

## Value

and object of the class abstracts

## Examples

``` r
abstracts <- get_abstracts(lacsSample)
```
