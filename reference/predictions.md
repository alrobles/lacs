# Title

Title

## Usage

``` r
predictions(object, ...)

# S3 method for class 'lacs'
predictions(object, ..., abstracts, plus = TRUE)
```

## Arguments

- object:

  A lacs object

- ...:

  additional arguments

- abstracts:

  An object of class abstracts. Is a `data.frame` with abstracts

- plus:

  Logical. If it is true use the cutoff threshold from the plus model.
  This tends to reduce the false negatives and increase the recall.
  Othewise gets the predicted value from glmnet default predicted
  response.

## Value

A `data.frame` containing four columns. The truth independent value
(observed y), the probability to belong in Class1, the probability to
belong Class2 and the predicted class either the plus (use cutoff
threshold) or glmnet model.

## Examples

``` r
model <- lacs(lacsSample[1:200, ])
lacsSample <- get_abstracts(lacsSample)
predictions(model, abstracts = lacsSample)
#> # A tibble: 600 × 4
#>    truth  Class1   Class2 predicted
#>    <fct>   <dbl>    <dbl> <fct>    
#>  1 0     0.0438  9.56e- 1 FALSE    
#>  2 0     0.00367 9.96e- 1 FALSE    
#>  3 1     0.0438  9.56e- 1 FALSE    
#>  4 1     1.000   2.81e-11 TRUE     
#>  5 0     0.0365  9.63e- 1 FALSE    
#>  6 0     0.0405  9.59e- 1 FALSE    
#>  7 1     0.897   1.03e- 1 TRUE     
#>  8 0     0.0438  9.56e- 1 FALSE    
#>  9 0     0.895   1.05e- 1 TRUE     
#> 10 0     0.634   3.66e- 1 FALSE    
#> # ℹ 590 more rows
```
