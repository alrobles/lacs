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
#>    truth Class1     Class2 predicted
#>    <fct>  <dbl>      <dbl> <fct>    
#>  1 0     0.132  0.868      FALSE    
#>  2 0     0.0250 0.975      FALSE    
#>  3 1     0.132  0.868      FALSE    
#>  4 1     1.000  0.00000548 TRUE     
#>  5 0     0.0702 0.930      FALSE    
#>  6 0     0.101  0.899      FALSE    
#>  7 1     0.420  0.580      FALSE    
#>  8 0     0.132  0.868      FALSE    
#>  9 0     0.904  0.0962     TRUE     
#> 10 0     0.132  0.868      FALSE    
#> # ℹ 590 more rows
```
