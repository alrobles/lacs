# Constructor function for the class lacs. A constructor for the lacs class

Constructor function for the class lacs. A constructor for the lacs
class

## Usage

``` r
new_lacs(
  plus = plus::new_plus(),
  vocabulary = data.table::data.table(),
  train = data.frame(),
  call = character()
)
```

## Arguments

- plus:

  An object of the class plus. Is the fit of the model

- vocabulary:

  An object of the class `text2vec_vocabulary`.

- train:

  An abstracts sample dataset used to train a plus model.

- call:

  The call of the function

## Value

an object of class lacs.

## Examples

``` r
new_lacs(
plus = example_plus,
vocabulary = example_vocabulary,
train = example_train,
call = character()
)
#> $plus
#> 
#> Call:  glmnet::cv.glmnet(x = train.X, y = y, family = "binomial") 
#> 
#> Measure: Binomial Deviance 
#> 
#>       Lambda Index Measure      SE Nonzero
#> min 0.003337    86  0.3711 0.06808     117
#> 1se 0.016998    51  0.4371 0.05312      82
#> 
#> $vocabulary
#> Number of docs: 600 
#> 175 stopwords: i, me, my, myself, we, our ... 
#> ngram_min = 1; ngram_max = 5 
#> Vocabulary: 
#>               term term_count doc_count
#>             <char>      <int>     <int>
#>     1: (comprising          2         1
#>     2:      (dobv)          2         2
#>     3:      (hfrs)          2         2
#>     4:        (nt)          2         1
#>     5:   (rt-pcr).          2         2
#>    ---                                 
#> 15577:     species        177        82
#> 15578:       using        199       144
#> 15579:     results        265       192
#> 15580:    patients        290        69
#> 15581:       study        292       191
#> 
#> $train
#> # A tibble: 400 × 4
#>    doi                                     title                  abstract class
#>    <chr>                                   <chr>                  <chr>    <chr>
#>  1 10.12737/2305-7807-2021-10-5-63-68      Adapting the Swedish … the cor… unkn…
#>  2 10.1139/e74-096                         Geophysical Studies o… this st… unkn…
#>  3 10.1098/rsif.2009.0228.focus            Control of airborne i… we prot… unkn…
#>  4 10.1108/eum0000000003768                The Blacksburg Electr… describ… unkn…
#>  5 10.1645/ge-110r                         MOLECULAR AND BIOLOGI… toxopla… poss…
#>  6 10.1108/oxan-db252614                   COVID-19 will further… subject… unkn…
#>  7 10.2118/196230-ms                       Oil Saturation in Res… abstrac… unkn…
#>  8 10.5204/lthj.1593                       Wonder Woman: An Asse… this pa… unkn…
#>  9 10.31967/mba.v4i2.508                   THE EFFECT OF SERVICE… this st… unkn…
#> 10 10.1146/annurev-economics-080213-041435 Beyond Ricardo: Assig… interna… unkn…
#> # ℹ 390 more rows
#> 
#> $call
#> character(0)
#> 
#> attr(,"class")
#> [1] "lacs"
```
