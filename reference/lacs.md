# Create an object of class lacs

Create an object of class lacs

## Usage

``` r
lacs(
  abstracts,
  vocabulary = NULL,
  term_count_min = 2,
  doc_proportion_min = 0,
  doc_proportion_max = 1,
  alpha = 1,
  sample_use_time = 30,
  learning_rate = 1,
  qq = 0.1,
  tf_idf = TRUE
)
```

## Arguments

- abstracts:

  Object of `abstracts` class. A dataset of abstracts

- vocabulary:

  A vocabulary created with the `get_vocabulary` function, from an
  `abstracts` object

- term_count_min:

  Vocabulary parameter. The minimum number of counts for an specific
  term in vocabulary

- doc_proportion_min:

  Vocabulary parameter. The minimum proportion of documents which should
  contain term

- doc_proportion_max:

  The Vocabulary parameter. The maximum proportion of documents which
  should contain term.

- alpha:

  Plus parameter. The elastic net mixing parameter, with \\0\le\alpha\le
  1\\

- sample_use_time:

  Plus parameter. Number of use time.

- learning_rate:

  Plus parameter. Learning rate. Default is 1.

- qq:

  Plus parameter. quantile threshold

- tf_idf:

  Logical. Default is true, use a tf_idf in the document - term matrix S

## Value

An object of class lacs

## Examples

``` r
lacs(lacsSample, tail(example_vocabulary, 500))
#> $plus
#> 
#> Call:  glmnet::cv.glmnet(x = train.X, y = y, family = "binomial") 
#> 
#> Measure: Binomial Deviance 
#> 
#>       Lambda Index Measure      SE Nonzero
#> min 0.007635    35  0.4413 0.05904      84
#> 1se 0.028083    21  0.4967 0.03559      49
#> 
#> $vocabulary
#> Number of docs: 600 
#> 175 stopwords: i, me, my, myself, we, our ... 
#> ngram_min = 1; ngram_max = 5 
#> Vocabulary: 
#>              term term_count doc_count
#>            <char>      <int>     <int>
#>   1: relationship         22        20
#>   2:         skin         22         8
#>   3:      spatial         22        15
#>   4:       strain         22        15
#>   5:        terms         22        20
#>  ---                                  
#> 496:      species        177        82
#> 497:        using        199       144
#> 498:      results        265       192
#> 499:     patients        290        69
#> 500:        study        292       191
#> 
#> $train
#> # A tibble: 600 × 4
#>    doi                          title                             abstract class
#>    <chr>                        <chr>                             <chr>    <chr>
#>  1 10.15294/jlj.v9i3.39705      KEEFEKTIFAN MODEL DISCOVERY LEAR… abstrak… unkn…
#>  2 10.1186/s13643-021-01790-7   Enrollment, retention, and strat… abstrac… unkn…
#>  3 10.1016/j.jcpa.2009.05.003   A Pathological Study of Sepsis A… the pat… posi…
#>  4 10.7589/0090-3558-45.2.502   Clinical Demodicosis in African … we inve… posi…
#>  5 10.3390/ijerph17176225       Comparing the Trail Users with T… backgro… unkn…
#>  6 10.3390/app12126130          Evaluation of the Effectiveness … 1 backg… unkn…
#>  7 10.1002/ijc.2910380412       STLV-I antibodies in feral popul… serum s… posi…
#>  8 10.1177/0954406220903743     Multiobjective optimization of a… in this… unkn…
#>  9 10.31703/giidr.2021(vi-i).02 Management of COVID-19 in Differ… covid 1… unkn…
#> 10 10.1155/2022/1775190         Serum Cystatin, Chemokine, and G… objecti… unkn…
#> # ℹ 590 more rows
#> 
#> $call
#> lacs(abstracts = lacsSample, vocabulary = tail(example_vocabulary, 
#>     500))
#> 
#> attr(,"class")
#> [1] "lacs"
```
