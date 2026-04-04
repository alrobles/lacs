# Title

Title

## Usage

``` r
# S3 method for class 'abstracts'
print(x, ...)
```

## Arguments

- x:

  Object of the abstracts type to print.

- ...:

  Passed on to \[tbl_format_setup()\].#'

## Value

An object with a \`print()\` method that will print the input similarly
to a tibble.

## Examples

``` r
print(get_abstracts(lacsSample))
#> # A tibble: 600 × 4
#>    doi                          title                             abstract class
#>    <chr>                        <chr>                             <chr>    <chr>
#>  1 10.15294/jlj.v9i3.39705      KEEFEKTIFAN MODEL DISCOVERY LEAR… abstrak… unkn…
#>  2 10.1186/s13643-021-01790-7   Enrollment, retention, and strat… abstrac… unkn…
#>  3 10.1016/j.jcpa.2009.05.003   A Pathological Study of Sepsis A… the pat… poss…
#>  4 10.7589/0090-3558-45.2.502   Clinical Demodicosis in African … we inve… poss…
#>  5 10.3390/ijerph17176225       Comparing the Trail Users with T… backgro… unkn…
#>  6 10.3390/app12126130          Evaluation of the Effectiveness … 1 backg… unkn…
#>  7 10.1002/ijc.2910380412       STLV-I antibodies in feral popul… serum s… poss…
#>  8 10.1177/0954406220903743     Multiobjective optimization of a… in this… unkn…
#>  9 10.31703/giidr.2021(vi-i).02 Management of COVID-19 in Differ… covid 1… unkn…
#> 10 10.1155/2022/1775190         Serum Cystatin, Chemokine, and G… objecti… unkn…
#> # ℹ 590 more rows
```
