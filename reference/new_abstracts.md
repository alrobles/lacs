# Constructor function for the class abstracts A constructor for the abstracts class

Constructor function for the class abstracts A constructor for the
abstracts class

## Usage

``` r
new_abstracts(
  doi = character(),
  title = character(),
  abstract = character(),
  class = character()
)
```

## Arguments

- doi:

  a vector with DOI for each paper

- title:

  A vector of titles for each paper

- abstract:

  A vector of abstracts for each paper

- class:

  A vector with the class for each paper

## Value

an object of class abstracts. Natively is a data.frame

## Examples

``` r
new_abstracts(
doi = example_doi,
title = example_title,
abstract = example_abstract,
class = example_class
)
#> # A tibble: 100 × 4
#>    doi                                title                       abstract class
#>    <chr>                              <chr>                       <chr>    <chr>
#>  1 10.1158/1538-7445.am2013-lb-50     Abstract LB-50: Identifica… abstrac… unkn…
#>  2 10.4208/cicp.280113.190813a        A Numerical Method and its… abstrac… unkn…
#>  3 10.1068/p3471                      Interpersonal Perception i… we comp… unkn…
#>  4 10.21203/rs.2.13179/v1             Activating the interleukin… abstrac… unkn…
#>  5 10.1039/c7ra07556a                 Effect of high DMSO concen… this is… unkn…
#>  6 10.1186/s12890-020-01281-w         LncRNA SNHG10 is downregul… abstrac… unkn…
#>  7 10.1016/s0168-1702(99)00024-6      A new Clethrionomys-derive… Puumala… posi…
#>  8 10.1093/oso/9780190843311.003.0011 Mechanisms of Control       this ch… unkn…
#>  9 10.1177/09691413211067922          Implementation of a centra… objecti… unkn…
#> 10 10.1016/j.virusres.2015.07.022     Detection of different Sou… Hantavi… posi…
#> # ℹ 90 more rows
```
