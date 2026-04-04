# Dataset with 710 abstracts originally selected by the hp3 project. These papers were used to obtain mammal-virus interactions, aligned and harmonized taxonomically by Rory Gibb of the clover project. Rory Gibb curated the hp3 original dataset and added doi and pmid. We retrived the abstracts through entrez API in PubMed using pmid. Is primarily used to test a lacs model as a benchmark in classification. We use true possitive rate metric (precision) to classify this dataset. We consider all these articles as positive class papers because they contain information on parasite-host interactions. Our lacs model is trained to find papers with this information. The original dataset is stored in 10.5281/zenodo.596810 and the clover curated dataset was retrived from 10.5281/zenodo.4435127

Dataset with 710 abstracts originally selected by the hp3 project. These
papers were used to obtain mammal-virus interactions, aligned and
harmonized taxonomically by Rory Gibb of the clover project. Rory Gibb
curated the hp3 original dataset and added doi and pmid. We retrived the
abstracts through entrez API in PubMed using pmid. Is primarily used to
test a lacs model as a benchmark in classification. We use true
possitive rate metric (precision) to classify this dataset. We consider
all these articles as positive class papers because they contain
information on parasite-host interactions. Our lacs model is trained to
find papers with this information. The original dataset is stored in
10.5281/zenodo.596810 and the clover curated dataset was retrived from
10.5281/zenodo.4435127

## Usage

``` r
data(hp3_abstracts)
```

## Format

An object of class `"data.frame"`;

## Examples

``` r
data(hp3_abstracts)
```
