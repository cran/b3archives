
<!-- README.md is generated from README.Rmd. Please edit that file -->
b3archives
==========

<!-- badges: start -->
<!-- badges: end -->
Download and read files from B3 (Brazil Stock Exchange and Over-the-Counter Market). You can find more files and their respective layouts in the [B3 website](http://www.b3.com.br/pt_br/market-data-e-indices/servicos-de-dados/market-data/historico/boletins-diarios/pesquisa-por-pregao/layout-dos-arquivos/).

Installation
------------

You can install the released version of `b3archives` from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("b3archives")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("JHMenegotto/b3archives")
```

Example
-------

Download and read the Index Report (BVBG.087.01):

``` r
library(b3archives)
archive <- b3_indexreport_download(as.Date("2019-09-18"))
df <- b3_indexreport_read(archive)
#> [1] "Stripping node set from file ./BVBG.087.01_BV000335201909180001000000000000003.xml. It can take a long time..."
file.remove(archive)
#> [1] TRUE
dplyr::glimpse(df)
#> Observations: 399
#> Variables: 4
#> $ Date         <date> 2019-09-18, 2019-09-18, 2019-09-18, 2019-09-18, ...
#> $ TickerSymbol <chr> "ICO2", "ISEE", "BDRX", "MLCX", "INDX", "SMLL", "...
#> $ Value        <dbl> 2351.15, 3564.14, 6969.75, 2009.35, 19296.01, 228...
#> $ Attribute    <chr> "OpngPric", "OpngPric", "OpngPric", "OpngPric", "...
```
