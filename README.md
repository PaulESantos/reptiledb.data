
<!-- README.md is generated from README.Rmd. Please edit that file -->

# reptiledb.data

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/reptiledb.data)](https://CRAN.R-project.org/package=reptiledb.data)
<!-- badges: end -->

T# 🦎 reptiledb.data: Access the Reptile Database in R

`reptiledb.data` provides easy access to the [Reptile
Database](http://www.reptile-database.org/), a comprehensive global
catalogue of all living reptile species. Developed by **PaulESantos**,
this R package includes a complete snapshot of the database as
ready-to-use R data objects for taxonomy, biodiversity research, and
comparative analyses.

------------------------------------------------------------------------

## 🌍 About the Reptile Database

The [Reptile Database](http://www.reptile-database.org/) is a
volunteer-driven, non-commercial initiative that curates the global
taxonomy of reptiles. It covers all known:

- Snakes 🐍  
- Lizards 🦎  
- Turtles 🐢  
- Amphisbaenians  
- Tuataras  
- Crocodiles 🐊

It currently includes:

- **Over 10,000 species**
- **Approximately 2,800 subspecies**
- Taxonomic hierarchy (orders, families, genera, species, synonyms)
- Distribution and type locality data
- Literature references and nomenclatural information

Although the focus is on taxonomy, future additions may include
ecological and behavioral traits.

------------------------------------------------------------------------

## 📦 Installation

Install the package from CRAN:

``` r
install.packages("reptiledb.data")
```

Or install the development version from GitHub:

``` r
# Using pak
pak::pak("PaulESantos/reptiledb.data")
```

## ⚡ Quick Start

``` r
library(reptiledb.data)

# Load the main dataset
data(reptiledb_012025)

# View the structure of the dataset
str(reptiledb_012025)
#> tibble [14,539 × 13] (S3: tbl_df/tbl/data.frame)
#>  $ order                 : Factor w/ 5 levels "Crocodylia","Rhynchocephalia",..: 3 3 3 3 3 3 3 3 3 3 ...
#>  $ family                : Factor w/ 94 levels "Acrochordidae",..: 75 75 75 75 75 75 75 75 75 75 ...
#>  $ genus                 : Factor w/ 1260 levels "Ablepharus","Abronia",..: 1 1 1 1 1 1 1 1 1 1 ...
#>  $ epithet               : Factor w/ 8631 levels "aaronbaueri",..: 159 159 159 340 1004 1214 1589 1589 1589 1589 ...
#>  $ species               : chr [1:14539] "Ablepharus alaicus" "Ablepharus alaicus" "Ablepharus alaicus" "Ablepharus anatolicus" ...
#>  $ species_author        : Factor w/ 3579 levels "Abalos, Baez & Nader",..: 914 914 914 2890 2054 1117 707 707 707 707 ...
#>  $ species_name_year     : chr [1:14539] "1901" "1901" "1901" "1997" ...
#>  $ subspecies_name       : chr [1:14539] "kucenkoi" "alaicus" "yakovlevae" NA ...
#>  $ subspecie_author_info : chr [1:14539] "NIKOLSKY 1902" "ELPATJEVSKY 1901" "EREMCHENKO 1983" NA ...
#>  $ subspecies_name_author: chr [1:14539] "Nikolsky" "Elpatjevsky" "Eremchenko" NA ...
#>  $ subspecies_year       : chr [1:14539] "1902" "1901" "1983" NA ...
#>  $ change                : Factor w/ 6 levels "changed","new genus (was: Brachyseps)",..: NA NA NA NA NA NA NA NA NA NA ...
#>  $ rdb_sp_id             : num [1:14539] 11256 11256 11256 23791 10001 ...

# Get the total number of records
nrow(reptiledb_012025)
#> [1] 14539

# Count the number of unique accepted species
length(unique(reptiledb_012025$species))
#> [1] 12439
```

## 📖 Citation & Credits

The data included in this package is derived from the Reptile Database,
curated by a network of volunteer experts and guided by a Scientific
Advisory Board. Please cite the database appropriately when using it in
research.

## 🙋 Contribute

This package is maintained by `PaulESantos`. Contributions, issue
reports, and suggestions are welcome via GitHub.
