<!-- README.md is generated from README.Rmd. Please edit that file -->

# 🦎 reptiledb.data: Access the Reptile Database in R

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN status](https://www.r-pkg.org/badges/version/reptiledb.data)](https://CRAN.R-project.org/package=reptiledb.data)
[![](http://cranlogs.r-pkg.org/badges/grand-total/reptiledb.data?color=green)](https://cran.r-project.org/package=reptiledb.data)
[![](http://cranlogs.r-pkg.org/badges/last-week/reptiledb.data?color=green)](https://cran.r-project.org/package=reptiledb.data)
<!-- badges: end -->

`reptiledb.data` provides easy access to [The Reptile Database](http://www.reptile-database.org/), a comprehensive global catalogue of all living reptile species. Developed by **PaulESantos**, this R package includes curated snapshots of the database as ready-to-use R data objects for taxonomy, biodiversity research, and comparative analyses, along with automated utilities to check and fetch online updates.

---

## 🌍 About the Reptile Database

The [Reptile Database](http://www.reptile-database.org/) is a volunteer-driven, non-commercial initiative that curates the global taxonomy of reptiles. It covers all known:

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

---

## 📦 Installation

Install the package from CRAN:

```r
install.packages("reptiledb.data")
```

Or install the development version from GitHub:

```r
# Using pak
pak::pak("PaulESantos/reptiledb.data")
```

---

## ⚡ Quick Start

### 1. Using Included Snapshots

The package comes pre-packaged with structured datasets:

- `reptiledb_062026` (Latest snapshot - June 2026, 14,719 entries)
- `reptiledb_092025` (September 2025 snapshot, 14,585 entries)
- `reptiledb_012025` (January 2025 snapshot, 14,539 entries)

```r
library(reptiledb.data)

# Load the latest included dataset snapshot
data(reptiledb_062026)

# View summary structure
str(reptiledb_062026)
#> tibble [14,719 × 13] (S3: tbl_df/tbl/data.frame)

# Total records
nrow(reptiledb_062026)
#> [1] 14719

# Unique accepted species
length(unique(reptiledb_062026$species))
```

### 2. Checking for Online Updates

You can check if a newer checklist has been published on [The Reptile Database](http://www.reptile-database.org/data/):

```r
# Check if a new version is available online
check_data_update()
```

### 3. Fetching the Latest Online Data Automatically

Fetch and clean the latest live online checklist from The Reptile Database server:

```r
# Download, clean, and cache the latest database online
latest_data <- fetch_latest_reptile_data(cache = TRUE)
head(latest_data)
```

---

## 🤖 Automated Database Updates

This package features an automated maintainer workflow via GitHub Actions that runs monthly. It checks for new releases on The Reptile Database server, automatically downloads and cleans raw files, creates a new snapshot dataset, updates package documentation, and opens a Pull Request.

---

## 📖 Citation & Credits

The data included in this package is derived from The Reptile Database, curated by a network of volunteer experts and guided by a Scientific Advisory Board. Please cite the database appropriately when using it in research:

> Uetz, P., Freed, P., Aguilar, R., Reyes, F., Kudera, J. & Hošek, J. (eds.) (2026) The Reptile Database, http://www.reptile-database.org.

---

## 🙋 Contribute

This package is maintained by `PaulESantos`. Contributions, issue reports, and suggestions are welcome via [GitHub Issues](https://github.com/PaulESantos/reptiledb.data/issues).
