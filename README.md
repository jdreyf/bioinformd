
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bioinformd

The goal of bioinformd is to generate R Markdown (RMD) files for
bioinformatic workflows.

## Installation

You can install from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jdreyf/bioinformd")
```

## Example usage

``` r
bioinfo_rmd_contrasts(filename="new_analysis", input.files = c("counts.csv", "pheno.csv"), 
                      contr.v='c(treat="treat-control")')
```
