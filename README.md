
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ctrecurrent

The goal of ctrecurrent is to transform the camera trap data into a
format suitable for recurrent event analysis. It contains the function
`ct_to_recurrent` to do so, requiring a dataframe with the following
information for each observation:

- Site ID,
- Timestamp (Date and Time) and
- Species

For full details and methodological background, please see the associated paper: [Ferry et al. (2025) Recurrent Event Analyses for Species Interactions](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.14338)

## Installation

You can install the development version of ctrecurrent from the R-cran
or from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("adibender/ctrecurrent")
```
