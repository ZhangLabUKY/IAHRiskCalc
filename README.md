
<!-- README.md is generated from README.Rmd. Please edit that file -->

# IAHRiskCalc

<!-- badges: start -->

[![R-CMD-check](https://github.com/ZhangLabUKY/IAHRiskCalc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ZhangLabUKY/IAHRiskCalc/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of IAHRiskCalc is to calculate clamp-based impaired awareness
of hypoglycemia (IAH) risk scores from symptom and physiological
response values collected at 45 and 90 mg/dL.

## Installation

You can install the development version of IAHRiskCalc from
[GitHub](https://github.com/ZhangLabUKY/IAHRiskCalc) with:

``` r
# install.packages("pak")
pak::pak("ZhangLabUKY/IAHRiskCalc")
```

## What the app does

- Supports uploaded `.csv`, `.xls`, and `.xlsx` clamp datasets,
  including canonical wide files and grouped workbook-style files.
- Supports manual entry for a single subject.
- Applies log2 transformation to physiological variables before scoring.
- Requires confirmation before non-positive physiological values are
  replaced with an offset equal to 80% of that variable’s minimum
  positive raw value in the current scoring dataset.
- Calculates an unadjusted 45 mg/dL score and an adjusted 45-vs-90 score
  using all 20 variables.
- Labels results as `IAH`, `Likely IAH`, `NAH`, or unable to calculate
  when required values are missing.
- Provides interactive Plotly plots and downloadable CSV/figure exports.

## Run locally

From the project root:

``` r
shiny::runApp()
```
