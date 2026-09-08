# IAHRiskCalc

The goal of IAHRiskCalc is to calculate study-derived lower- and
higher-response clamp phenotypes from symptom and physiological response
values collected at 45 and 90 mg/dL. Their correspondence to clinical
awareness status is provisional.

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
- Reports scores greater than or equal to their thresholds as a
  higher-response phenotype, with provisional correspondence to `NAH`,
  and scores below threshold as a lower-response phenotype, with
  provisional correspondence to `IAH`.
- Keeps the study’s internal `IAH`/`NAH` coding while presenting
  phenotype-first public results and CSV exports.
- Accepts numeric symptom values, including decimals, without rounding.
- Provides interactive Plotly plots and downloadable CSV/figure exports.

## Run locally

From the project root:

``` r

shiny::runApp()
```
