# IAHRiskCalc

The goal of IAHRiskCalc is to classify clamp-based impaired awareness of
hypoglycemia (IAH) status from symptom and physiological response values
collected at 45 and 90 mg/dL.

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
- Classifies scores greater than or equal to their thresholds as normal
  awareness (`NAH`) and scores below threshold as impaired awareness.
- Labels results as `IAH` when both scores are below threshold,
  `Likely IAH` when exactly one score is below threshold, `NAH` when
  both scores meet or exceed threshold, or unable to calculate when
  required values are missing.
- Provides interactive Plotly plots and downloadable CSV/figure exports.

## Run locally

From the project root:

``` r

shiny::runApp()
```
