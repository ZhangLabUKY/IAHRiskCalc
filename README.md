# IAH Clamp-Based Risk Calculator

This Shiny app calculates clamp-based impaired awareness of hypoglycemia (IAH) risk scores from symptom and physiological response values collected at 45 and 90 mg/dL.

## What the app does

- Supports uploaded `.csv`, `.xls`, and `.xlsx` clamp datasets, including canonical wide files and grouped workbook-style files.
- Supports manual entry for a single subject.
- Applies log2 transformation to physiological variables before scoring.
- Requires confirmation before non-positive physiological values are replaced with an offset equal to 80% of that variable's minimum positive raw value in the current scoring dataset.
- Calculates an unadjusted 45 mg/dL score and an adjusted 45-vs-90 score using all 20 variables.
- Labels results as `IAH`, `Likely IAH`, `NAH`, or unable to calculate when required values are missing.
- Provides interactive Plotly plots and downloadable CSV/figure exports.

## Run locally

From the project root:

```r
shiny::runApp()
```

The app entry point is the root `app.R` file. Helper functions live in `R/`, and app styling lives in `www/styles.css`.

## Tests

Run the test suite with the project R installation:

```powershell
C:/Users/ssa390/AppData/Local/Programs/R/R-4.5.3/bin/Rscript.exe -e "testthat::test_dir('tests/testthat')"
```

The tests cover score calculation, upload parsing, preprocessing, plotting/export helpers, app structure, and deployment readiness checks.

## Posit Connect Cloud

This project is intended to deploy as a root-`app.R` Shiny app. Generate the deployment manifest from the project root with:

```powershell
C:/Users/ssa390/AppData/Local/Programs/R/R-4.5.3/bin/Rscript.exe -e "rsconnect::writeManifest()"
```

The generated `manifest.json` should remain at the repository root for Posit Connect Cloud deployment.

## Project metadata

`DESCRIPTION` provides package-like metadata and declares the R packages used by the app. Author and license fields currently use placeholders and should be finalized before public release.
