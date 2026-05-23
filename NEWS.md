# IAHRiskCalc 0.0.1

- Built the initial IAH clamp-based risk calculator as a root `app.R` Shiny app.
- Added upload and manual-entry Calculator workflows for 45 and 90 mg/dL symptom and physiological response values.
- Added upload parsing for canonical wide files and grouped clamp workbook layouts.
- Added preprocessing for physiological responses, including log2 transformation and confirmed offsets for non-positive values using 80% of the variable-specific minimum positive raw value.
- Added missing-data handling with `No Imputation` and `Mean imputation` options.
- Added unadjusted 45 mg/dL and adjusted 45-vs-90 score calculation with `IAH`, `Likely IAH`, `NAH`, and unable-to-calculate result labels.
- Added interactive Plotly profile and contribution figures, plus static figure exports for PDF, TIFF, SVG, PNG, and JPEG workflows.
- Added Calculator CSV export, polished warning/result table labels, and refreshed Methods documentation.
- Added a clinical research visual theme with UK-blue accents and cleaner Shiny controls.
- Added package-like metadata, Posit Connect manifest support, and test coverage for calculation, parsing, preprocessing, plotting, exports, app structure, and deployment readiness.
