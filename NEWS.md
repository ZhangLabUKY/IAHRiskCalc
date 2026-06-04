# IAHRiskCalc 0.0.1

- Built the initial IAH clamp-based risk calculator as a Shiny app with manual-entry and uploaded-file workflows.
- Added parsing for uploaded `.csv`, `.xls`, and `.xlsx` clamp files in canonical wide and raw grouped workbook layouts.
- Added dynamic primary scoring: adjusted 45-vs-90 scoring is used when complete 45 and 90 mg/dL data are available, while unadjusted 45 mg/dL scoring is used when complete 45 mg/dL data are available but 90 mg/dL data are unavailable.
- Updated awareness classification so primary scores greater than or equal to the selected cutoff classify as `NAH`, and primary scores below cutoff classify as `IAH`.
- Added patient-facing result cards showing Patient Value, Overall Classification spelled out as impaired or normal awareness of hypoglycemia, and an IAH Risk Prediction gauge.
- Added uploaded-subject result pagination with up to four subjects per tab page, while preserving scored CSV downloads from the Results header.
- Added preprocessing for physiological responses, including log2 transformation, upload per-column offsets for non-positive raw values, and manual-entry paired same-analyte 45/90 offsets.
- Added missing-data handling with `No Imputation` and `Mean imputation` options, with missing 45 mg/dL values blocking scoring unless imputed.
- Added interactive Plotly response-profile and contribution figures, plus static figure exports for PDF, TIFF, SVG, PNG, and JPEG workflows.
- Updated plot behavior so adjusted scoring shows and exports response profile plus contribution plots, while unadjusted-only scoring shows and exports contribution plots only.
- Added a navbar with Calculator, Plots, and Methods tabs, GitHub repository and app website links, and a version label sourced from `DESCRIPTION`.
- Refreshed Methods documentation, visual styling, downloadable figure readability, and functional test coverage for calculations, parsing, preprocessing, plotting, exports, and Shiny server behavior.
- Fixed installed-package checks by declaring `bslib` in package imports and qualifying Shiny tag helpers for R CMD check compatibility.
