# SCAMP Data Dictionary — Shiny prototype v0.1

This prototype treats the Excel workbook as the maintained source of truth.

## 1. Files

- `app.R` — Shiny application.
- `data/SCAMP_data_dictionary.xlsx` — current copy of the working SCAMP dictionary.

## 2. Required R packages

Run once in R:

```r
install.packages(c(
  "shiny",
  "bslib",
  "readxl",
  "dplyr",
  "DT"
))
```

## 3. Run locally

Open `app.R` in RStudio and click **Run App**, or run:

```r
shiny::runApp()
```

from the project folder.

## 4. Updating the dictionary

### Simple approach

Keep the workbook filename as:

`data/SCAMP_data_dictionary.xlsx`

and replace/update that file.

While the app is running, it checks the workbook every 5 seconds and re-reads
it when the file modification time changes.

### Recommended shared-drive approach

If the maintained SCAMP dictionary is stored on a shared/network drive, point
the app to that file before running it:

```r
Sys.setenv(
  SCAMP_DICT_PATH = "X:/SCAMP/Data Dictionary/SCAMP_data_dictionary.xlsx"
)

shiny::runApp()
```

This avoids maintaining a second Excel copy.

For a permanent deployment, the environment variable can be configured on the
hosting server rather than written into `app.R`.

## 5. Current prototype functions

- Free-text search across variable name, question/description, coding, notes,
  skip logic and topics.
- Filters for instance, cohort, top level, Level 1 topic, Level 2 topic and
  data type.
- Collection labels derived from the workbook's `README` sheet.
- Full metadata display for a selected variable.
- First-pass cross-wave matching based on identical `Description` text.
- References page.
- Export of filtered results to CSV.
