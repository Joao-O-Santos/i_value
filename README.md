# i-value statistic checker

A small Shiny wrapper around `check_statistic()` for F, t, and chi-square tests.

## Run locally

```r
shiny::runApp()
```

The app uses the shared implementation in `i-value.R`. For Posit Connect Cloud,
`manifest.json` records the R and package dependencies. The free Connect Cloud
plan publishes public applications from public GitHub repositories.
