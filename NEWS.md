# ojoregex 0.6.1

* Made `update_datasets()` non-exported, since it's intended to be used only during development.

# ojoregex 0.6.0

* Changed the appendix from a vignette to an article in the `ojoregex` package documentation.
* Added `update_datasets()` function to update the datasets in the package from the Big Ol Regex Spreadsheet.

# ojoregex 0.5.1

* Fixed a bug with the `violent_crimes_list` column not being included in the returned result.

# ojoregex 0.5.0

* Added `violent_crimes_list` column to the result returned by `ojoregex::ojo_apply_regex(.include_cats = TRUE)`. It is `TRUE` for charges that are included in the violent crimes list (see [here](https://oklahoma.gov/content/dam/ok/en/able-commission/documents/Felony%20offenses%20violent%20crimes.pdf)), and `FALSE` otherwise.
