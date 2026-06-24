# Apply OJO Regex

This function applies regular expressions patterns to clean and
categorize charge descriptions in a given dataset.

## Usage

``` r
ojo_apply_regex(
  data,
  col_to_clean = "count_as_filed",
  .keep_flags = FALSE,
  .include_cats = TRUE,
  .quiet = FALSE
)
```

## Arguments

- data:

  A data frame containing the dataset to be processed.

- col_to_clean:

  The name of the column in the dataset containing the charge
  descriptions to be cleaned and categorized.

- .keep_flags:

  Logical value indicating whether to keep the concept flags generated
  during processing. Defaults to FALSE, which returns only the cleaned
  dataset without the flags.

- .include_cats:

  Logical value indiciating whether the categories / subcategories
  should be included in the returned data

- .quiet:

  Should the progress bar be shown?

## Value

A cleaned and categorized dataset with charge descriptions in the
specified column, along with any additional columns present in the
original dataset.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load example dataset
data(example_data)

# Apply OJO Regex to clean and categorize charge descriptions
cleaned_data <- apply_ojo_regex(data = example_data, col_to_clean = "charge_description")
} # }
```
