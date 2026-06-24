# Pre clean charge descriptions to be matched

This function pre-cleans charge descriptions to be matched by removing
specific patterns that are not relevant for matching. It removes phrases
like "in concert with" from the end of the charge descriptions.

## Usage

``` r
regex_pre_clean(count_as_filed)
```

## Arguments

- count_as_filed:

  A character vector containing the charge descriptions to be
  pre-cleaned.

## Value

A character vector with pre-cleaned charge descriptions.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example usage
clean_text <- regex_pre_clean("TAXS, FAIL TO DISPLAY TAX STAMP ON CDS IN CONCERT W/J POOLE")
clean_text
} # }
```
