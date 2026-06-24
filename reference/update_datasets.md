# Update ojoRegex datasets to match the Big Ol Spreadsheet

This function reads the Big Ol Spreadsheet and saves the data to the
package data folder. It's not exported / available to package users
becuase it's intended to be used during development only, so don't run
this unless you're developing the package!!

## Usage

``` r
update_datasets(email = NULL)
```

## Arguments

- email:

  The email address to use for Google authentication.

## Value

A cleaned and categorized dataset with charge descriptions in the
specified column, along with any additional columns present in the
original dataset.

## Examples

``` r
if (FALSE) { # \dontrun{
update_datasets("abell@okpolicy.org")
} # }
```
