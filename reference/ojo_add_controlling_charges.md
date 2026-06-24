# Add controlling charges to the dataset

This function processes a dataset of charges and adds columns to
classify the maximum sentence types and calculate control ranks based on
the severity of the sentences.

## Usage

``` r
ojo_add_controlling_charges(ojo_regex_cats)
```

## Arguments

- ojo_regex_cats:

  A data frame containing charge information, including columns
  `max_sentence_first_offense` and `max_sentence_any`.

## Value

A data frame with additional columns for sentence classifications and
control ranks.
