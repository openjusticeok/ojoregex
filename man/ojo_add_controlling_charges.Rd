% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ojo_regex_controlling_charges.R
\name{ojo_add_controlling_charges}
\alias{ojo_add_controlling_charges}
\title{Add controlling charges to the dataset}
\usage{
ojo_add_controlling_charges(ojo_regex_cats)
}
\arguments{
\item{ojo_regex_cats}{A data frame containing charge information, including
columns \code{max_sentence_first_offense} and \code{max_sentence_any}.}
}
\value{
A data frame with additional columns for sentence classifications
and control ranks.
}
\description{
This function processes a dataset of charges and adds columns to classify
the maximum sentence types and calculate control ranks based on the severity
of the sentences.
}
