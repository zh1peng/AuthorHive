% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_title_page.R
\name{generate_title_page}
\alias{generate_title_page}
\title{Generate a Title Page}
\usage{
generate_title_page(
  data,
  style = c("default", "APA", "Nature"),
  title = NULL,
  co_first_footnote = TRUE
)
}
\arguments{
\item{data}{A data frame containing at least: FirstName, MiddleName, LastName,
(optionally) Rank, Correspondence, and one or more Affiliation* columns.}

\item{style}{A character string specifying the style (e.g., "default", "APA", "Nature").}

\item{title}{An optional paper title.}

\item{co_first_footnote}{Logical. If TRUE and Rank is provided, adds a note for co-first authors.}
}
\value{
A character string with the formatted title page.
}
\description{
Produces a formatted title page that includes the paper title,
author names with affiliation indices, and an affiliation legend. Optionally adds
a note for co-first authors.
}
\examples{
authors <- data.frame(
  FirstName = c("Alice", "Bob"),
  MiddleName = c("M.", ""),
  LastName = c("Smith", "Johnson"),
  Degree = c("PhD", "MD"),
  Email = c("alice@example.com", "bob@example.com"),
  Rank = c(1, 2),
  Correspondence = c(TRUE, FALSE),
  Affiliation1 = c("University of X, Dept. of Y", "University of X, Dept. of Y"),
  Affiliation2 = c(NA, "Company Z, Research Division"),
  stringsAsFactors = FALSE
)
generate_title_page(authors, style = "default", title = "Example Paper")
}
