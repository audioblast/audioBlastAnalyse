% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyse.R
\name{analyse}
\alias{analyse}
\title{Run all analyses}
\usage{
analyse(
  db,
  sense = "web",
  verbose = FALSE,
  force = FALSE,
  base_dir = "",
  cores = 1,
  reverse = FALSE,
  shuffle = FALSE,
  checkFile = NULL
)
}
\arguments{
\item{db}{database connector}

\item{sense}{"web" for online files, or name of source to analyse locally}

\item{verbose}{Gives verbose output if TRUE}

\item{force}{Forces recalculation of analyses if TRUE}

\item{base_dir}{Directory relative paths are located in}

\item{cores}{Number of cores to use}

\item{reverse}{Reverses the data frame of recordings to analyse}

\item{shuffle}{Shuffles rows before analysing}

\item{checkFile}{Provide a filename to check for debugging purposes}
}
\description{
Run all analyses
}
