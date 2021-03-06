% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/feature_plots_chunk.R
\name{feature_plots_chunk}
\alias{feature_plots_chunk}
\title{Feature plots chunk}
\usage{
feature_plots_chunk(
  grp.var = "grp",
  path,
  proj.nm,
  contr.v,
  use_annot = TRUE,
  row.type = "gene",
  elst = FALSE
)
}
\arguments{
\item{grp.var}{Variable name in \code{pheno} for group.}

\item{path}{Path of RMD.}

\item{proj.nm}{Name of project to paste into filenames.}

\item{contr.v}{Named vector of contrasts.}

\item{use_annot}{Should \code{annot} be used.}

\item{row.type}{Character in filename for plots.}

\item{elst}{Logical indicating if expression object an EList.}
}
\description{
Feature plots chunk.
}
