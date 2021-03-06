% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/limma_contrasts_chunk.R
\name{limma_contrasts_chunk}
\alias{limma_contrasts_chunk}
\title{Limma contrasts chunk}
\usage{
limma_contrasts_chunk(
  grp.var,
  contr.v,
  path,
  proj.nm,
  limma.model = NULL,
  use_aw = TRUE,
  use_trend = FALSE,
  use_annot = TRUE,
  row.type = "gene",
  elst = FALSE
)
}
\arguments{
\item{grp.var}{Variable name in \code{pheno} for group.}

\item{contr.v}{Named vector of contrasts.}

\item{path}{Path of RMD.}

\item{proj.nm}{Name of project to paste into filenames.}

\item{limma.model}{Model formula for \code{arrayWeights}.}

\item{use_aw}{Logical indicating if array weights should be used.}

\item{use_trend}{Logical indicating if \code{limma trend} should be used.}

\item{use_annot}{Should \code{annot} be used.}

\item{row.type}{Character in filename for features.}

\item{elst}{Logical indicating if expression object an EList.}
}
\description{
Limma contrasts chunk.
}
