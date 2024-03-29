% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model-helpers.R
\name{get_lm}
\alias{get_lm}
\title{Get lm()}
\usage{
get_lm(df, y, x, adj, fm, ...)
}
\arguments{
\item{df}{source data.frame}

\item{y}{response variable}

\item{x}{variable of interest}

\item{adj}{adjustment varaiables}

\item{fm}{MEC filter}

\item{...}{Arguments passed to lm()}
}
\value{
list with tidy(), glance(), and lrtest() results
}
\description{
Run custom lm()
}
