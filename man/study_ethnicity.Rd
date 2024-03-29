% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/study-parameters.R
\name{study_ethnicity}
\alias{study_ethnicity}
\title{Self-report Ethnicity Study Parameters}
\usage{
study_ethnicity(main_var = "Q1_eth",
  main_var_cols = viridis::viridis(5), main_cont = "",
  dir_out = "Output/Ethnicity", dir_dta = file.path(dir_out, "Data"),
  dir_dwn = file.path(dir_out, "Downloads"),
  dir_fig = file.path(dir_out, "Figures"), dir_tab = file.path(dir_out,
  "Tables"), study_filter = "mGWAS", study_title = "Ethnicity",
  sg = "ethnicity", q_cut = c(phylum = 0.05, genera = 0.05),
  zhern_cols = viridis::viridis(10), zhern_names = c("Demographics",
  "Alcohol", "Anthropometric measures", "Special diet",
  "Cultural foods & dietary components", "Dietary quality index",
  "Medical conditions", "Medications", "Physical activity", "Smoking"),
  run_code = FALSE, download_files = TRUE)
}
\arguments{
\item{main_var}{Main study categorical variable}

\item{main_var_cols}{Colors for main study varible}

\item{main_cont}{Main study continuous variable}

\item{dir_out}{Output directory}

\item{dir_dta}{Output directory for data objects}

\item{dir_dwn}{Output directory for downloadable files}

\item{dir_fig}{Output directory for figures}

\item{dir_tab}{Output directory for tables}

\item{study_filter}{Study indicator for filtering to appropriate samples}

\item{study_title}{Formal study title}

\item{sg}{Lowercase study group name}

\item{q_cut}{FDR cutoffs for phylum and genera}

\item{zhern_cols}{Colors for Zhernakova categories}

\item{zhern_names}{Names of Zhernakova categories}

\item{run_code}{Run scripts during book build}

\item{download_files}{Create downloadable file links}
}
\value{
Variables are saved to the global environment.
}
\description{
Set global variables for the MEC Ethnicity paper
}
