
#' Self-report Ethnicity Study Parameters
#'
#' Set global variables for the MEC Ethnicity paper
#' @param main_var Main study categorical variable
#' @param main_var_cols Colors for main study varible
#' @param main_cont Main study continuous variable
#' @param dir_out Output directory
#' @param dir_dta Output directory for data objects
#' @param dir_dwn Output directory for downloadable files
#' @param dir_fig Output directory for figures
#' @param dir_tab Output directory for tables
#' @param study_filter Study indicator for filtering to appropriate samples
#' @param study_title Formal study title
#' @param sg Lowercase study group name
#' @param q_cut FDR cutoffs for phylum and genera
#' @param zhern_cols Colors for Zhernakova categories
#' @param zhern_names Names of Zhernakova categories
#' @param run_code Run scripts during book build
#' @param download_files Create downloadable file links
#'
#' @return Variables are saved to the global environment.
#' @export
study_ethnicity <- function(main_var = "Q1_eth",
                            main_var_cols = viridis::viridis(5),
                            main_cont = "",
                            dir_out = "Output/Ethnicity",
                            dir_dta = file.path(dir_out, "Data"),
                            dir_dwn = file.path(dir_out, "Downloads"),
                            dir_fig = file.path(dir_out, "Figures"),
                            dir_tab = file.path(dir_out, "Tables"),
                            study_filter = "mGWAS",
                            study_title = "Ethnicity",
                            sg = "ethnicity",
                            q_cut = c(phylum = 0.05, genera = 0.05),
                            zhern_cols = viridis::viridis(10),
                            zhern_names = c("Demographics",
                                            "Alcohol",
                                            "Anthropometric measures",
                                            "Special diet",
                                            "Cultural foods & dietary components",
                                            "Dietary quality index",
                                            "Medical conditions",
                                            "Medications",
                                            "Physical activity",
                                            "Smoking"),
                            run_code = FALSE,
                            download_files = TRUE) {
  main_var <<- main_var
  main_var_cols <<- main_var_cols
  main_cont <<- main_cont
  dir_out <<- dir_out
  dir_dta <<- dir_dta
  dir_dwn <<- dir_dwn
  dir_fig <<- dir_fig
  dir_tab <<- dir_tab
  study_filter <<- study_filter
  study_title <<- study_title
  sg <<- sg
  q_cut <<- q_cut
  zhern_cols <<- zhern_cols
  names(zhern_cols) <<- zhern_names

  # Book build
  run_code <<- run_code
  download_files <<- download_files
}

#' Genetic Ancestry Study Parameters
#'
#' Set global variables for the MEC Characterization paper
#' @param main_var Main study categorical variable
#' @param main_var_cols Colors for main study varible
#' @param main_cont Main study continuous variable
#' @param dir_out Output directory
#' @param dir_dta Output directory for data objects
#' @param dir_dwn Output directory for downloadable files
#' @param dir_fig Output directory for figures
#' @param dir_tab Output directory for tables
#' @param study_filter Study indicator for filtering to appropriate samples
#' @param study_title Formal study title
#' @param sg Lowercase study group name
#' @param q_cut FDR cutoffs for phylum and genera
#' @param zhern_cols Colors for Zhernakova categories
#' @param zhern_names Names of Zhernakova categories
#' @param run_code Run scripts during book build
#' @param download_files Create downloadable file links
#'
#' @return Variables are saved to the global environment.
#' @export
study_ancestry <- function(main_var = "anc_max",
                           main_var_cols = c("#A6CEE3", "#1F78B4",
                                             "#B2DF8A", "#33A02C",
                                             "#FB9A99", "#E31A1C",
                                             "#FF7F00",
                                             "#6A3D9A"),
                           main_cont = "",
                           dir_out = "Output/Ancestry",
                           dir_dta = file.path(dir_out, "Data"),
                           dir_dwn = file.path(dir_out, "Downloads"),
                           dir_fig = file.path(dir_out, "Figures"),
                           dir_tab = file.path(dir_out, "Tables"),
                           study_filter = "mGWAS",
                           study_title = "Ancestry",
                           sg = "ancestry",
                           q_cut = c(phylum = 0.05, genera = 0.05),
                           zhern_cols = viridis::viridis(10),
                           zhern_names = c("Demographics",
                                           "Alcohol",
                                           "Anthropometric measures",
                                           "Special diet",
                                           "Cultural foods & dietary components",
                                           "Dietary quality index",
                                           "Medical conditions",
                                           "Medications",
                                           "Physical activity",
                                           "Smoking"),
                           run_code = FALSE,
                           download_files = TRUE) {
  main_var <<- main_var
  main_var_cols <<- main_var_cols
  main_cont <<- main_cont
  dir_out <<- dir_out
  dir_dta <<- dir_dta
  dir_dwn <<- dir_dwn
  dir_fig <<- dir_fig
  dir_tab <<- dir_tab
  study_filter <<- study_filter
  study_title <<- study_title
  sg <<- sg
  q_cut <<- q_cut
  zhern_cols <<- zhern_cols
  names(zhern_cols) <<- zhern_names

  # Book build
  run_code <<- run_code
  download_files <<- download_files
}

#' Liver Fat Study Parameters
#'
#' Set global variables for the MEC Liver Fat paper
#' @param main_var Main study categorical variable
#' @param main_var_cols Colors for main study varible
#' @param main_cont Main study continuous variable
#' @param dir_out Output directory
#' @param dir_dta Output directory for data objects
#' @param dir_dwn Output directory for downloadable files
#' @param dir_fig Output directory for figures
#' @param dir_tab Output directory for tables
#' @param study_filter Study indicator for filtering to appropriate samples
#' @param study_title Formal study title
#' @param study_sent Study title for sentence
#' @param sg Lowercase study group name
#' @param q_cut FDR cutoffs for phylum and genera
#' @param cf bookdown config file
#' @param run_code Run scripts during book build
#' @param download_files Create downloadable file links
#'
#' @return Variables are saved to the global environment.
#' @export
study_liver_fat <- function(main_var = "OMRI_NAFLD_55",
                            main_var_cols = c("dodgerblue", "red"),
                            main_cont = "OMRI_pct_liver_fat_corr",
                            dir_out = "Output/Liver-Fat",
                            dir_dta = file.path(dir_out, "Data"),
                            dir_dwn = file.path(dir_out, "Downloads"),
                            dir_fig = file.path(dir_out, "Figures"),
                            dir_tab = file.path(dir_out, "Tables"),
                            study_filter = "LF",
                            study_title = "Liver Fat",
                            study_sent = "Liver fat",
                            sg = "liver fat",
                            q_cut = c(phylum = 0.05, genera = 0.05),
                            cf = "_bookdown-liver-fat.yml",
                            run_code = FALSE,
                            download_files = TRUE) {
  ggplot2::theme_set(ggplot2::theme_bw())
  main_var <<- main_var
  main_var_cols <<- main_var_cols
  main_cont <<- main_cont
  dir_out <<- dir_out
  dir_dta <<- dir_dta
  dir_dwn <<- dir_dwn
  dir_fig <<- dir_fig
  dir_tab <<- dir_tab
  study_filter <<- study_filter
  study_title <<- study_title
  study_sent <<- study_sent
  sg <<- sg
  q_cut <<- q_cut

  # Book build
  cf <<- cf
  run_code <<- run_code
  download_files <<- download_files
}

#' Pancreas Fat Study Parameters
#'
#' Set global variables for the MEC Pancreas Fat paper
#' @param main_var Main study categorical variable
#' @param main_var_cols Colors for main study varible
#' @param main_cont Main study continuous variable
#' @param dir_out Output directory
#' @param dir_dta Output directory for data objects
#' @param dir_dwn Output directory for downloadable files
#' @param dir_fig Output directory for figures
#' @param dir_tab Output directory for tables
#' @param study_filter Study indicator for filtering to appropriate samples
#' @param study_title Formal study title
#' @param study_sent Study title for sentence
#' @param sg Lowercase study group name
#' @param q_cut FDR cutoffs for phylum and genera
#' @param cf bookdown config file
#' @param run_code Run scripts during book build
#' @param download_files Create downloadable file links
#'
#' @return Variables are saved to the global environment.
#' @export
study_pancreas_fat <- function(main_var = "OMRI_NAFPD_55",
                            main_var_cols = c("dodgerblue", "red"),
                            main_cont = "OMRI_pct_panc_fat_corr",
                            dir_out = "Output/Pancreas-Fat",
                            dir_dta = file.path(dir_out, "Data"),
                            dir_dwn = file.path(dir_out, "Downloads"),
                            dir_fig = file.path(dir_out, "Figures"),
                            dir_tab = file.path(dir_out, "Tables"),
                            study_filter = "PF",
                            study_title = "Pancreas Fat",
                            study_sent = "Pancreas fat",
                            sg = "pancreas fat",
                            q_cut = c(phylum = 0.20, genera = 0.05),
                            cf = "_bookdown-pancreas-fat.yml",
                            run_code = FALSE,
                            download_files = TRUE) {
  ggplot2::theme_set(ggplot2::theme_bw())
  main_var <<- main_var
  main_var_cols <<- main_var_cols
  main_cont <<- main_cont
  dir_out <<- dir_out
  dir_dta <<- dir_dta
  dir_dwn <<- dir_dwn
  dir_fig <<- dir_fig
  dir_tab <<- dir_tab
  study_filter <<- study_filter
  study_title <<- study_title
  study_sent <<- study_sent
  sg <<- sg
  q_cut <<- q_cut

  # Book build
  cf <<- cf
  run_code <<- run_code
  download_files <<- download_files
}

#' Visceral Fat Study Parameters
#'
#' Set global variables for the MEC Visceral Fat paper
#' @param main_var Main study categorical variable
#' @param main_var_cols Colors for main study varible
#' @param main_cont Main study continuous variable
#' @param dir_out Output directory
#' @param dir_dta Output directory for data objects
#' @param dir_dwn Output directory for downloadable files
#' @param dir_fig Output directory for figures
#' @param dir_tab Output directory for tables
#' @param study_filter Study indicator for filtering to appropriate samples
#' @param study_title Formal study title
#' @param study_sent Study title for sentence
#' @param sg Lowercase study group name
#' @param q_cut FDR cutoffs for phylum and genera
#' @param cf bookdown config file
#' @param run_code Run scripts during book build
#' @param download_files Create downloadable file links
#'
#' @return Variables are saved to the global environment.
#' @export
study_visceral_fat <- function(main_var = "OMRI_viscfat_5",
                               main_var_cols = c("dodgerblue", "red"),
                               main_cont = "OMRI_viscfat_avg_corr",
                               dir_out = "Output/Visceral-Fat",
                               dir_dta = file.path(dir_out, "Data"),
                               dir_dwn = file.path(dir_out, "Downloads"),
                               dir_fig = file.path(dir_out, "Figures"),
                               dir_tab = file.path(dir_out, "Tables"),
                               study_filter = "VF",
                               study_title = "Visceral Fat",
                               study_sent = "Visceral fat",
                               sg = "visceral fat",
                               q_cut = c(phylum = 0.05, genera = 0.05),
                               cf = "_bookdown-visceral-fat.yml",
                               run_code = FALSE,
                               download_files = TRUE) {
  ggplot2::theme_set(ggplot2::theme_bw())
  main_var <<- main_var
  main_var_cols <<- main_var_cols
  main_cont <<- main_cont
  dir_out <<- dir_out
  dir_dta <<- dir_dta
  dir_dwn <<- dir_dwn
  dir_fig <<- dir_fig
  dir_tab <<- dir_tab
  study_filter <<- study_filter
  study_title <<- study_title
  study_sent <<- study_sent
  sg <<- sg
  q_cut <<- q_cut

  # Book build
  cf <<- cf
  run_code <<- run_code
  download_files <<- download_files
}

