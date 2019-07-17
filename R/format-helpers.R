
#' Replace "<="
#'
#' Uses `gsub()` to replace "<=" to the unicode symbol.
#' @param x String to modify.
#'
#' @return String with unicode symbol for "<="
#' @export
gsub_leq <- function(x) gsub("<=", "\u2264", x)

#' Replace ">="
#'
#' Uses `gsub()` to replace ">=" to the unicode symbol.
#' @param x String to modify.
#'
#' @return String with unicode symbol for ">="
#' @export
gsub_geq <- function(x) gsub(">=", "\u2265", x)

#' Return pretty field name
#'
#' Returns the formatted name of various MEC data fields.
#' @param x Field to format
#'
#' @return The pretty field name
#' @export
mec_field <- function(x) {
  fields <- c(
    batch = "Batch",
    batch_correct = "Batch correction",
    OST_sample_age = "Age (years)",
    Q1_CORR_SEX = "Sex",
    Q1_eth = "Ethnicity",
    Japanese = "Japanese American",
    Black = "African American",
    White = "White",
    Latino = "Latino",
    Hawaiian = "Native Hawaiian",
    Q1_NEWHT_CORR = "Height",
    Q3_wtpd = "Weight",
    Q3_HIBPY = "Blood pressure",
    Q1_smokstat = "Smoking status",
    Q1_smkyrs = "Smoking years",
    Q1_packyrs = "Smoking packyears",
    Q1_calories = "Calories (Kcal/day)",
    Q3_dfiber = "Dietary fiber (g/day)",
    Q3_choleste = "Cholesterol (g/day)",
    Q3_nsp = "Non starch polysaccharides (g/day)",
    Q3_carbohyd = "Carbohydrate (g/day)",
    Q3_ethanol = "Ethanol (g/day)",
    Q3_fat = "Fat (g/day)",
    Q1_DP_indices_AMDSE_score = "Q1 Dietary Patterns Alternate Mediterranean Diet, Energy Adjusted score",
    Q3_DP_AMDS_E_TOTSCORE = "Q3 Dietary Patterns Alternate Mediterranean Diet, Energy Adjusted score",
    Q1_POB = "Place of birth",
    Q1_MBORN = "Birthplace of mother",
    Q1_FBORN = "Birthplace of father",
    Q1_numbro = "Number of brothers, Categorical",
    Q1_numsis = "Number of sisters, Categorical",
    Q1_yrsschl_c = "Education (years)",
    fat_kcal_day = "Fat (kcal/day)",
    pct_unsat_fat_day = "Unsaturated fat (% Fat intake/day)",
    pct_sat_fat_day = "Saturated fat (% Fat intake/day)",
    fiber_kcal_day = "Fiber (kcal/day)",
    carboh_kcal_day = "Carbohydrates (kcal/day)",
    protein_kcal_day = "Protein (kcal/day)",
    OQ3_DP_AHEI2010_TOTSCORE = "HEI",
    BAI = "BAI",
    OQ3_dfiber = "Fiber (g/day)",
    OQ3_nsp = "NSP (g/day)",
    OST_antibx = "Antibiotic use? (past year)",
    OST_antifung = "Antifungal use? (past year)",
    bristol_score = "Bristol Score",
    bristol_score_cat = "Bristol Score (cat)",
    O_LBP = "LBP (ng/mL)",
    O_cyto_IL6 = "IL-6 (pg/mL)",
    O_hsCRP = "CRP (mg/L)",
    O_insulin = "Insulin (microU/mL)",
    O_glucose = "Glucose (mg/dL)",
    O_ALT = "ALT (U/L)",
    ODXA_pfat_tot_corr = "% Total adiposity",
    ODXA_pfat_trunk = "% Trunk fat",
    OCL_meds_antiviral = "Antiviral use? (past 2 weeks)",
    Q1_ethanol = "Q1 Ethanol (g/day)",
    OQ3_ethanol = "Q3 Ethanol (g/day)",
    OQ3_fat = "Fat (g/day)",
    OQ3_secoiso = "Secoisolariciresinol (UNITS/day)",
    OQ3_diabet = "Diabetes",
    O_IGFBP2 = "IGFBP2 (ng/mL)",
    O_HOMA_IR = "HOMA IR",
    O_MetS_glucose = "MetS Glucose",
    O_MetS_BP = "Elevated BP",
    O_MetS_TG = "Elevated TG",
    O_TG = "Triglycerides (mg/dL)",
    OSNP_rs738409 = "PNPLA3",
    O_adiponectin = "Adiponectin (ng/mL)",
    O_TMAO = "TMAO (microM/L)",
    O_TMAO_choline = "TMAO choline (microM/L)",
    O_ba_CA = "CA (ng/mL)",
    O_ba_CDCA = "CDCA (ng/mL)",
    O_ba_DCA = "DCA (ng/mL)",
    O_ba_LCA = "LCA (ng/mL)",
    O_ba_UDCA = "UDCA (ng/mL)",
    O_ba_HDCA = "HDCA (ng/mL)",
    O_uric_acid = "Uric Acid (microM/L)",
    O_FGF21 = "Fibroblast Growth Factor (ng/mL)",
    OQ3_choline = "Choline (mg/day)",
    O_TMAO_betaine = "TMAO betaine (microM/L)",
    O_TMAO_carnitine = "TMAO carnitine (microM/L)",
    shannon = "Shannon diversity",
    pd_whole_tree = "PD whole tree",
    chao1 = "Chao 1",
    batch = "Batch",
    batch_correct = "Batch correction group",
    all_bacteria = "All bacteria copy number",
    bifido = "Bifido copy number",
    msmithii = "Msmithii copy number",
    msmithii_percent = "% Msmithii of All bacteria",
    msmithii_prevalence = "Sample has Msmithii",
    OST_sample_BMI = "BMI",
    OCL_anthro_waist_hip = "Waist/hip",
    ODXA_fat_tot_corr = "Total adiposity (kg)",
    ODXA_fat_trunk_limb = "Trunk to limb ratio",
    OMRI_viscfat_avg_corr = "Visc. fat (Avg. of L1-L5)",
    OMRI_subcfat_avg_corr = "Subc. fat (Avg. of L1-L5)",
    OMRI_viscfat_subcfat_avg_corr = "Visc./Subc. fat (Avg. of L1-L5)",
    OMRI_pct_viscfat_avg_corr = "% Visc. fat (Avg. of L1-L5)",
    OMRI_pct_subcfat_avg_corr = "% Subc. fat (Avg. of L1-L5)",
    OMRI_pct_liver_fat_corr = "% Liver fat",
    `log(OMRI_pct_liver_fat_corr)`  = "log(% Liver fat)",
    OMRI_pct_panc_fat_corr = "% Pancreas fat",
    `log(OMRI_pct_panc_fat_corr)`  = "log(% Pancreas fat)",
    OQ3_ac_mets = "METS activity (per day)",
    genus = "Genus",
    genus_id = "Genus",
    phylum = "Phylum",
    count_rar = "Rarified bug counts",
    OMRI_NAFLD_55 = "Liver fat groups",
    OMRI_NAFLD_5 = "Liver fat groups",
    OMRI_NAFPD_55 = "Pancreas fat groups",
    OMRI_NAFPD_5 = "Pancreas fat groups",
    rs738409_geno = "PNPLA3",
    rs77249491_geno = "Hepatic risk loci"
  )

  if (all(x %in% names(fields))) return(fields[x])

  warning("Missing formal name(s)")
  out <- fields[x]
  names(out) <- x
  out[is.na(out)] <- x[is.na(out)]
  return(out)
}

#' Collapse Vector to Sentence
#'
#' Collapse vector of strings to formal sentence.
#' @param x Strings to collapse.
#'
#' @return String with appropriate commas/and.
#' @export
collapse_vec <- function(x) {
  if (length(x) <= 1) return(x)

  out <- if (length(x) == 2) {
    paste(x, collapse = " and ")
  } else if (length(x) >= 2) {
    paste0(paste(x[-length(x)], collapse = ", "),
           ", and ", tail(x, 1))
  } else NULL

  out
}

#' Preview Tibble
#'
#' Shows a preview of a tibble/data.frame.
#' @param x Data to preview.
#' @param cols Number of column to preview.
#' @param rows Number of rows to preview.
#' @param ... Additional arguments sent to knitr::kable
#'
#' @return knitr::kable output of data preview.
#' @export
preview_tbl <- function(x, cols = 5, rows = 10, ...) {
  nr <- min(nrow(x), rows)
  nc <- min(ncol(x), cols)
  knitr::kable(
    x[1:nr, 1:nc],
    booktabs = TRUE,
    caption = paste0("The first ", nc, " columns and ", nr, " rows of ",
                     "`", deparse(substitute(x)), "`."),
    ...
  )
}

#' Arsenal Data Frame
#'
#' Convert arsenal::tableby to data frame.
#' @param x arsenal::tableby object.
#' @param labs Label translations.
#' @param ... Additional arguments sent to arsenal::summary.tableby.
#'
#' @return knitr::kable output of data preview.
#' @export
df_arsenal <- function(x, labs = NULL, ...) {
  a1 <- summary(x, text = TRUE)
  a1 <- as.data.frame(a1)
  a1 <- dplyr::select(a1, variable = 1)
  a1$variable <- ifelse(grepl("^-  ", a1$variable),
                        NA_character_,
                        a1$variable)
  a1 <- tidyr::fill(a1, !! "variable")

  a2 <- summary(x, text = TRUE, labelTranslations = labs, ...)
  a2 <- as.data.frame(a2)

  out <- dplyr::bind_cols(a1["variable"], a2)
  out <- dplyr::rename(out, ` ` = !! "V1")
  return(out)
}

#' Link to Data
#'
#' Makes a hyperlink to a downloadable data object.
#' @param x Data frame to be linked to.
#' @param path File path to write the downloadable data to.
#' @param timestamp Add a timestamp to the file name.
#'
#' @return HTML code for link
#' @return File saved to disk for download
#' @importFrom tools file_ext
#' @export
make_dl_link <- function(x, path, timestamp = TRUE){
  type <- file_ext(path)
  srch <- sub(paste0("\\.", type, "$"),
              paste0("*.", type),
              path)
  old <- sort(Sys.glob(srch), decreasing = TRUE)

  if (timestamp) {
    path <- sub(paste0("\\.", type, "$"),
                paste0("-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".", type),
                path)
  }

  if (type == "csv") {
    readr::write_csv(x, path)
  } else if (type == "xlsx") {
    writexl::write_xlsx(x, path)
  }
  if (length(old) > 0) {
    htmltools::tags$p(
      htmltools::h5("Download results:"),
      htmltools::a(basename(path),
                   href = path,
                   download = basename(path)),
      htmltools::h5("Previous results:"),
      lapply(old, function(xx) {
        link_text <- if (xx == tail(old, 1)) {
          basename(xx)
        } else {
          paste0(basename(xx), " | ")
        }
        htmltools::a(link_text,
                     href = xx,
                     download = basename(xx))
      })
    )
  } else {
    htmltools::tags$p(
      htmltools::h5("Download results:"),
      htmltools::a(basename(path),
                   href = path,
                   download = basename(path))
    )
  }
}

