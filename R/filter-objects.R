#' Filter MEC Data Objects
#'
#' Filters MEC data objects to pre-defined sample sets
#' @param x The MEC data object to be filtered.
#' @param study The MEC study to filter to.
#' @name filter_mec
#'
#' @return The filtered data object
#' @export
filter_mec <- function (x, study) {
  UseMethod("filter_mec", x)
}

#' @rdname filter_mec
#' @export
filter_mec.default <- function(x,
                               study = c("1", "2", "mGWAS", "MEC-APS",
                                         "LF5", "LF55", "PF5", "PF55",
                                         "VF", "FHC", "Other")) {
  study <- match.arg(study)
  fn <- dplyr::case_when(
    study == "1" ~ "samples_study1.rds",
    study == "2" ~ "samples_study2.rds",
    study == "mGWAS" ~ "samples_mgwas.rds",
    study == "MEC-APS" ~ "samples_mecaps.rds",
    study == "LF5" ~ "samples_nafld5.rds",
    study == "LF55" ~ "samples_nafld55.rds",
    study == "PF5" ~ "samples_nafpd5.rds",
    study == "PF55" ~ "samples_nafpd55.rds",
    study == "VF" ~ "samples_viscfat.rds",
    study == "FHC" ~ "samples_fhc.rds",
    study == "Other" ~ "samples_others.rds"
  )
  fn <- file.path("Data", fn)
  if (!file.exists(fn)) stop("Sample source file does not exist!")

  return(readRDS(fn))
}

#' @rdname filter_mec
#' @export
filter_mec.data.frame <- function(x, study) {
  ids <- filter_mec.default(x, study)
  id_cols <- intersect(names(x), names(ids))
  if (length(id_cols) == 0) {
    stop("Data does not have stool_id, p01_id, or lab_id columns")
  }
  dplyr::semi_join(x, ids, by = id_cols)
}

#' @rdname filter_mec
#' @export
filter_mec.matrix <- function(x, study) {
  ids <- filter_mec.default(x, study)
  cn <- colnames(x)
  rn <- rownames(x)
  if (!identical(cn, rn)) stop("Expecting identical row and column names")
  id_col <- sapply(ids, function(x) length(intersect(x, cn)))
  if (all(id_col == 0)) {
    stop("Data does not have valid stool_ids, p01_ids, or lab_ids")
  }
  id_keep <- ids[[names(which.max(id_col))]]
  id_keep <- intersect(cn, id_keep)
  x[id_keep, id_keep, drop = FALSE]
}

#' @rdname filter_mec
#' @export
filter_mec.dist <- function(x, study) {
  ids <- filter_mec.default(x, study)
  x <- as.matrix(x)
  out <- filter_mec(x, study)
  stats::as.dist(out)
}
