#' Tidy Hurdle Model
#'
#' broom::tidy.hurdle method
#' @param hm The hurdle model.
#' @param exponentiate Exponentiate the estimates.
#' @param ... Additional arguments passed to `confint`.
#'
#' @return Tibble containing the tidied hurdle model.
#' @importFrom stats confint
#' @export
tidy.hurdle <- function(hm, exponentiate = FALSE, ...) {
  hm_coef <- summary(hm)$coef
  hm_coef <- lapply(hm_coef, data.frame)
  hm_coef <- do.call(rbind, hm_coef)
  hm_coef <- tibble::rownames_to_column(hm_coef, var = "term")
  hm_coef$term <- sub("^count\\.", "count_", hm_coef$term)
  hm_coef$term <- sub("zero\\.", "zero_", hm_coef$term)

  hm_ci <- confint(hm, ...)
  hm_ci <- data.frame(hm_ci)
  hm_ci <- tibble::rownames_to_column(hm_ci, var = "term")

  hm_tidy <- dplyr::inner_join(hm_coef, hm_ci, by = "term")
  hm_tidy <- hm_tidy[c("term", "Estimate", "Std..Error", "z.value",
                       "Pr...z..", "X2.5..", "X97.5..")]
  names(hm_tidy) <- c("term", "estimate", "std.error", "statistic",
                      "p.value", "conf.low", "conf.high")
  hm_tidy <- tibble::as_tibble(hm_tidy)

  if (!exponentiate)  return(hm_tidy)

  hm_tidy$estimate <- exp(hm_tidy$estimate)
  ci_lo <- exp(hm_tidy$conf.low)
  ci_hi <- exp(hm_tidy$conf.high)
  hm_tidy$conf.low <- pmin(ci_lo, ci_hi)
  hm_tidy$conf.high <- pmax(ci_lo, ci_hi)

  return(hm_tidy)
}

#' Sniff Model Objects
#'
#' Sniff out and return model information
#' @param x The model to be sniffed.
#' @param ... Additional arguments sent to `broom::tidy`.
#' @name sniff
#'
#' @return Tibble containing the sniffed out model info
#' @importFrom stats formula residuals
#' @export
sniff <- function (x, ...) {
  UseMethod("sniff", x)
}

#' @rdname sniff
#' @export
sniff.default <- function(x, ...) {
  out <- tibble::tibble(method = as.character(x$call)[1],
                        formula = format(formula(x)),
                        n = length(residuals(x)),
                        result = list(broom::tidy(x, conf.int = TRUE, ...)))
  out <- tidyr::unnest(out)
  return(out)
}

#' @rdname sniff
#' @export
sniff.hurdle <- function(x, ...) {
  out <- sniff.default(x, ...)
  dist <- paste(x$dist, collapse = "_")
  out$method <- paste(out$method, dist, sep = "_")
  out
}

#' @rdname sniff
#' @export
sniff.adonis <- function(x, ...) {
  out <- tibble::tibble(method = as.character(x$call)[1],
                        formula = format(formula(x)),
                        n = nrow(x$model.matrix),
                        n_perms = nrow(x$f.perms),
                        result = list(suppressWarnings(broom::tidy(x$aov, ...))))
  out <- tidyr::unnest(out)
  return(out)
}

#' Polish Data Objects
#'
#' Select specified variables from data.frame or rows/columns from matrix/dist
#' @param x The object to be polished
#' @param ... Columns passed to dplyr::select
#' @param cols Columns to keep from matrix
#' @param rows Rows to keep from matrix
#' @param id Names/index to keep from dist
#' @param mat Return dist object as matrix
#' @name polish
#'
#' @return data.frame with specified colums or matrix/dist with specified rows/columns
#' @importFrom stats as.dist
#' @export
polish <- function (x, ...) {
  UseMethod("polish", x)
}

#' @rdname polish
#' @export
polish.data.frame <- function(x, ...) {
  dplyr::select(x, "stool_id", ...)
}

#' @rdname polish
#' @export
polish.matrix <- function(x, cols = NULL, rows = NULL, ...) {
  x[rows, cols, drop = FALSE]
}

#' @rdname polish
#' @export
polish.dist <- function(x, id = NULL, mat = FALSE, ...) {
  x <- as.matrix(x)
  x <- polish(x, id, id)
  if (mat) return(x)
  as.dist(x)
}

#' Retrieve Data Objects
#'
#' Load data (.rds file) and polish
#' @param x Data to be loaded
#' @param ... Arguments passed to polish()
#' @param path Path to data directory
#' @param join Type of join to use when retrieving list of data
#' @name retrieve
#'
#' @return Polished data.frame, matrix, or dist
#' @export
retrieve <- function (x, ...) {
  UseMethod("retrieve", x)
}

#' @rdname retrieve
#' @export
retrieve.default <- function(x, ..., path = "Data") {
  path <- file.path(path, paste0(x, ".rds"))
  polish(readRDS(path), ...)
}

#' @rdname retrieve
#' @export
retrieve.list <- function(x, ..., path = "Data", join = dplyr::left_join){
  x <- purrr::imap(x, ~ retrieve(.y, .x, path = path))
  purrr::reduce(x, join, by = "stool_id")
}

#' Create Formula
#'
#' formula with variable of interest placed as the last term
#' @param x variable of interest
#' @param adj adjustment variables
#' @param y response variable
#' @param ... Arguments passed to reformulate()
#'
#' @return formula
#' @importFrom stats reformulate
#' @export
formadoo <- function(x, adj = NULL, y = NULL, ...) {
  adj <- setdiff(unlist(adj), c(x, y))
  x <- setdiff(x, y)
  x <- c(adj, x)
  reformulate(x, y, ...)
}

#' Stratify Data
#'
#' Stratify a data frame by a list of strata
#' @param df The data to be stratified.
#' @param strata List of vectors to stratify by.
#' @param name_all The indicator name for the strata containing all observations
#' @param sep  Separator
#' @param ... Additional arguments sent to `broom::tidy`.
#'
#' @return Tibble containing stratified data.
#'
#' strata should be a list of vectors to stratify df by;
#' each vector should have length = nrow(df);
#' if named list, then the names become
#' columns in the data frame (duplicate names are ajusted);
#' if not a named list, then generic names are made
#' @importFrom utils tail
#' @export
tbl_strata <- function(df, strata, name_all = "All", sep = "___") {
  stopifnot(is.list(strata))
  stopifnot(all(sapply(strata, is.atomic)))

  chk <- lapply(strata, length)
  stopifnot(all(chk == nrow(df)))

  strata <- lapply(strata, as.factor)

  nn <- if (is.null(names(strata))) {
    paste0("s_", seq_along(strata))
  } else {
    names(strata)
  }
  nn[nn == ""] <- "no_name"
  nn <- tail(make.unique(c(names(df), nn), sep = "__"), length(nn))
  names(strata) <- nn

  cc <- split(df, strata, sep = "___")
  dd <- dplyr::bind_rows(cc, .id = "sss")
  dd <- tidyr::separate(dd, !! "sss", names(strata), sep = sep)
  dd <- dplyr::bind_rows(dd, df)

  for (x in names(strata)) {
    dd[[x]] <- factor(dd[[x]], levels = levels(strata[[x]]))
    dd[[x]] <- forcats::fct_explicit_na(dd[[x]], name_all)
  }

  dd
}
