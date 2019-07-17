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
