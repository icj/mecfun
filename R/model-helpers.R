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

#' Get tableby()
#'
#' Run custom tableby()
#' @param df source data.frame
#' @param y response variable
#' @param x variable of interest
#' @param fm MEC filter
#' @param lab Label for x
#' @param ... Arguments passed to tableby()
#'
#' @return tableby object
#' @export
get_tableby <- function(df, y, x, fm, lab = NULL, ...) {
  frm <- formulize(y, x)
  df <- df %>%
    polish(y, x) %>%
    filter_mec(fm)
  if (!is.null(lab)) {
    labels(df)[[x]] <- lab
  }
  tableby(frm, data = df,
          numeric.stats = c("Nmiss", "meansd", "medianq1q3", "range"),
          digits = 3, digits.pct = 0, ...)
}

#' Get lm()
#'
#' Run custom lm()
#' @param df source data.frame
#' @param y response variable
#' @param x variable of interest
#' @param adj adjustment varaiables
#' @param fm MEC filter
#' @param ... Arguments passed to lm()
#'
#' @return list with tidy(), glance(), and lrtest() results
#' @export
get_lm <- function(df, y, x, adj, fm, ...) {
  df <- df %>%
    polish(y, x, adj) %>%
    filter_mec(fm) %>%
    drop_na
  frm1 <- formadoo(x = x, adj = adj, y = y)
  frm0 <- formadoo(x = adj, y = y)

  mod1 <- lm(frm1, data = df, ...)
  mod0 <- update(mod1, frm0)

  m1_out <- try(list(tidy = tidy(mod1, conf.int = TRUE),
                     glance = glance(mod1),
                     lrt = lmtest::lrtest(mod0, mod1)))

  m1_out
}

#' SAS-like Proc Rank() for binning
#'
#' Run SAS-like proc rank to create bins
#' @param x numeric vector
#' @param k number of bins
#' @param ties how to break ties (see rank)
#'
#' @return ordered factor
#' @export
proc_rank <- function(x, k = 5, ties = "min") {
  stopifnot(!any(is.na(x)))
  out <- floor(rank(x, ties.method = ties) * k / (length(x) + 1)) + 1
  factor(out, levels = 1:k, ordered = TRUE)
}

#' Get Data for polr()
#'
#' Retrieve, polish and quintile data for polr() bug models
#' @param df source data.frame
#' @param x variable of interest
#' @param adj adjustment varaiables
#' @param fm MEC filter
#' @param bug genera or phyla
#' @param ... Arguments passed to proc_rank()
#'
#' @return nested tibble
#' @export
get_polr_data <- function(df, x, adj, fm, bug = c("phyla", "genera"), ...) {
  bug_id <- list(genera = quo(genus_id), phyla = quo(phylum))[[bug]]

  df <- df %>%
    polish(x, adj) %>%
    filter_mec(fm)

  retrieve(bug, !! bug_id, rel_abd) %>%
    filter_mec("mGWAS") %>%
    group_by(!! bug_id) %>%
    mutate(quintile = proc_rank(rel_abd, ...)) %>%
    ungroup %>%
    filter_mec(fm) %>%
    inner_join(df, by = "stool_id") %>%
    drop_na %>%
    nest(-!! bug_id)
}

#' Get Results for polr()
#'
#' Create list of polr() and lrtest() results
#' @param m1 polr() object
#' @param lrt lrtest() object
#'
#' @return list with tidy(), glance(), and lrtest() results
#' @export
get_polr_result <- function(m1, lrt) {
  m1_out <- try(list(tidy = tidy(m1, exponentiate = TRUE) %>%
                       filter(coefficient_type == "coefficient") %>%
                       bind_cols(
                         confint_tidy(m1, func = stats::confint.default) %>%
                           mutate_all(exp) %>%
                           mutate(ci_lo = pmin(conf.low, conf.high),
                                  ci_hi = pmax(conf.low, conf.high)) %>%
                           select(conf.low = ci_lo, conf.high = ci_hi)) %>%
                       select(-coefficient_type),
                     glance = glance(m1),
                     lrt = lrt))
  m1_out
}

#' Get polr()
#'
#' Run custom polr()
#' @param df source data.frame
#' @param x variable of interest
#' @param adj adjustment varaiables
#' @param fm MEC filter
#' @param bug genera or phyla
#' @param ... Arguments passed to polr()
#'
#' @return list with tidy(), glance(), and lrtest() results
#' @export
get_polr <- function(df, x, adj, fm, bug = c("phyla", "genera"), ...) {
  bug <- match.arg(bug)
  df <- get_polr_data(df, x, adj, fm, bug)
  frm1 <- formadoo(x = x, adj = adj, y = "quintile")
  frm0 <- formadoo(x = adj, y = "quintile")

  mod_fun <- function(dfb, frm, ...) {
    MASS::polr(formula = frm,
               data = dfb,
               Hess = TRUE,
               method = "logistic",
               ...)
  }

  df %>%
    mutate(mod1 = map(data, possibly(mod_fun, otherwise = NULL), frm = frm1),
           mod0 = map(data, possibly(mod_fun, otherwise = NULL), frm = frm0),
           lrt = map2(mod0, mod1, possibly(lmtest::lrtest, otherwise = NULL)),
           res = map2(mod1, lrt, possibly(get_polr_result, otherwise = NULL))) %>%
    select(-data, -mod1, -mod0, -lrt)
}

#' Get Permanova Data
#'
#' Retrieve and polisth Unifrac matrix and predictor data
#' @param df source data.frame
#' @param x variable of interest
#' @param adj adjustment varaiables
#' @param fm MEC filter
#' @param uni unifrac_uw or unifrac_wt
#'
#' @return list with unifrac distance object and predictor data.frame
#' @export
get_paov_data <- function(df, x, adj, fm, uni = c("unifrac_uw", "unifrac_wt")) {
  uni <- match.arg(uni)
  df <- df %>%
    polish(x, adj) %>%
    filter_mec(fm)
  mat <- retrieve(uni, df$stool_id)
  stopifnot(identical(labels(mat), df$stool_id))
  list(yy = mat, xx = df)
}

#' Get Permanova
#'
#' Run custon adonis()/adonis2()
#' @param df source data.frame
#' @param x variable of interest
#' @param adj adjustment varaiables
#' @param fm MEC filter
#' @param uni unifrac_uw or unifrac_wt
#' @param md a1 = adonis() or a2 = adonis2()
#' @param np number of permutations
#' @param seed seed for reproducibility
#' @param ... arguments passed to adonis()/adonis2()
#'
#' @return list with unifrac label and model results
#' @export
get_paov <- function(df, x, adj, fm, uni, md = c("a1", "a2"), np = 2, seed = 123456, ...) {
  md <- match.arg(md)
  df <- get_paov_data(df, x, adj, fm, uni)
  mat <- df$yy
  df <- df$xx
  frm <- formadoo(x = x, adj = adj, y = "mat")
  set.seed(seed)
  mod_fun <- list(
    a1 = function() {
      vegan::adonis(formula = frm,
                    data = df,
                    contr.unordered = "contr.treatment",
                    permutations = np,
                    ...)
    },
    a2 = function() {
      vegan::adonis2(formula = frm,
                     data = df,
                     by = NULL,
                     permutations = np,
                     ...)
    }
  )
  mod <- try(mod_fun[[md]]())
  mod <- if (any(class(mod) == "try_error") | md == "a2") mod else mod$aov.tab

  list(unifrac = uni, mod = mod)
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
