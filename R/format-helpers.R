
#' Replace annoying text to unicode symbol
#'
#' Uses `gsub()` to replace things like "<=" to the unicode symbol. Useful
#' for plotting.
#' @param x String to modify.
#'
#' @return String with substituted unicode symbol
#' @export
gsub_unicode <- function(x) {
  x <- gsub("<=", "\u2264", x)
  x <- gsub(">=", "\u2265", x)
  x
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

  old <- Sys.glob(srch)
  old <- sort(old, decreasing = TRUE)

  if (timestamp) {
    path <- sub(paste0("\\.", type, "$"),
                paste0("-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".", type),
                path)
  }

  old_files <- length(old) > 0
  write_fun <- if (type == "csv") {
    readr::write_csv
  } else if (type == "xlsx") {
    writexl::write_xlsx
  }

  read_fun <- if (type == "csv") {
    readr::read_csv
  } else if (type == "xlsx") {
    function(pp) {
      shts <- readxl::excel_sheets(pp)
      lapply(shts, readxl::read_xlsx, path = pp)
    }
  }

  write_fun(x, path)

  if (old_files) {
    new_file <- suppressMessages(read_fun(path))
    old_file <- suppressMessages(read_fun(old[1]))
    same <- if (type == "xlsx") {
      all(sapply(1:length(new_file), function(x){
        identical(new_file[[x]], old_file[[x]])
      }))
    } else identical(new_file, old_file)

    if (same) {
      file.remove(path)
      path <- old[1]
      old <- old[-1]
    }
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

