# utils.R
# Small internal helper functions used by the rest of the package.
# Nothing here downloads anything - it's just string/path plumbing.

#' Turn a start year into the two-year code NCES uses in file names
#'
#' IPEDS labels a collection year like "2023-24" as starting year 2023.
#' NCES's documentation files and data files encode that as "202324"
#' (i.e. the two end years glued together, both as 2 digits).
#'
#' @param start_year integer, e.g. 2023 for the "2023-24" collection year
#' @return character string, e.g. "202324"
#' @keywords internal
.ipeds_year_code <- function(start_year) {
  start_year <- as.integer(start_year)
  end_yy <- sprintf("%02d", (start_year + 1) %% 100)
  paste0(start_year, end_yy)
}

#' Build (and create if needed) the local cache directory
#'
#' Everything this package downloads gets saved here so we never
#' re-download the same table/year twice. Point this at a folder inside
#' your GitHub repo so the cache can be committed and shared.
#'
#' @param cache_dir path to the cache folder
#' @keywords internal
.ensure_cache_dir <- function(cache_dir) {
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  invisible(cache_dir)
}

#' Quietly download a file only if it isn't already cached
#'
#' @param url source URL
#' @param destfile local path to save to
#' @param force logical, re-download even if destfile already exists
#' @keywords internal
.download_if_needed <- function(url, destfile, force = FALSE) {
  if (!file.exists(destfile) || force) {
    # mode = "wb" is important on Windows so zip/xlsx bytes aren't corrupted
    # by newline translation
    utils::download.file(url, destfile, mode = "wb", quiet = TRUE)
  }
  invisible(destfile)
}
