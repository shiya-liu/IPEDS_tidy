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
    # by newline translation. If the download fails, remove any partial file
    # so the next call doesn't mistake it for a good cached copy.
    tryCatch(
      utils::download.file(url, destfile, mode = "wb", quiet = TRUE),
      error = function(e) {
        if (file.exists(destfile)) unlink(destfile)
        stop(e)
      }
    )
  }
  invisible(destfile)
}

#' Check whether a file is a zip archive (zip files start with the bytes "PK")
#'
#' A failed request can leave an HTML error page on disk under a .zip name,
#' so we check the bytes instead of trusting the download call.
#'
#' @param path local file path
#' @return TRUE or FALSE
#' @keywords internal
.is_zip <- function(path) {
  file.exists(path) && identical(readBin(path, "raw", n = 2), charToRaw("PK"))
}

#' Download from the first URL that returns a real zip file
#'
#' Tries each URL in order and stops at the first one that works. Used because
#' NCES serves current and older tables from different locations.
#'
#' @param urls character vector of candidate URLs, tried in order
#' @param destfile local path to save the zip to
#' @return destfile (invisibly); errors listing every URL tried if none worked
#' @keywords internal
.download_first_zip <- function(urls, destfile) {
  for (url in urls) {
    ok <- tryCatch({
      # warnings (e.g. "HTTP status was 404") count as failure too
      suppressWarnings(utils::download.file(url, destfile, mode = "wb", quiet = TRUE))
      TRUE
    }, error = function(e) FALSE)
    if (ok && .is_zip(destfile)) return(invisible(destfile))
    if (file.exists(destfile)) unlink(destfile)
  }
  stop(
    "Could not download a zip from any of these URLs:\n  ",
    paste(urls, collapse = "\n  "),
    "\nCheck the table name against the dictionary and your connection to nces.ed.gov.",
    call. = FALSE
  )
}
