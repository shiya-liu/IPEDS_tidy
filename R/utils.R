# utils.R
# Small internal helper functions used by the rest of the package:
# string/path plumbing, downloading, the download log, and the CSV reader.

# The IPEDS page that lists every table's "Data File" zip
.ipeds_files_page <- "https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?gotoReportId=7&fromIpeds=true"

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
#' Every real download is recorded in the cache's download log.
#'
#' @param url source URL
#' @param destfile local path to save to
#' @param cache_dir cache folder (the download log lives here)
#' @param force logical, re-download even if destfile already exists
#' @keywords internal
.download_if_needed <- function(url, destfile, cache_dir, force = FALSE) {
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
    .log_download(cache_dir, file = basename(destfile), source_url = url)
  }
  invisible(destfile)
}

#' Add one row to the cache's download log
#'
#' The log (download_log.csv in the cache folder) records where every cached
#' file came from and when, so a cached table can always be traced back to
#' its NCES source. One row is added per download; a re-download with
#' force = TRUE adds a new row rather than overwriting the old one.
#'
#' @param cache_dir cache folder
#' @param file name of the cached file, e.g. "ADM2024.csv"
#' @param source_url the URL the file was downloaded from
#' @param source_file for zips: the CSV inside the zip that was used
#' @keywords internal
.log_download <- function(cache_dir, file, source_url, source_file = NA_character_) {
  log_path <- file.path(cache_dir, "download_log.csv")
  row <- tibble::tibble(
    file          = file,
    source_url    = source_url,
    source_file   = source_file,
    downloaded_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  )
  # append = TRUE also skips the header row, so only the first write adds it
  readr::write_csv(row, log_path, append = file.exists(log_path))
  invisible(log_path)
}

#' Read a CSV with every column as text
#'
#' Reading as text first means no value can be lost to a wrong type guess.
#' Blank cells (and "NA", which 0.1.0 cache files used) become NA.
#'
#' @param file path to a CSV file
#' @return tibble, all columns character
#' @keywords internal
.read_csv_as_text <- function(file) {
  suppressMessages(
    readr::read_csv(
      file,
      col_types = readr::cols(.default = readr::col_character()),
      na = c("", "NA"),
      name_repair = "unique",  # a few years ship duplicated column names
      show_col_types = FALSE,
      progress = FALSE
    )
  )
}

#' Turn text columns into numbers where every value is a number
#'
#' type_convert() decides each column's type by looking at ALL of its
#' values, not a sample of rows. Number columns become numeric; codes with
#' leading zeros (e.g. "0123") and any column with a non-number stay text.
#'
#' @param text tibble from .read_csv_as_text()
#' @return tibble with column types set
#' @keywords internal
.set_column_types <- function(text) {
  readr::type_convert(text, col_types = readr::cols(.default = readr::col_guess()))
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
#' @return the URL that worked (for the download log); errors listing every
#'   URL tried if none worked
#' @keywords internal
.download_first_zip <- function(urls, destfile) {
  for (url in urls) {
    ok <- tryCatch({
      # warnings (e.g. "HTTP status was 404") count as failure too
      suppressWarnings(utils::download.file(url, destfile, mode = "wb", quiet = TRUE))
      TRUE
    }, error = function(e) FALSE)
    if (ok && .is_zip(destfile)) return(url)
    if (file.exists(destfile)) unlink(destfile)
  }
  stop(
    "Could not download a zip from any of these URLs:\n  ",
    paste(urls, collapse = "\n  "),
    "\nCheck the table name against the dictionary and your connection to nces.ed.gov.",
    "\nThe table's 'Data File' link is listed on the IPEDS Complete Data Files page: ",
    .ipeds_files_page,
    call. = FALSE
  )
}
