# download.R
# Download and cache individual IPEDS survey tables as plain CSV data frames.
#
# NCES serves each table as its own small zip (not just the giant multi-GB
# Access database). The URL pattern below was confirmed against NCES's site
# and against a well-established open-source IPEDS download script:
#   https://nces.ed.gov/ipeds/datacenter/data/HD2023_Data_Stata.zip
# Despite the "_Stata" in the name, the zip actually contains a plain CSV -
# no Stata software or license is needed to read it.

#' Build the download URL for one IPEDS survey table
#'
#' @param table_name exact table name as it appears in the dictionary's
#'   `table_name` column, e.g. "HD2023", "EF2023A", "F2223_F1A"
#' @return character URL to the zip file containing that table's CSV
#' @export
ipeds_table_url <- function(table_name) {
  paste0("https://nces.ed.gov/ipeds/datacenter/data/", table_name, "_Data_Stata.zip")
}

#' Download (if not already cached) and read one IPEDS survey table
#'
#' @param table_name exact table name, e.g. "HD2023"
#' @param cache_dir folder to store the downloaded zip, unzipped CSV, and a
#'   cleaned copy. Point this at your GitHub repo so tables only ever need
#'   to be downloaded once, by anyone.
#' @param force logical, re-download even if a cached copy exists
#' @return tibble with the table's data, column names left as-is (upper case,
#'   IPEDS convention) except for a couple of internal housekeeping columns
#'   dropped or renamed
#' @export
download_ipeds_table <- function(table_name, cache_dir, force = FALSE) {
  .ensure_cache_dir(cache_dir)
  tables_dir <- file.path(cache_dir, "tables")
  .ensure_cache_dir(tables_dir)

  cleaned_path <- file.path(tables_dir, paste0(table_name, ".csv"))

  if (file.exists(cleaned_path) && !force) {
    return(readr::read_csv(cleaned_path, show_col_types = FALSE))
  }

  zip_path <- file.path(tables_dir, paste0(table_name, "_raw.zip"))
  .download_if_needed(ipeds_table_url(table_name), zip_path, force = force)

  unzip_dir <- file.path(tables_dir, paste0(table_name, "_unzipped"))
  if (dir.exists(unzip_dir)) unlink(unzip_dir, recursive = TRUE)
  utils::unzip(zip_path, exdir = unzip_dir)

  csv_files <- list.files(unzip_dir, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
  if (length(csv_files) == 0) {
    stop(
      "No CSV found inside the downloaded zip for ", table_name,
      " - NCES may have changed their file naming. Check ", ipeds_table_url(table_name),
      " manually.", call. = FALSE
    )
  }

  # If NCES shipped a revised version (filename contains "_rv"), prefer that
  # one over the original - it supersedes it
  revised <- csv_files[grepl("_rv", csv_files, ignore.case = TRUE)]
  chosen_file <- if (length(revised) > 0) revised[1] else csv_files[1]

  # some years ship a duplicated column (e.g. two "ef2022a" columns) - repair
  # to unique names quietly rather than erroring, we dedupe properly next
  data <- suppressMessages(
    readr::read_csv(chosen_file, show_col_types = FALSE, name_repair = "unique")
  )

  # IPEDS variable names are already the "real" names (e.g. UNITID, INSTNM) -
  # we upper-case them for consistency, since a few years ship lower case,
  # and drop any duplicate columns that survived name-repair (keep first)
  names(data) <- toupper(names(data))
  data <- data[, !duplicated(names(data)), drop = FALSE]

  readr::write_csv(data, cleaned_path)

  # tidy up the intermediate zip/unzip so the cache folder doesn't balloon
  unlink(zip_path)
  unlink(unzip_dir, recursive = TRUE)

  data
}

#' Download/cache the institution directory (HD table) for one or more years
#'
#' The HD table is IPEDS's "who is this institution" table - name, state,
#' sector, control (public/private), level (4yr/2yr), Carnegie class, HBCU
#' status, etc. This is what filter_institutions() filters on.
#'
#' @param years integer vector of start years, e.g. 2018:2023
#' @param cache_dir same cache folder used elsewhere
#' @param force logical, re-download even if cached
#' @return tibble stacking each year's HD table, with a `YEAR` column added
#' @export
get_institution_directory <- function(years, cache_dir, force = FALSE) {
  purrr::map_dfr(years, function(yr) {
    hd <- download_ipeds_table(paste0("HD", yr), cache_dir, force = force)
    dplyr::mutate(hd, YEAR = yr, .before = 1)
  })
}
