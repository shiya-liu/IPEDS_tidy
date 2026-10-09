# dictionary.R
# Build and search the "what does this variable mean, in which table, in
# which year" dictionary. This is what Claude (or you) searches whenever
# you describe a variable in plain English instead of its exact IPEDS name.
#
# The source is NCES's own per-year documentation workbook, e.g.
# https://nces.ed.gov/ipeds/tablefiles/tableDocs/IPEDS202324Tablesdoc.xlsx
# Every collection year back to 2004-05 has one of these. We don't hand-curate
# anything - we just download the official file and reshape it.

#' Build the URL for a given year's IPEDS documentation workbook
#'
#' @param start_year integer, e.g. 2023 for the "2023-24" collection year
#' @return character URL
#' @export
ipeds_doc_url <- function(start_year) {
  code <- .ipeds_year_code(start_year)
  paste0(
    "https://nces.ed.gov/ipeds/tablefiles/tableDocs/IPEDS", code, "Tablesdoc.xlsx"
  )
}

#' Download and parse the variable dictionary for one or more years
#'
#' For each year, this pulls NCES's own documentation workbook and combines
#' three of its sheets into one tidy data frame:
#'   - Tables{yy}     : one row per survey table (what does HD2023 cover)
#'   - varTable{yy}    : one row per variable (name, title, description, table)
#'   - valueSets{yy}   : one row per coded value (e.g. SECTOR = 1 means what)
#' The sheet name suffix changes by year (varTable23, varTable22, ...), so we
#' find it by pattern instead of hardcoding it.
#'
#' @param years integer vector of start years, e.g. 2018:2023
#' @param cache_dir folder to save/read the raw workbooks and the combined
#'   dictionary CSV. Point this at your GitHub repo's data folder so the
#'   dictionary can be committed and reused without re-downloading.
#' @param force logical, re-download workbooks even if already cached
#' @return a tidy tibble with columns: year, table_name, table_title,
#'   var_name, var_title, long_description, data_type
#' @export
build_ipeds_dictionary <- function(years, cache_dir, force = FALSE) {
  .ensure_cache_dir(cache_dir)
  raw_dir <- file.path(cache_dir, "raw_docs")
  .ensure_cache_dir(raw_dir)

  # One row of dictionary entries per requested year, then stack them
  per_year <- purrr::map_dfr(years, function(yr) {

    xlsx_path <- file.path(raw_dir, paste0("Tablesdoc_", .ipeds_year_code(yr), ".xlsx"))
    .download_if_needed(ipeds_doc_url(yr), xlsx_path, force = force)

    sheet_names <- readxl::excel_sheets(xlsx_path)

    # sheet names look like "varTable23", "Tables23", "valueSets23" - the
    # numeric suffix is the two-digit start year, so find each by prefix.
    # NCES is not consistent about capitalization across years (2021-22 uses
    # "Tables21"/"vartable21", 2024-25 uses "tables24"/"varTable24"), so the
    # match ignores case.
    yy <- sprintf("%02d", yr %% 100)
    var_sheet   <- sheet_names[stringr::str_detect(
      sheet_names, stringr::regex(paste0("^varTable", yy, "$"), ignore_case = TRUE))]
    table_sheet <- sheet_names[stringr::str_detect(
      sheet_names, stringr::regex(paste0("^Tables", yy, "$"), ignore_case = TRUE))]

    if (length(var_sheet) == 0 || length(table_sheet) == 0) {
      stop(
        "Could not find varTable/Tables sheets for year ", yr,
        " - found sheets: ", paste(sheet_names, collapse = ", "),
        call. = FALSE
      )
    }

    vars <- readxl::read_excel(xlsx_path, sheet = var_sheet) |> janitor::clean_names()
    tabs <- readxl::read_excel(xlsx_path, sheet = table_sheet) |> janitor::clean_names()

    # Keep just the columns we actually use downstream, renamed to a stable
    # schema that doesn't change even if NCES tweaks column names/casing.
    # table_title is only added if that column exists this year, so a
    # missing column here never breaks the whole build.
    if (!"table_title" %in% names(tabs)) tabs$table_title <- NA_character_
    tabs_small <- tabs |>
      dplyr::select(table_name, table_title) |>
      dplyr::distinct(table_name, .keep_all = TRUE)

    # long_description and data_type have been present in every year we've
    # checked, but fall back to NA instead of erroring if a future year
    # drops one of them
    for (optional_col in c("long_description", "data_type")) {
      if (!optional_col %in% names(vars)) vars[[optional_col]] <- NA_character_
    }

    vars |>
      dplyr::select(table_name, var_name, var_title, long_description, data_type) |>
      dplyr::left_join(tabs_small, by = "table_name") |>
      dplyr::mutate(year = yr, .before = 1)
  })

  out_path <- file.path(cache_dir, "ipeds_dictionary.csv")
  readr::write_csv(per_year, out_path)
  message("Dictionary saved to ", out_path, " (", nrow(per_year), " variable-year rows)")

  per_year
}

#' Load a previously-built dictionary CSV without re-downloading anything
#'
#' @param cache_dir folder where build_ipeds_dictionary() saved the CSV
#' @export
load_ipeds_dictionary <- function(cache_dir) {
  path <- file.path(cache_dir, "ipeds_dictionary.csv")
  if (!file.exists(path)) {
    stop(
      "No dictionary found at ", path,
      ". Run build_ipeds_dictionary() first.", call. = FALSE
    )
  }
  readr::read_csv(path, show_col_types = FALSE)
}

#' Search the dictionary for variables matching a plain-English description
#'
#' This does NOT try to be a black box that silently picks "the" variable -
#' IPEDS variable names are genuinely ambiguous (e.g. multiple "retention
#' rate" variables exist for full-time vs part-time, 2yr vs 4yr cohorts).
#' Instead it ranks candidates so a person (or Claude, reading on your
#' behalf) can pick the right one(s) with the description in front of them.
#'
#' Ranking logic:
#'   tier 0 = every search word appears in the variable TITLE
#'   tier 1 = every search word appears somewhere in title + description
#'   tier 2 = only some search words matched
#' Within a tier, more total word-hits (title hits count 3x) ranks higher.
#'
#' @param dict a dictionary tibble from build_ipeds_dictionary()/load_ipeds_dictionary()
#' @param query character, e.g. "graduation rate 150 percent"
#' @param years optional integer vector to restrict the search to
#' @param n how many top candidates to return
#' @return tibble of candidate matches, best first
#' @export
search_ipeds_variables <- function(dict, query, years = NULL, n = 15) {
  words <- stringr::str_to_lower(stringr::str_split(stringr::str_squish(query), " ")[[1]])

  d <- dict
  if (!is.null(years)) {
    d <- dplyr::filter(d, .data$year %in% years)
  }

  d |>
    dplyr::mutate(
      title_l = stringr::str_to_lower(.replace_na_with(.data$var_title, "")),
      desc_l  = stringr::str_to_lower(.replace_na_with(.data$long_description, "")),
      # how many of the search words show up in the title alone / in title+description
      title_hits = .count_word_hits(title_l, words),
      desc_hits  = .count_word_hits(desc_l, words),
      blob_hits  = .count_word_hits(paste(title_l, desc_l), words),
      # tier 0 = every word matched the title (best); tier 1 = every word
      # matched somewhere; tier 2 = only a partial match
      tier = dplyr::case_when(
        title_hits == length(words) ~ 0L,
        blob_hits  == length(words) ~ 1L,
        TRUE ~ 2L
      ),
      score = title_hits * 3 + desc_hits
    ) |>
    dplyr::filter(.data$blob_hits > 0) |>
    dplyr::arrange(.data$tier, dplyr::desc(.data$score)) |>
    dplyr::select(year, table_name, table_title, var_name, var_title, long_description, tier, score) |>
    utils::head(n)
}

#' Replace NA entries in a character vector with a fallback value
#' @keywords internal
.replace_na_with <- function(x, fallback) {
  dplyr::if_else(is.na(x), fallback, x)
}

#' Count, for each element of a text vector, how many of `words` appear in it
#' @param text character vector, one string per row
#' @param words character vector of lowercase search terms
#' @return integer vector, same length as text
#' @keywords internal
.count_word_hits <- function(text, words) {
  hits <- purrr::map(words, ~ stringr::str_detect(text, stringr::fixed(.x)))
  Reduce(`+`, hits)
}
