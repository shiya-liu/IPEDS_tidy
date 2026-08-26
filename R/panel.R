# panel.R
# The orchestrator: turn a confirmed list of (year, table, variable) picks
# plus an institution filter into one tidy multi-year data frame.
#
# This is the function everything else in the package builds up to. You
# normally get to `selections` by running search_ipeds_variables() for each
# variable you described, reviewing the candidates, and keeping the rows you
# actually want.

#' Assemble a tidy multi-year IPEDS panel
#'
#' One row per institution per year. Columns from different survey tables in
#' the same year are joined together by UNITID; years are then stacked.
#' Institutions or variables that don't exist in a given year become NA
#' rather than causing an error, since IPEDS coverage genuinely changes
#' year to year.
#'
#' @param selections a data frame with one row per variable you want, columns
#'   `year`, `table_name`, `var_name` - typically built by running
#'   search_ipeds_variables() and keeping the rows you confirmed are correct
#' @param cache_dir the cache folder used by download_ipeds_table()
#' @param unitids optional integer vector of exact UNITIDs to keep. If NULL,
#'   supply institution filters instead (state=, control=, etc., passed
#'   through `...` to filter_institutions()).
#' @param ... institution filter arguments passed to filter_institutions(),
#'   e.g. state = c("CA","NY"), control = 1, iclevel = 1. Ignored if
#'   `unitids` is supplied directly.
#' @return tidy tibble: one row per UNITID x YEAR, with INSTNM/STABBR for
#'   readability plus every requested variable as its own column
#' @export
build_ipeds_panel <- function(selections, cache_dir, unitids = NULL, ...) {

  years <- sort(unique(selections$year))

  # --- Step 1: work out which institutions to keep ---------------------
  hd_all <- get_institution_directory(years, cache_dir)

  if (is.null(unitids)) {
    filter_args <- list(...)
    if (length(filter_args) > 0) {
      matched <- do.call(filter_institutions, c(list(hd = hd_all), filter_args))
      unitids <- unique(matched$UNITID)
    } else {
      # no filter given at all - keep every institution (rare, but allowed)
      unitids <- unique(hd_all$UNITID)
    }
  }

  # --- Step 2: pull each (year, table) combination once -----------------
  table_year_pairs <- unique(selections[, c("year", "table_name")])

  per_table <- purrr::pmap(table_year_pairs, function(year, table_name) {
    vars_needed <- selections$var_name[
      selections$year == year & selections$table_name == table_name
    ]

    data <- download_ipeds_table(table_name, cache_dir)

    # any_of() means a variable missing from this particular year's file
    # is silently skipped (with a warning) instead of erroring out - IPEDS
    # variable coverage genuinely changes year to year
    missing_vars <- setdiff(vars_needed, names(data))
    if (length(missing_vars) > 0) {
      warning(
        "Table ", table_name, " (year ", year, ") is missing: ",
        paste(missing_vars, collapse = ", "),
        " - these will be NA for that year.", call. = FALSE
      )
    }

    data |>
      dplyr::filter(.data$UNITID %in% unitids) |>
      dplyr::select(dplyr::any_of(c("UNITID", vars_needed))) |>
      dplyr::mutate(year = year, .before = 1)
  })

  # --- Step 3: join tables within each year, then stack years -----------
  by_year <- split(per_table, table_year_pairs$year)

  panel <- purrr::map_dfr(names(by_year), function(yr) {
    purrr::reduce(by_year[[yr]], dplyr::full_join, by = c("year", "UNITID"))
  })

  # --- Step 4: add institution name/state for readability ---------------
  hd_lookup <- hd_all |>
    dplyr::select(year = YEAR, UNITID, INSTNM, STABBR) |>
    dplyr::distinct()

  panel |>
    dplyr::left_join(hd_lookup, by = c("year", "UNITID")) |>
    dplyr::relocate(INSTNM, STABBR, .after = UNITID) |>
    dplyr::arrange(.data$UNITID, .data$year)
}
