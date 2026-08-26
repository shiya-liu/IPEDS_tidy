# institutions.R
# Filter the institution directory (HD table) down to the schools you
# actually want, by characteristics rather than by typing out UNITIDs.
#
# Common HD columns and what their codes mean (full code books live in the
# dictionary under table_name == "HD<year>"):
#   STABBR   - two-letter state abbreviation, e.g. "CA"
#   SECTOR   - 0-9, detailed sector (public/private x 4yr/2yr/less-than-2yr x more)
#   CONTROL  - 1 = public, 2 = private nonprofit, 3 = private for-profit
#   ICLEVEL  - 1 = four-or-more-years, 2 = at-least-2-but-less-than-4-years, 3 = less-than-2-years
#   HBCU     - 1 = Historically Black College or University
#   TRIBAL   - 1 = Tribal college
#   CARNEGIE / C21BASIC - Carnegie Classification code (2021 basic classification)

#' Filter institutions by characteristics
#'
#' Any argument you leave NULL is not filtered on. Values are matched
#' case-insensitively for state, and against the raw IPEDS codes for
#' everything else (see the comments at the top of this file, or look them
#' up in the dictionary's valueSets rows).
#'
#' @param hd a tibble from get_institution_directory()
#' @param state character vector of state abbreviations, e.g. c("CA","NY")
#' @param control integer vector, 1 = public, 2 = private nonprofit, 3 = private for-profit
#' @param iclevel integer vector, 1 = 4yr, 2 = 2yr, 3 = less-than-2yr
#' @param sector integer vector, detailed IPEDS sector code
#' @param hbcu logical, TRUE to keep only HBCUs
#' @param tribal logical, TRUE to keep only tribal colleges
#' @param carnegie integer vector of Carnegie classification codes (C21BASIC)
#' @param name_contains character, keep institutions whose name contains this
#'   text (case-insensitive) - handy for a specific list like "Harvard"
#' @return filtered tibble, same columns as `hd`
#' @export
filter_institutions <- function(hd,
                                 state = NULL,
                                 control = NULL,
                                 iclevel = NULL,
                                 sector = NULL,
                                 hbcu = NULL,
                                 tribal = NULL,
                                 carnegie = NULL,
                                 name_contains = NULL) {
  out <- hd

  if (!is.null(state)) {
    out <- dplyr::filter(out, toupper(.data$STABBR) %in% toupper(state))
  }
  if (!is.null(control)) {
    out <- dplyr::filter(out, .data$CONTROL %in% control)
  }
  if (!is.null(iclevel)) {
    out <- dplyr::filter(out, .data$ICLEVEL %in% iclevel)
  }
  if (!is.null(sector)) {
    out <- dplyr::filter(out, .data$SECTOR %in% sector)
  }
  if (isTRUE(hbcu)) {
    out <- dplyr::filter(out, .data$HBCU == 1)
  }
  if (isTRUE(tribal)) {
    out <- dplyr::filter(out, .data$TRIBAL == 1)
  }
  if (!is.null(carnegie)) {
    out <- dplyr::filter(out, .data$C21BASIC %in% carnegie)
  }
  if (!is.null(name_contains)) {
    out <- dplyr::filter(out, stringr::str_detect(stringr::str_to_lower(.data$INSTNM), stringr::str_to_lower(name_contains)))
  }

  out
}
