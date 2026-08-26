# smoke_test.R
# Run this once after installing the package to confirm your machine can
# actually reach NCES and everything wires together end to end. It only
# touches one year and one small table, so it finishes in well under a
# minute on a normal connection.
#
# Run with:  source("inst/smoke_test.R")   (from the package root)
# or after installing the package:  source(system.file("smoke_test.R", package = "ipedstidy"))

library(ipedstidy)

cache_dir <- tempdir()  # use a throwaway folder for this test

cat("1) Building a one-year variable dictionary (2023)...\n")
dict <- build_ipeds_dictionary(years = 2023, cache_dir = cache_dir)
stopifnot(nrow(dict) > 1000)  # 2023 alone has ~2,700 variable rows
cat("   OK -", nrow(dict), "variable rows found\n\n")

cat("2) Searching the dictionary for 'total enrollment'...\n")
hits <- search_ipeds_variables(dict, "total enrollment", years = 2023)
stopifnot(nrow(hits) > 0)
print(hits[1:3, c("table_name", "var_name", "var_title")])
cat("   OK\n\n")

cat("3) Downloading the 2023 institution directory (HD2023)...\n")
hd <- get_institution_directory(years = 2023, cache_dir = cache_dir)
stopifnot(nrow(hd) > 5000)
cat("   OK -", nrow(hd), "institutions\n\n")

cat("4) Filtering to public 4-year institutions in Massachusetts...\n")
ma_public <- filter_institutions(hd, state = "MA", control = 1, iclevel = 1)
print(ma_public[, c("INSTNM", "STABBR")])
stopifnot(nrow(ma_public) > 0)
cat("   OK -", nrow(ma_public), "institutions matched\n\n")

cat("5) Building a one-variable, one-year panel for those institutions...\n")
# DRVEF2023 / ENRTOT = total enrollment, one of the "frequently used derived
# variables" tables - small and fast, good for a smoke test
selections <- data.frame(
  year = 2023,
  table_name = "DRVEF2023",
  var_name = "ENRTOT"
)
panel <- build_ipeds_panel(
  selections, cache_dir,
  unitids = ma_public$UNITID
)
print(panel)
# NOTE: nrow(panel) can be a little less than nrow(ma_public) - that's
# expected, not a bug. IPEDS's directory table sometimes includes
# administrative "central office" records alongside real campuses (e.g.
# UMass has one) that share the same state/control/level but report no
# enrollment data of their own, so they won't show up in DRVEF2023.
# build_ipeds_panel() prints a message() naming anything it drops for
# exactly this reason - check the message above, don't just check the count.
stopifnot(nrow(panel) > 0, nrow(panel) <= nrow(ma_public))
cat("   OK -", nrow(panel), "of", nrow(ma_public), "matched institutions have enrollment data\n\n")

cat("ALL SMOKE TESTS PASSED. The package can reach NCES and the pipeline works.\n")
