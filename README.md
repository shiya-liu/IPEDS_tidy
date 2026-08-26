# ipedstidy

An R package + Claude Skill that get you a tidy, multi-year IPEDS (higher-ed
institutional data) panel from a plain-English request, instead of clicking
through the [IPEDS Data Center](https://nces.ed.gov/ipeds/datacenter) year by
year and stitching files together by hand.

## Why it's split into two pieces

Claude's chat sandbox cannot reach `nces.ed.gov` directly (its outbound
network is allow-listed and NCES isn't on it). So the actual downloading has
to happen somewhere with normal internet access — your computer, via R.
Once something is downloaded, though, it's saved as a plain CSV in a cache
folder that you commit to a GitHub repo — and Claude *can* read a GitHub
repo (via `git`), so anything already cached can be served straight from
chat with no R involved.

In practice: the first time you ask for a given year/table, you'll run one
R command. Every request after that, for data you've already pulled, just
happens in chat.

## One-time setup

1. **Create a GitHub repo** (public is simplest) to hold the cache, e.g.
   `ipeds-cache`. This is what grows over time and what Claude reads from.

2. **Install the package locally**, from this folder:

   ```r
   install.packages(c("dplyr", "readr", "readxl", "stringr", "purrr",
                       "tibble", "janitor", "rlang"))
   install.packages("devtools")  # only needed to install a local package folder
   devtools::install("path/to/ipedstidy")
   ```

3. **Run the smoke test** to confirm your machine can actually reach NCES
   and the whole pipeline works end to end:

   ```r
   source("path/to/ipedstidy/inst/smoke_test.R")
   ```

   It should finish in well under a minute and print `ALL SMOKE TESTS
   PASSED.` If it doesn't, fix that before doing anything bigger — see
   Troubleshooting below.

4. **Point the cache at your cloned repo** and build your first dictionary:

   ```r
   library(ipedstidy)
   cache_dir <- "path/to/your/local/clone/of/ipeds-cache/data"

   # Build the definitions dictionary for the years you care about.
   # This is what lets you (or Claude) search by plain-English description
   # instead of needing to already know the exact IPEDS variable name.
   dict <- build_ipeds_dictionary(years = 2015:2023, cache_dir = cache_dir)
   ```

   Then commit and push the repo. From now on, Claude can read that
   dictionary directly without you doing anything.

5. **Tell Claude about the repo once** — in any chat, say something like
   "my IPEDS cache repo is https://github.com/you/ipeds-cache". It'll save
   that as a reference memory so future sessions don't need to be told
   again, and the `ipeds-panel-builder` skill will know where to look.

## Everyday use, in chat

Once set up, just ask in plain English:

> "Get me total enrollment and 6-year graduation rate for public 4-year
> universities in New England, 2018 through 2023."

Claude will search the dictionary, show you the specific variables it
thinks you mean before committing to anything, resolve which institutions
match "public 4-year in New England," and either hand you a finished tidy
CSV directly (if everything needed is already cached) or give you the exact
one-line R command to run locally for whatever isn't cached yet.

## Everyday use, directly in R

You don't need Claude at all if you'd rather drive it yourself:

```r
library(ipedstidy)
cache_dir <- "path/to/ipeds-cache/data"

dict <- load_ipeds_dictionary(cache_dir)

# search however you'd describe the variable
search_ipeds_variables(dict, "graduation rate 150 percent", years = 2023)

# once you've picked the exact rows you want:
selections <- data.frame(
  year       = c(2023, 2023),
  table_name = c("DRVEF2023", "GR200_23"),
  var_name   = c("ENRTOT",    "BAGR150")
)

panel <- build_ipeds_panel(
  selections, cache_dir,
  state = c("CT", "ME", "MA", "NH", "RI", "VT"),
  control = 1,     # public
  iclevel = 1       # 4-year or more
)
```

`panel` is one row per institution per year, ready for `dplyr`/`ggplot2`/
whatever comes next.

## Function reference

| Function | What it does |
|---|---|
| `build_ipeds_dictionary(years, cache_dir)` | Downloads NCES's own per-year documentation workbook and builds a searchable variable dictionary. Run once per new year you need. |
| `load_ipeds_dictionary(cache_dir)` | Reads a dictionary already built, no download. |
| `search_ipeds_variables(dict, query, years, n)` | Ranks dictionary rows against a plain-English description. Returns candidates to review, not a single guess. |
| `get_institution_directory(years, cache_dir)` | Downloads/caches the HD (directory) table for each year — institution names, state, sector, level, Carnegie class, etc. |
| `filter_institutions(hd, ...)` | Filters a directory table by state/control/level/sector/HBCU/tribal/Carnegie/name. |
| `download_ipeds_table(table_name, cache_dir)` | Downloads and caches one specific survey table as a CSV. |
| `build_ipeds_panel(selections, cache_dir, unitids = NULL, ...)` | The main entry point — joins everything into one tidy panel. |

## A few things worth knowing

- **Table names change shape by year for some surveys.** Finance and
  student financial aid tables encode a fiscal-year offset (e.g. `F2223_F1A`
  for the file released alongside 2023-24 IPEDS data, covering FY2022-23).
  Always pull the exact `table_name` from the dictionary for the year in
  question rather than assuming a fixed pattern.
- **Variable matching is deliberately not fully automatic.** IPEDS has many
  similarly-named variables that mean different things (full-time vs.
  part-time retention; 100% vs. 150% of normal time; GASB vs. FASB
  accounting for public vs. private institutions). `search_ipeds_variables()`
  ranks candidates; a person (or Claude, checking with you) should confirm
  the right one before a big pull.
- **Institutions open, close, and change sector.** `build_ipeds_panel()`
  resolves institution filters per year and takes the union across years by
  default, so a school matches the panel for every requested year if it
  matched the filter in *any* of them. If you need stricter per-year
  matching, filter the output afterward.

## Troubleshooting

- **Smoke test fails to download anything**: check that you're not behind a
  proxy/VPN blocking `nces.ed.gov`, and that `options(timeout=...)` is high
  enough for your connection (`utils::download.file()` defaults to 60
  seconds, which can be tight on the bigger tables).
- **"Could not find varTable/Tables sheets for year ..."**: NCES
  occasionally tweaks sheet naming in the documentation workbook. Open the
  file for that year and check the sheet names match the `varTable{yy}` /
  `Tables{yy}` pattern the code expects; file an issue in this repo with
  what you find.
- **A variable you expected is missing from a year**: `build_ipeds_panel()`
  warns (rather than errors) when a requested variable doesn't exist in a
  given year's file, and fills NA instead. Check the warning text — IPEDS
  genuinely adds/retires/renames variables across years.
