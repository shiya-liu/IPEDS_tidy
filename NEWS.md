# ipedstidy 0.1.1

* `download_ipeds_table()` tries NCES's current address first
  (`/ipeds/complete-data-files/<TABLE>.zip`), then the two older ones. The
  old `_Data_Stata.zip` address returns 404 for 2024 tables such as ADM2024
  and SFA2324. A response that is not a real zip (e.g. an HTML error page)
  is skipped, and partial files are removed. If nothing works, the error
  lists every address tried and links the IPEDS Complete Data Files page.
* New `ipeds_table_urls()` returns all candidate URLs in the order tried;
  `ipeds_table_url()` still returns one URL (now the current address).
* `build_ipeds_dictionary()` matches workbook sheet names ignoring
  capitalization. 0.1.0 failed on the 2022-23 (`vartable22`) and 2024-25
  (`tables24`) workbooks.
* Tables are read with every column as text first; column types are then
  set by looking at all values in each column. The cached CSV keeps NCES's
  values exactly, with blank for missing (0.1.0 wrote `NA`; old cache files
  still read correctly).
* New `download_log.csv` in the cache folder records each download: file,
  source URL, CSV used inside the zip, and date/time. Raw zips are still
  deleted after reading.
