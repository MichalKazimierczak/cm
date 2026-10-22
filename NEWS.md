# CleanMatch (development version)

## Performance

These changes keep the output of 0.1.0; the tests in `tests/testthat/` check it.

* `harmonize_cities()` looks city names up in a table instead of running one regex per city of the
  country over all rows (41,031 for DE). It also cleans each distinct name once. About 100 times
  faster on 1,000 rows; 1 million rows of DE take seconds instead of an estimated 12 hours.

* `detect_address()` uses the same lookup as `harmonize_cities()` to find the city in each distinct
  address, then removes it once. About 100 times faster on 500 PL addresses.

* `detect_region()` looks up the pieces of each distinct value that are as long as a postal code,
  instead of testing every postal code of the country (462,780 for NL) on every value. 45 to 100
  times faster; 1 million NL codes take about 5 seconds. `clean()` calls it once for both zip columns.
  For a country with no postal codes it now stops with a clear message; 0.1.0 also stopped, with an
  unclear one (or, for a single value, returned `character(0)`).
