# CleanMatch (development version)

## Performance

These changes keep the output of 0.1.0; the tests in `tests/testthat/` check it.

* `harmonize_cities()` looks city names up in a table instead of running one regex per city of the
  country over all rows (41,031 for DE). It also cleans each distinct name once. About 100 times
  faster on 1,000 rows; 1 million rows of DE take seconds instead of an estimated 12 hours.
