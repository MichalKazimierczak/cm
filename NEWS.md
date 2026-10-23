# CleanMatch (development version)

## Licence

* CleanMatch is licensed under the European Union Public Licence, version 1.2 or later (`LICENSE.md`).

## Changes that affect results

* `harmonize_cities()` and `detect_address()` now match 91 historical German city names that end in
  a note in `cities`, such as `WIRZIBURG [A ]` for Wuerzburg (probably "[a. 704]" with the date
  removed). 0.1.0 read the note as a regex class and matched only "WIRZIBURG A". The note is now
  dropped, and the one phonetic transcription marked `[IPA]` (Liebenau) is no longer used.

* `norma_legal()`, and so `norma()`, no longer drops rows from countries without a list of legal forms
  (for example US, CN or JP) or without a country. They are kept, after the other rows, with an empty
  legal form. 0.1.0 removed them without a warning.

* `norma_legal()`, and so `norma()`, removes every weak word of a rule, not only the first one in the
  name: `DIE DELTA HOLDING GMBH` now gives `DELTA` (0.1.0: `DELTA HOLDING`), and `MUELLER BAECKEREI`
  gives `MULLER BACKEREI` (0.1.0: `MULLER BAECKEREI`). Names of the same company from two sources now
  normalise alike more often.

* `norma()` and `norma_trans()` transliterate only names that contain Greek or Cyrillic letters. The
  test for "Greek or Cyrillic" (`[Α-ΩA-Я]`) began with a Latin `A`, so it covered every Latin capital and
  sent nearly every BG, CY and GR name through transliteration: `Maya Khristov Trading OOD` became
  `MAA HRISTOV TRADING`, now `MAYA KHRISTOV TRADING`.

* `harmonize_cities()`, `detect_address()` and `norma()` keep exactly the letters of the Latin, Greek and
  Cyrillic alphabets (`\\p{Latin}`, `\\p{Greek}`, `\\p{Cyrillic}`) and turn everything else into spaces.
  The old ranges (`a-я`, `A-Я`) also began with Latin letters: they kept symbols such as `°` and `©`, and
  dropped Cyrillic letters outside `а-я`, such as `ё`, `і` and `ј`, which the city list contains. For
  example `пфаффенгофен ан дер ільм` matched the wrong city (PFAFFENHOFEN), now PFAFFENHOFEN AN DER ILM.
  The R code is now pure ASCII.

* `nmatch()` leaves out rows with a missing or empty name. With `sort = TRUE` (the default) a missing
  name got the empty key and was paired with every empty name in the other dataset, which could create
  many false pairs. Matched pairs between real names are unchanged; their order within a key can differ.

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

* `norma_short()` splits only the names that contain a 'trading as' phrase. `ifelse()` ran the
  slow `(.*)phrase(.*)` replacement on every name. About 50 times faster; `norma()` on 20,000
  names went from 23 to 3.4 seconds.

* `norma_legal()` detects each legal-form pattern once instead of twice and replaces it only in
  the names where it was found; the weak-words step no longer runs `str_detect()`. About 2 times faster.

* `nmatch()` builds the matching key in one pass over the distinct names. About 5 times faster.
  It no longer fails when every name in `b` has the same number of words (0.1.0 stopped with
  "replacement has n rows").
