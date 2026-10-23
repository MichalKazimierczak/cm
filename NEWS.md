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
  normalise alike more often. This includes the rule for umlauts written as `UE`, `AE`, `OE` (see the
  next entries), which 0.1.0 applied only to the first occurrence in a name.

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

* `harmonize_cities()` names the new columns `<city_a>_harmonized` and `<city_b>_harmonized` in all
  cases. It picked every column whose name appears inside `city_a`, so with columns `city` and
  `city_orbis` both new columns were called `city_harmonized` (one result could no longer be reached by
  name), and an extra column `a` gave `a_harmonized`.

* `clean()` uses the region from dataset A (`orb_reg`) whenever the postal code of A gives no region,
  also when the postal code is missing. Before, only codes not found in the list fell back to `orb_reg`;
  a missing code left `reg_a` as `NA` and `reg_sim` as `FALSE`, lowering the score of true matches in
  `rankmatch()`.

* The Greek weak words in `weak_words_regex` are removed only as whole words. Six rules had no word
  boundaries and one began with `\H` instead of `\b`, so they cut parts out of longer words:
  `ATHINAIKI ZYTHOPOIIA` gave `IKI ZYTHOPOIIA` (now `ZYTHOPOIIA`), `GREEKFOOD` gave `FOOD`, `ANELLADA`
  gave `A`. The change is made by `data-raw/weak_words_gr_boundaries.R`.

* The weak-word rule that turns `UE`, `AE` and `OE` into `U`, `A` and `O` (so that `MUELLER`, `MÜLLER`
  and `MULLER` all give `MULLER`) now applies in the German-speaking countries DE, AT, CH and LI. It is
  new for CH and LI, and removed for BG, where Latin-script names have no umlaut spellings and it only
  shortened them (`BOEV` gave `BOV`). LU is left out because its rules are also used for BX (Benelux),
  where Dutch names would be changed (`HOEK` would give `HOK`). The change is made by
  `data-raw/weak_words_umlaut_countries.R`.

* The umlaut rule no longer shortens the most common German words and names where `UE`, `AE`, `OE`
  are not umlauts. `UE` stays after `A`, `E` and `Q` (`BAUER`, `NEUE`, `FEUER`, `STEUER`, `QUELLE`) and in a
  final `-UEL` (`SAMUEL`, `MANUEL`); `AE` stays in a final `-AEL` (`MICHAEL`, `ISRAEL`, `RAFAEL`). Umlauts
  written as `UE`, `AE`, `OE` still match umlauts already removed (`MUELLER`, `MÜLLER` and `MULLER` all give
  `MULLER`; `GUENTHER` gives `GUNTHER`; `MINERALOEL` gives `MINERALOL`). Rare cases such as `POET`,
  `BLUE` or `MICHAELA` are still shortened. The change is made by `data-raw/weak_words_umlaut_exceptions.R`.

* `detect_region()` gives no region (`""`, or `NA` for a missing code) for a country without postal codes
  in `zip_codes`, such as CH or GB, instead of stopping. `clean()` therefore works for these countries
  and uses the region from dataset A (`orb_reg`); before, it stopped with an error.

* `detect_region()` gives each region once per value. `80331 80333` (two codes in DE212) gave
  `DE212, DE212`, now `DE212`; in Cyprus, where many codes are listed twice, `CY000, CY000` is now `CY000`.

* `detect_region()` finds Dutch postal codes written without the space or in lower case (`1012AB`,
  `1012ab`), which the list stores as `1012 AB`. `1012ABC` still gives no region.

* `cities` is rebuilt from a fresh GeoNames download (`cities1000.zip`, 23 October 2026) by
  `data-raw/cities.R`, which documents every step. It now covers the 27 EU countries and the United Kingdom
  (61,576 places): Greece (`GR`) and the United Kingdom (`GB`) are new, so `harmonize_cities()` and
  `detect_address()` work for them; they stopped with an error before. Other changes:
  - Alternate names that GeoNames transliterated automatically from other scripts (`lu sen bao cheng`,
    `wa te lin gen`) are left out; they gave junk patterns such as `TI` or `WL Z` that matched short words
    in any address.
  - Cyrillic names keep all their letters (the old list had lost `і`, `ї`, `є`: `Н ДЕРАНВЕН`).
  - When places share a name, the one with the largest population wins (before, whichever row came first).
  - Some official names now follow GeoNames' current spelling, often the English form: `PADOVA` is now
    `PADUA`, `NUERNBERG` is `NUREMBERG`, `ANTWERPEN` is `ANTWERP`.
  Of 144,041 names in both the old and the new list, 0.66% now give a different city, mostly because of these
  new official names. The list has two new columns, `geonameid` and `population`.

## Bug fixes

* `harmonize_cities()`, `detect_address()`, `detect_region()`, `norma_legal()` (and so `norma()` and
  `clean()`) no longer create or overwrite objects in your global environment. They called `data()`,
  which loads into the global environment, so an object of yours named `cities`, `zips`, `pc`, `l` or `w`
  was replaced by the package data. They now read the data with `CleanMatch::cities` and so on.
  `data(zip_codes)` and the other `data()` calls still work as before when you use them yourself.

## Packaging

* `DESCRIPTION` lists every package the code uses (`stringdist`, `plyr` and `utils` added; `dplyr` removed,
  it was not used). The package no longer imports whole packages, so loading it no longer warns about
  "replacing previous import" (`first`, `last`, `between`, `extract`).

## Performance

These changes on their own keep the output of 0.1.0; the tests in `tests/testthat/` checked it when they were
made, before the changes above.

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
