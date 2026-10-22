---
title: CleanMatch bug log
tags:
  - cleanmatch
  - bug-log
  - r-package
created: 2026-10-22
updated: 2026-10-22
repository: MichalKazimierczak/cm
---

# CleanMatch bug log

Errors found while making CleanMatch faster (October 2026). The speed changes kept the 0.1.0 output,
bugs included; each fix below changes results and needs its own commit, a NEWS entry and a re-run
of `tests/testthat/fixtures/make_fixtures.R`.

Priority: ⏫ changes match results, 🔼 can crash or corrupt your session, 🔽 packaging and tidying.
"Tested" means reproduced in R; "from the code" means found by reading only.

## Change match results

- [ ] ⏫ `norma` / `norma_trans`: Latin-script names from BG, CY and GR go through transliteration ➕ 2026-10-22
	- Tested: `"Maya Khristov Trading OOD"` (BG) becomes `MAA HRISTOV TRADING`; `"Kiyashko Yulia EOOD"` becomes `KIASHKO ULIA`.
	- Cause: in `[Α-ΩA-Я]` the `A` is the Latin letter (U+0041), so the range runs from Latin A to Cyrillic Я and covers every Latin capital. Every BG name then gets the rules `YA→A`, `YU→U`, `IY→I`, `KH→H`.
	- Same mistake in `norma.R` lines 31, 32 and 85 and `norma_trans.R` line 22.
	- Fix: use Cyrillic `А-Я` (U+0410) and `а-я`, or `\\p{Cyrillic}` and `\\p{Greek}`.
- [ ] ⏫ `norma_short`: four 'trading as' phrases never match ➕ 2026-10-22
	- Tested: "OPERANDO COMO", "WITH DISTINCTIVE TITLE", "H O D N" (T H O D N) and "HODN" never split a name.
	- Cause: the pattern string breaks across lines, so the alternatives that start a line need a newline character before them; `"\bHODN\b"` in a normal R string holds backspace characters, not word boundaries.
	- Fix: build the pattern from a character vector, `paste(phrases, collapse = "|")`, and write `\\bHODN\\b`.
	- Also: the group starts with `(:?`, probably meant as `(?:`; it works only because the code counts it as group 2.
- [ ] ⏫ `norma_legal`: rows from countries without a legal-form list disappear ➕ 2026-10-22
	- Tested: of `DE`, `FR` and `US` rows, the `US` row is missing from the output.
	- Fix: after the country loop, add back the rows of `x` whose country is not in `lis_co`, with `lf = ""`.
- [ ] ⏫ `harmonize_cities`, `detect_address`, `norma`: character ranges `a-я` and `A-Я` start with Latin letters ➕ 2026-10-22
	- The class `[^a-zA-Zα-ωΑ-Ωa-яA-Я]` keeps most characters between U+0041 and U+044F, such as `°`, `©`, `×`, `¿`, not only letters.
	- Fix: use Cyrillic `а-яА-Я`; decide whether accented Latin letters should be kept (they are now, which helps the later `latin-ascii` step), and if so name them explicitly (`\\p{L}`).
- [ ] ⏫ `detect_address`: removing the city glues the words around it ➕ 2026-10-22
	- From the code: the pattern `(^| )CITY( |$)` is replaced by `""`, so both spaces go: `"... 5 MUNICH CENTER"` would become `"... 5CENTER"` in the street column.
	- Fix: replace with `" "` and squish the result.
- [ ] ⏫ `clean`: several countries in one call mix up regions ➕ 2026-10-22
	- From the code: `cc <- unique(r[, cc])` can hold several codes; `detect_region()` then compares `zips$country_code == cc` with recycling.
	- Fix: call `detect_region()` per country, or stop when there is more than one.
- [ ] 🔼 `detect_region`: countries without postal codes stop `clean()` ➕ 2026-10-22
	- Tested: for CH (and any code not in `zip_codes`, such as GB or BX) the function stops; 0.1.0 also stopped, or returned `character(0)` for a single value.
	- Fix: return `""` for every value, so `clean()` falls back to `orb_reg`.
- [ ] 🔽 `nmatch`: words are pasted without a separator ➕ 2026-10-22
	- `"AB C"` and `"A BC"` get the same key `ABC`. Decide whether this is wanted (it catches spacing variants) or use `collapse = " "`.

## Can crash or corrupt your session

- [ ] 🔼 All functions that call `data()`: package data overwrite objects in your global environment ➕ 2026-10-22
	- Tested: after `norma()`, the objects `l` and `w` exist in the global environment. `data()` loads into the global environment by default; the functions also overwrite `cities`, `zips` and `pc`.
	- Fix: with `LazyData: true`, refer to the data directly (`cities`, or `CleanMatch::cities`), and give the objects the names of their files (`zip_codes` holds `zips`, `postal_codes_regex` holds `pc`, `legal_forms_regex` holds `l`, `weak_words_regex` holds `w`).
- [ ] 🔼 `clean`: fails when a key column is named `key_a` or `key_b` ➕ 2026-10-22
	- Tested: `get(key_a)` inside `data.table` finds the column, not the argument: "invalid first argument".
	- Fix: `data.table(d)[, .(n = uniqueN(.SD[[1]])), by = key_b, .SDcols = key_a]`, or base R `tapply`.
- [ ] 🔼 `harmonize_cities`: wrong name for the new columns ➕ 2026-10-22
	- From the code: `str_detect(city_a, colnames(r))` has its arguments swapped; it selects every column whose name appears inside `city_a`. With columns `city` and `city_a`, or a column `a`, the names come out wrong.
	- Fix: `paste0(city_a, "_harmonized")`.
- [ ] 🔼 `detect_address`: deletes your columns named `city` or `miasto` ➕ 2026-10-22
	- From the code: it uses both as scratch columns and removes them at the end.
	- Fix: keep the values in local vectors.
- [ ] 🔼 `harmonize_cities`, `detect_address`, `clean`, `norma`: only plain data frames work ➕ 2026-10-22
	- `r[, col]` returns a vector only for a `data.frame`; tibbles and `data.table`s break. `norma` and `clean` convert with `data.frame(r)`, the others do not.
	- Fix: use `r[[col]]`, or convert at the start.
- [ ] 🔼 `rankmatch`: needs a `bvd_is_root` column that no function creates ➕ 2026-10-22
	- From the code: without it the score is `numeric(0)` and the function fails. Document it as an Orbis-specific input, or skip the term when the column is missing.
- [ ] 🔼 `norma` with `short = TRUE` on names with several phrases ➕ 2026-10-22
	- From the code: `(.*)` is greedy, so only the last 'trading as' phrase splits the name. Decide whether that is wanted.
- [x] 🔼 `nmatch`: fails when every name in `b` has the same number of words ➕ 2026-10-22 ✅ 2026-10-22
	- Tested on 0.1.0: "replacement has 4 rows, data has 2". `sapply()` turned the word lists into a matrix.
	- Fixed by the speed change in commit `1b021e3` (`vapply` instead of `sapply`).

## Packaging and tidying

- [ ] 🔽 `DESCRIPTION`: `License:` is empty; `stringdist` and `plyr` are used but missing from `Imports` ➕ 2026-10-22
- [ ] 🔽 Examples: empty in `clean`, `rankmatch`, `harmonize_cities`; the others use objects that do not exist (`df`, `x`, `y`). `R CMD check` runs them ➕ 2026-10-22
- [ ] 🔽 `@return` texts copied from `rankmatch` ("ordered and ranked matched dataframe") in `harmonize_cities`, `detect_address` and others ➕ 2026-10-22
- [ ] 🔽 `detect_address` and `detect_region` are not exported; decide whether they are internal ➕ 2026-10-22
- [ ] 🔽 Document the datasets in `R/data.R` (`@format`, `@source`, version of GeoNames and NUTS) ➕ 2026-10-22
- [ ] 🔽 `cities` data: 2,272 keys point to more than one city in the same country (AT `ALTENMARKT`: 4) and the first row wins silently; some entries are junk (a Wikipedia URL, regex classes such as `[A ]`) ➕ 2026-10-22
- [ ] 🔽 The regexes need a UTF-8 R session (Greek and Cyrillic ranges); R 4.2 or later on Windows is UTF-8. Say so in the README, or write the ranges as `\\u` escapes ➕ 2026-10-22
- [ ] 🔽 `norma_trans`: `if(nrow(r_trans!=0))` has the bracket in the wrong place; it works by accident ➕ 2026-10-22
- [ ] 🔽 `clean`: the `ifelse(is.na(...))` lines for `city_sim` and `street_sim` appear twice ➕ 2026-10-22
- [ ] 🔽 `.Rhistory` and `.Rproj.user/` are tracked in git; add them to `.gitignore` ➕ 2026-10-22
