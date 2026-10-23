---
title: CleanMatch bug log
tags:
  - cleanmatch
  - bug-log
  - r-package
created: 2026-10-22
updated: 2026-10-23
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
- [-] 🔽 `nmatch`: words are pasted without a separator ➕ 2026-10-22 ❌ 2026-10-23
	- `"AB C"` and `"A BC"` get the same key `ABC`.
	- Not a bug: intended, so that spacing variants match (Michal, 2026-10-23). Worth one sentence in the `nmatch()` documentation (`@details`).

- [x] ⏫ `cities` / city lookup: notes such as `[A ]` in historical names were read as regex ➕ 2026-10-22 ✅ 2026-10-22
	- Tested: 94 German alternate names end in a note: `[A ]` (88), `[ A ]`, `[~ A ]`, `[~A ]` and `[IPA]`; for example `WIRZIBURG [A ]` (Wuerzburg), `GERSBEKE[A ]` (Gorsbach), `LEIVƏNƆ: [IPA]` (Liebenau).
	- Probably "[a. 704]" (*anno*) or "[~a. 1000]" with digits and full stop removed when the list was cleaned; not confirmed against the source.
	- 0.1.0 read `[A ]` as a regex class, so `WIRZIBURG [A ]` matched only "WIRZIBURG A", never "WIRZIBURG".
	- Fixed in commit `a9ed7bf`: `city_lookup()` keeps the name before the note and drops the `[IPA]` entry; `detect_address()` removes the matched key. About 90 old spellings now match. Three stripped names already exist as plain names (BUCKEBORCH, same city; NIENHAGEN and BARDORP, other cities listed earlier, which keep priority).

- [ ] ⏫ `detect_region`: a postal code is found inside longer numbers and words ➕ 2026-10-22
	- Tested on the real list: `556269` gives `DEB15, DEB18` (contains `55626` and `56269`); `647119` gives `DE71B, DEA12`; `803312` gives `DE212`; NL `1012 ABC` gives `NL32B` (contains `1012 AB`). House or phone numbers in a zip column can give wrong regions.
	- Cause: the code is searched anywhere in the value, as `str_detect()` did in 0.1.0.
	- Proposed fix (in the loop over code lengths): a piece counts only when the character before and after it is not of the same kind (digit or letter) as the code's first and last character:
		```r
		piece<-substr(u[id],st,st+len-1)
		kind<-function(x) ifelse(grepl("[0-9]",x),"digit",ifelse(grepl("[A-Z]",x),"letter","other"))
		before<-kind(substr(u[id],st-1,st-1))
		after<-kind(substr(u[id],st+len,st+len))
		alone<-before!=kind(substr(piece,1,1)) & after!=kind(substr(piece,len,len))
		m<-match(piece,names(rows))
		m[!alone]<-NA
		```
	- Trial (not committed): `80331`, `D-80331`, `D80331`, `PLZ:80331`, `80331 Muenchen`, `1012 AB`, `NL-1012 AB` keep their region; `803312`, `556269`, `1012 ABC` give `""`. The 0.1.0 fixtures for DE, CY and PL do not change.
	- Make together with the two tasks below; add tests for `803312`, `556269` and `1012 ABC`, and a NEWS entry.
- [ ] 🔼 `detect_region`: the same region is pasted twice ➕ 2026-10-22
	- Tested: `80331 80333` gives `DE212, DE212` (two codes, one region).
	- Proposed fix: keep each region once per value (`unique()` on the regions before `paste`).
- [ ] 🔼 `detect_region`: Dutch codes written without the space are not found ➕ 2026-10-22
	- Tested: `1012AB` gives `""`; the list stores `1012 AB`.
	- Proposed fix: for NL, insert the space before the lookup (`1012AB` → `1012 AB`), for example `stringr::str_replace_all(u,"\\b([0-9]{4}) ?([A-Z]{2})\\b","\\1 \\2")`. Check that `1012 ABC` still gives `""`.

- [ ] ⏫ `clean`: the fallback to `orb_reg` is skipped when the postal code of A is missing ➕ 2026-10-23
	- Tested: a pair with `zip_a = NA`, `orb_reg = "DE300"` and B in DE300 gets `reg_a = NA` and `reg_sim = FALSE`, although both are in DE300.
	- Cause: `detect_region()` returns `""` for a postal code not in the list but `NA` for a missing one; the fallback `ifelse(r$reg_a=="",r[,orb_reg],r$reg_a)` only catches `""`, because `NA == ""` is `NA`.
	- Effect: true matches whose A record has no postal code lose the `reg_sim` part of their score in `rankmatch()`.
	- Proposed fix: `r$reg_a<-ifelse(is.na(r$reg_a)|r$reg_a=="",r[,orb_reg],r$reg_a)`; update the `clean` fixture and add a NEWS entry.

- [ ] ⏫ `clean`: similarities are Jaro, not Jaro-Winkler, and `p` cannot be set ➕ 2026-10-23
	- Tested: `stringsim(..., method = "jw")` uses the default `p = 0`, which is pure Jaro similarity. Jaro-Winkler, which adds a bonus for a common start (useful for names), needs `p > 0`, usually `0.1`: `MUNCHEN` / `MUENCHEN` scores 0.863 with `p = 0` and 0.890 with `p = 0.1`.
	- `p` is hard-coded: `clean()` has no argument for it, so users cannot choose. Affects `name_sim`, `lf_sim`, `city_sim` and `street_sim` (four `stringsim()` calls).
	- The comment above `name_sim` says "Leventshein distance", a third method.
	- Proposed fix: add an argument `p = 0` to `clean()` and pass it to the four `stringsim()` calls; document it (`p = 0.1` for Jaro-Winkler, maximum 0.25). The default `p = 0` keeps today's results; decide whether to change the default to 0.1, which changes scores. Correct the comment.

- [ ] ⏫ `norma_legal`: weak words are removed only once per rule ➕ 2026-10-23
	- Tested (DE): `DIE DELTA HOLDING GMBH` becomes `DELTA HOLDING` (`DIE` removed, `HOLDING` kept); `DELTA GROUP HOLDING GMBH` becomes `DELTA HOLDING`; `MUELLER BAECKEREI GMBH` becomes `MULLER BAECKEREI` (only the first `UE` converted).
	- Cause: the weak-word loop uses `str_replace()`, which replaces only the first match, and each rule is one long pattern (`GROUP|HOLDING|DIE|...`). Same in 0.1.0.
	- Effect: names of the same company from two sources normalise differently when they hold a different number of weak words or umlaut spellings, so exact matching in `nmatch()` can miss them.
	- Proposed fix: `str_replace_all()` in the weak-word loop; check the rules whose replacement is not empty (DE `(U|A|O)E` → `\\1`) still give the intended result; update the `norma` and `norma_legal` fixtures and add a NEWS entry.

- [ ] ⏫ `nmatch`: a missing name matches every empty name ➕ 2026-10-23
	- Tested: with `sort = TRUE` (the default), an A record with name `NA` is paired with a B record with name `""` (key `""` on both sides).
	- Cause: `sort()` drops `NA`, so a missing name becomes an empty word list and the key `""`, the same as an empty name. With `sort = FALSE` the key is the text `"NA"` and this does not happen.
	- Effect: every A record without a name pairs with every B record with an empty name (1,000 × 1,000 gives a million false pairs), which also slows down `clean()` and `rankmatch()`.
	- Proposed fix: drop rows whose key is `""` or whose name is `NA` before `merge()` (or give them a key that cannot match); update the `nmatch` fixture and add a NEWS entry.

## Can crash or corrupt your session

- [ ] 🔼 All functions that call `data()`: package data overwrite objects in your global environment ➕ 2026-10-22
	- Tested: after `norma()`, the objects `l` and `w` exist in the global environment. `data()` loads into the global environment by default; the functions also overwrite `cities`, `zips` and `pc`.
	- Fix: with `LazyData: true`, refer to the data directly (`cities`, or `CleanMatch::cities`), and give the objects the names of their files (`zip_codes` holds `zips`, `postal_codes_regex` holds `pc`, `legal_forms_regex` holds `l`, `weak_words_regex` holds `w`).
- [ ] 🔼 `clean`: fails when a key column is named `key_a` or `key_b` ➕ 2026-10-22
	- Tested: `get(key_a)` inside `data.table` finds the column, not the argument: "invalid first argument".
	- Fix: `data.table(d)[, .(n = uniqueN(.SD[[1]])), by = key_b, .SDcols = key_a]`, or base R `tapply`.
- [ ] 🔼 `harmonize_cities`: wrong name for the new columns ➕ 2026-10-22
	- Tested: `str_detect(city_a, colnames(r))` has its arguments swapped; it selects every column whose name appears inside `city_a`, and the first one gives the name.
	- With columns `city` and `city_orbis`, both new columns are named `city_harmonized`; `r$city_harmonized` then returns only the first, so the second is silently lost, and a later `clean(..., "city_harmonized", "city_harmonized", ...)` would compare a column with itself (city similarity 1 for every pair).
	- With an extra column `a` (before `city_a`), the new column is named `a_harmonized`. The only sign is the warning "number of items to replace is not a multiple of replacement length".
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

- [ ] 🔽 `nmatch`: the comment at the top (lines 16 to 19) about transliteration standards was copied from `norma_trans()`; replace it with one that describes `nmatch()` ➕ 2026-10-23
- [ ] 🔽 `DESCRIPTION`: `stringdist` and `plyr` are used but missing from `Imports` (`R CMD check` stops with an ERROR) ➕ 2026-10-22
- [x] 🔽 `DESCRIPTION`: `License:` was empty (`R CMD check` stopped with an ERROR) ➕ 2026-10-22 ✅ 2026-10-23
	- Set to `EUPL (>= 1.2)`, full text in `LICENSE.md`. R 4.3 accepts `EUPL (>= 1.2)` but not `EUPL-1.2`. Copyright holder (probably EUIPO) still to confirm with EUIPO.
- [ ] 🔽 Examples: empty in `clean`, `rankmatch`, `harmonize_cities`; the others use objects that do not exist (`df`, `x`, `y`). `R CMD check` runs them ➕ 2026-10-22
- [ ] 🔽 `@return` texts copied from `rankmatch` ("ordered and ranked matched dataframe") in `harmonize_cities`, `detect_address` and others ➕ 2026-10-22
- [ ] 🔽 `detect_address` and `detect_region` are not exported; decide whether they are internal ➕ 2026-10-22
- [ ] 🔽 Document the datasets in `R/data.R` (`@format`, `@source`, version of GeoNames and NUTS) ➕ 2026-10-22
- [ ] 🔽 `cities` data: 2,272 keys point to more than one city in the same country (AT `ALTENMARKT`: 4) and the first row wins silently; some entries are junk (a Wikipedia URL) ➕ 2026-10-22
- [ ] 🔽 The regexes need a UTF-8 R session (Greek and Cyrillic ranges); R 4.2 or later on Windows is UTF-8. Say so in the README, or write the ranges as `\\u` escapes ➕ 2026-10-22
- [ ] 🔽 `norma_trans`: `if(nrow(r_trans!=0))` has the bracket in the wrong place; it works by accident ➕ 2026-10-22
- [ ] 🔽 `clean`: the `ifelse(is.na(...))` lines for `city_sim` and `street_sim` appear twice ➕ 2026-10-22
- [ ] 🔽 `.Rhistory` and `.Rproj.user/` are tracked in git; add them to `.gitignore` ➕ 2026-10-22
