# CleanMatch

CleanMatch normalises company names from different sources (legal forms, 'trading as' phrases, transliteration),
matches them, and scores the candidate matches with names, legal forms, cities, streets and regions.

## Installation

From GitHub:

```r
remotes::install_github("MichalKazimierczak/cm")
```

From a package file (`CleanMatch_1.0.0.tar.gz`, no compiler or Rtools needed, also on Windows):

```r
install.packages(c("data.table", "plyr", "stringdist", "stringi", "stringr", "tidyr"))   # dependencies, once
install.packages("CleanMatch_1.0.0.tar.gz", repos = NULL, type = "source")
packageVersion("CleanMatch")   # 1.0.0
```

Names with accents, or in Greek or Cyrillic, need an R session that uses UTF-8 (the default in R 4.2 and
later, also on Windows; check with `l10n_info()$"UTF-8"`). In other sessions such names are read byte by byte
and do not match (`München` gives `M C BC NCHEN`). The package code itself is ASCII only.

## Checking the quality of matches

`match_quality()` adds flags to the matched pairs; it removes nothing, so you decide which matches to keep.

```r
q <- match_quality(pairs, "id_a", "id_b", "name_a", "name_b",
                   lf_a = "lf_a", lf_b = "lf_b", zip_a = "zip_a", zip_b = "zip_b",
                   city_a = "city_a", city_b = "city_b", street_a = "street_a", street_b = "street_b",
                   norm_a = norma_output_a, norm_b = norma_output_b,      # optional: stored norma() results
                   norm_key_a = "id_a", norm_key_b = "id_b",
                   type_a = "applicant_type", type_b = "psn_sector")      # optional: entity type columns
table(q$q_class)              # A confirmed, B plausible, C uncertain, D conflicting
quality_summary(q)            # pairs per class and type, and the reasons
```

Every flag is its own column (`q_conflict_city`, `q_key_freq_a`, `q_alias`, ...), so you can filter on the criteria
you trust; `?match_quality` explains each of them.

## Licence

The code is licensed under the European Union Public Licence, version 1.2 or later (`LICENSE.md`).
The bundled datasets keep the licences of their sources:

| Dataset | Source | Licence |
|---|---|---|
| `cities` | [GeoNames](https://www.geonames.org), `cities1000.zip` downloaded 23 October 2026, changed by `data-raw/cities.R` (see `?cities`) | [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/) |
| `zip_codes` | Eurostat, GISCO: [postal codes to NUTS correspondence tables (TERCET)](https://ec.europa.eu/eurostat/web/gisco/geodata/administrative-units/postal-codes) | © European Union, Eurostat; reuse with acknowledgement of the source |
| `legal_forms_regex`, `weak_words_regex`, `postal_codes_regex` | compiled for CleanMatch | EUPL 1.2, as the code |

Details, versions and the changes made to each source are in the help pages (`?cities`, `?zips`).
