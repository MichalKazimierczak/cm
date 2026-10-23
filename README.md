# CleanMatch

CleanMatch normalises company names from different sources (legal forms, 'trading as' phrases, transliteration),
matches them, and scores the candidate matches with names, legal forms, cities, streets and regions.

## Installation

```r
remotes::install_github("MichalKazimierczak/cm")
```

Names with accents, or in Greek or Cyrillic, need an R session that uses UTF-8 (the default in R 4.2 and
later, also on Windows; check with `l10n_info()$"UTF-8"`). In other sessions such names are read byte by byte
and do not match (`München` gives `M C BC NCHEN`). The package code itself is ASCII only.

## Licence

The code is licensed under the European Union Public Licence, version 1.2 or later (`LICENSE.md`).
The bundled datasets keep the licences of their sources:

| Dataset | Source | Licence |
|---|---|---|
| `cities` | [GeoNames](https://www.geonames.org), `cities1000.zip` downloaded 23 October 2026, changed by `data-raw/cities.R` (see `?cities`) | [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/) |
| `zip_codes` | Eurostat, GISCO: [postal codes to NUTS correspondence tables (TERCET)](https://ec.europa.eu/eurostat/web/gisco/geodata/administrative-units/postal-codes) | © European Union, Eurostat; reuse with acknowledgement of the source |
| `legal_forms_regex`, `weak_words_regex`, `postal_codes_regex` | compiled for CleanMatch | EUPL 1.2, as the code |

Details, versions and the changes made to each source are in the help pages (`?cities`, `?zips`).
