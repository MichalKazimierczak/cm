# CleanMatch

CleanMatch normalises company names from different sources (legal forms, 'trading as' phrases, transliteration),
matches them, and scores the candidate matches with names, legal forms, cities, streets and regions.

## Installation

```r
remotes::install_github("MichalKazimierczak/cm")
```

The package needs an R session that uses UTF-8 (the default in R 4.2 and later, also on Windows).

## Licence

The code is licensed under the European Union Public Licence, version 1.2 or later (`LICENSE.md`).
The bundled datasets keep the licences of their sources:

| Dataset | Source | Licence |
|---|---|---|
| `cities` | [GeoNames](https://www.geonames.org), changed (see `?cities`) | [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/) |
| `zip_codes` | Eurostat, GISCO: [postal codes to NUTS correspondence tables (TERCET)](https://ec.europa.eu/eurostat/web/gisco/geodata/administrative-units/postal-codes) | © European Union, Eurostat; reuse with acknowledgement of the source |
| `legal_forms_regex`, `weak_words_regex`, `postal_codes_regex` | compiled for CleanMatch | EUPL 1.2, as the code |

Details, versions and the changes made to each source are in the help pages (`?cities`, `?zips`).
