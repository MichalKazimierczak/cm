#' City names and their alternate names
#'
#' One row per name of a city: its official ASCII name and each alternate name (translations, transliterations,
#' historical spellings), as a whole-word regex pattern. Used by \code{\link{harmonize_cities}} and \code{detect_address()}.
#'
#' Rows are sorted official names first (\code{o}), then by length of the name (\code{l}), longest first; the first
#' pattern that matches wins.
#'
#' @format A tibble with 184,546 rows (51,301 cities in 26 EU countries; Greece is not included) and 5 columns:
#' \describe{
#'   \item{ASCII.Name}{official name of the city, in ASCII capitals}
#'   \item{Alternate.Names}{one name of the city as a pattern \code{(^| )NAME( |$)}, in capitals}
#'   \item{Country.Code}{ISO 3166-1 alpha-2 country code}
#'   \item{o}{\code{TRUE} when the pattern holds the official name}
#'   \item{l}{number of characters of the name in the pattern}
#' }
#' @source GeoNames (\url{https://www.geonames.org}), licensed under the Creative Commons Attribution 4.0
#'   International licence (CC BY 4.0, \url{https://creativecommons.org/licenses/by/4.0/}).
#'   Downloaded [date to confirm] from [file to confirm, for example the export of cities with a population of
#'   1,000 or more].
#'
#'   Changed from the original: kept 26 EU countries, all except Greece [selection to confirm]; kept the official
#'   name, the alternate names and the country code; converted names to ASCII capitals; wrote each name as a
#'   whole-word regex pattern; added the columns \code{o} and \code{l}; sorted the rows.
"cities"

#' Postal codes and their NUTS 3 regions
#'
#' Correspondence between postal codes and NUTS 3 regions, used by \code{detect_region()} and \code{\link{clean}}. Load with
#' \code{data(zip_codes)}, which creates the object \code{zips}.
#'
#' A postal code can belong to more than one NUTS 3 region (36 codes do). Within a country all codes have the
#' same number of characters.
#'
#' @format A data frame with 805,026 rows and 3 columns, covering 27 EU countries:
#' \describe{
#'   \item{NUTS3}{NUTS 3 region code}
#'   \item{CODE}{postal code, as written in the source (for example \code{80331} for DE, \code{1012 AB} for NL)}
#'   \item{country_code}{country code as used by Eurostat (\code{GR} for Greece)}
#' }
#' @source Eurostat, GISCO: postal codes to NUTS correspondence tables (TERCET),
#'   \url{https://ec.europa.eu/eurostat/web/gisco/geodata/administrative-units/postal-codes}.
#'   Copyright European Union, Eurostat; reuse is authorised provided the source is acknowledged
#'   (Commission Decision 2011/833/EU). NUTS version [to confirm], file version [to confirm], downloaded
#'   [date to confirm]. Check the conditions for individual countries on the GISCO download page.
#'
#'   Changed from the original: files of all countries combined, columns \code{NUTS3}, \code{CODE} and \code{country_code}
#'   kept.
#' @name zips
#' @aliases zip_codes
#' @usage data(zip_codes)
NULL

#' Postal code formats per country
#'
#' A regex for the postal code of each country and the format to write it in, used by \code{detect_address()} to find
#' and cut out the postal code. Load with \code{data(postal_codes_regex)}, which creates the object \code{pc}.
#'
#' @format A data frame with 36 rows and 3 columns:
#' \describe{
#'   \item{country}{country code}
#'   \item{expres}{regex matching a postal code of that country, with capture groups}
#'   \item{forma}{replacement that writes the code in the standard format, using the capture groups}
#' }
#' @source Compiled for CleanMatch; part of the package, licensed under the EUPL 1.2.
#' @name pc
#' @aliases postal_codes_regex
#' @usage data(postal_codes_regex)
NULL

#' Legal forms per country
#'
#' Regex patterns for the legal forms of companies in each country, with their standard abbreviation, used by
#' \code{\link{norma_legal}} to remove the legal form from a name and store it in the column \code{lf}. Load with
#' \code{data(legal_forms_regex)}, which creates the object \code{l}.
#'
#' Patterns are applied in the order of \code{order}, so longer forms (\code{GEMEINNUETZIGE AKTIENGESELLSCHAFT}) come before
#' shorter ones they contain (\code{AKTIENGESELLSCHAFT}).
#'
#' @format A data frame with 671 rows (38 countries) and 4 columns:
#' \describe{
#'   \item{Country}{country code}
#'   \item{to.replace}{regex matching the legal form in a normalised name}
#'   \item{replacement}{standard abbreviation of the legal form, for example \code{GMBH}, \code{AG}, \code{SPZOO}}
#'   \item{order}{order in which the patterns are applied}
#' }
#' @source Compiled for CleanMatch by Michal Kazimierczak; part of the package, licensed under the EUPL 1.2.
#' @name l
#' @aliases legal_forms_regex
#' @usage data(legal_forms_regex)
NULL

#' Weak words per country
#'
#' Regex patterns for words that carry little information in company names (\code{GROUP}, \code{HOLDING}, articles,
#' conjunctions, country names), with their replacement, used by \code{\link{norma_legal}} after the legal forms are
#' removed. Load with \code{data(weak_words_regex)}, which creates the object \code{w}.
#'
#' @format A data frame with 87 rows (39 countries) and 4 columns:
#' \describe{
#'   \item{Country}{country code}
#'   \item{to.replace}{regex matching the weak words}
#'   \item{replacement}{text that replaces them, usually empty or a space}
#'   \item{order}{order in which the patterns are applied}
#' }
#' @source Compiled for CleanMatch by Michal Kazimierczak; part of the package, licensed under the EUPL 1.2.
#' @name w
#' @aliases weak_words_regex
#' @usage data(weak_words_regex)
NULL
