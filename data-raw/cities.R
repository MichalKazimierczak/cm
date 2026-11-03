# Builds data/cities.rda from GeoNames (https://www.geonames.org, CC BY 4.0).
#
# Source: cities1000.zip, all populated places with 1,000 or more inhabitants (and seats of
# administrative divisions), https://download.geonames.org/export/dump/. The first version of
# the list was built from the same file (99.7% of its city names are in it).
#
# Countries: the 26 EU countries of the first version, plus Greece (GR, missing in the first
# version) and the United Kingdom (GB). GeoNames uses ISO 3166-1 codes (GR, GB), not the
# Eurostat codes (EL, UK).
#
# For each place, the names are its ASCII name, its name and its alternate names, with
# - notes in square brackets removed ("Virteburh [a. 704]" gives "Virteburh"), names marked
#   "[IPA]" (phonetic transcriptions) left out;
# - alternate names without a capital letter left out: GeoNames adds them by transliterating
#   names in other scripts automatically ("cai ci", "wa te lin gen"), and they gave junk keys;
# - each name cleaned as harmonize_cities() cleans its input (Latin, Greek and Cyrillic letters
#   kept, everything else turned into spaces, Latin letters converted to ASCII, capitals);
# - names of fewer than 2 characters, and names a place has twice, left out.
#
# Rows are sorted official names first (o), then by length of the name (l), longest first,
# then by population, largest first: when places share a name, city_lookup() keeps the first
# row, so the largest place wins.
#
# Run from the package root: Rscript data-raw/cities.R. It downloads the file into a temporary
# folder and writes data/cities.rda; record the date of the run in R/data.R.

library(stringi)
library(stringr)

countries <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", "FR", "GR", "HR", "HU", "IE",
               "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK", "GB")

tmp <- tempfile(fileext = ".zip")
download.file("https://download.geonames.org/export/dump/cities1000.zip", tmp, mode = "wb", quiet = TRUE)
cols <- c("geonameid", "name", "asciiname", "alternatenames", "latitude", "longitude", "feature_class",
          "feature_code", "country_code", "cc2", "admin1", "admin2", "admin3", "admin4", "population",
          "elevation", "dem", "timezone", "modification_date")
g <- utils::read.delim(unz(tmp, "cities1000.txt"), header = FALSE, col.names = cols, quote = "",
                       comment.char = "", na.strings = "", encoding = "UTF-8", colClasses = "character")
g <- g[g$country_code %in% countries, ]
g$population <- as.numeric(g$population)

clean <- function(x) {
  x <- str_replace_all(x, "[^\\p{Latin}\\p{Greek}\\p{Cyrillic}]", " ")
  x <- str_squish(x)
  stri_trans_general(x, "latin-ascii; upper")
}

## one row per name of a place: the ASCII name (official), the name, and the alternate names
alt <- strsplit(ifelse(is.na(g$alternatenames), "", g$alternatenames), ",", fixed = TRUE)
long <- data.frame(
  i = c(seq_len(nrow(g)), seq_len(nrow(g)), rep(seq_len(nrow(g)), lengths(alt))),
  raw = c(g$asciiname, g$name, unlist(alt, use.names = FALSE)),
  official = c(rep(TRUE, nrow(g)), rep(FALSE, nrow(g)), rep(FALSE, sum(lengths(alt)))),
  generated = c(rep(FALSE, 2 * nrow(g)), !grepl("\\p{Lu}", unlist(alt, use.names = FALSE), perl = TRUE)),
  stringsAsFactors = FALSE
)
long <- long[!long$generated & !grepl("[IPA]", long$raw, fixed = TRUE), ]
long$raw <- str_remove_all(long$raw, "\\s*\\[[^]]*\\]")
long$key <- clean(long$raw)
long <- long[nchar(long$key) >= 2, ]

g$ascii <- clean(g$asciiname)
long$ASCII.Name <- g$ascii[long$i]
long$o <- long$key == long$ASCII.Name
long <- long[order(long$i, !long$o), ]
long <- long[!duplicated(long[, c("i", "key")]), ]

cities <- data.frame(
  ASCII.Name = long$ASCII.Name,
  Alternate.Names = paste0("(^| )", long$key, "( |$)"),
  Country.Code = g$country_code[long$i],
  o = long$o,
  l = nchar(long$key),
  geonameid = as.integer(g$geonameid[long$i]),
  population = g$population[long$i],
  stringsAsFactors = FALSE
)
cities <- cities[order(!cities$o, -cities$l, -cities$population, cities$geonameid), ]
rownames(cities) <- NULL

message("places: ", nrow(g), ", rows: ", nrow(cities), ", latest GeoNames modification: ",
        max(g$modification_date), ", downloaded: ", format(Sys.Date()))
save(cities, file = "data/cities.rda", compress = "xz", version = 3)
