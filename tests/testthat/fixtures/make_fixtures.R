# Builds the regression fixtures: fixed inputs and the output CleanMatch 0.1.0
# gave for them. Run from the package root with the 0.1.0 code loaded:
#   pkgload::load_all(); source("tests/testthat/fixtures/make_fixtures.R")
# Re-run it only when a change is meant to alter output, and say so in NEWS.md.

out <- "tests/testthat/fixtures"
set.seed(2026)

stems <- c("Muller", "Kowalski", "Rossi", "Dupont", "Garcia", "Nowak", "Bianchi", "Schmidt",
           "Alpha", "Nord", "Euro", "Tech", "Bau", "Food", "Trans", "Konsult", "Invest")
lf <- c(DE = "GmbH", DE = "AG", DE = "GmbH & Co. KG", AT = "GesmbH", PL = "sp. z o.o.",
        PL = "S.A.", FR = "SARL", FR = "SAS", IT = "S.r.l.", ES = "S.L.", BX = "B.V.", SE = "AB publ")
ta <- c("trading as", "handelnd unter", "in sigla", "nazwa skrocona", "dite", "in breve")
i <- sample(length(lf), 300, TRUE)
name <- paste(sample(stems, 300, TRUE), sample(stems, 300, TRUE),
              sample(c("", "Holding", "Group", "und Partner", "Gesellschaft fur"), 300, TRUE), lf[i])
k <- sample(300, 30)
name[k] <- paste(name[k], sample(ta, 30, TRUE), sample(stems, 30, TRUE), "Ltd")
companies <- data.frame(key = 1:300, name = name, country = names(lf)[i], stringsAsFactors = FALSE)

# norma (end to end) and its parts, on the input each part receives
x <- list(input = companies)
x$output <- norma(companies, "name", "country", "key")
saveRDS(x, file.path(out, "norma.rds"))

up <- companies
up$name <- toupper(up$name)
up$name[c(3, 7)] <- NA
saveRDS(list(input = up, output = norma_short(up, "name")), file.path(out, "norma_short.rds"))

lg <- companies
lg$name <- stringr::str_squish(stringr::str_replace_all(toupper(lg$name), "[^A-Z0-9]", " "))
lg$name[c(3, 7)] <- NA
saveRDS(list(input = lg, output = norma_legal(lg, "name", "country")), file.path(out, "norma_legal.rds"))

# nmatch
w <- c("ALPHA", "BETA", "GAMMA", "DELTA", "NORD", "SUD", "EURO", "TECH")
nm <- function(k) vapply(seq_len(k), function(i) paste(sample(w, sample(1:4, 1), TRUE), collapse = " "), "")
a <- data.frame(id_a = 1:200, n = nm(200)); b <- data.frame(id_b = 1:200, n = nm(200))
a$n[5] <- NA
x <- list(a = a, b = b)
x$sorted <- nmatch(a, b, "n", "n")
x$unsorted <- nmatch(a, b, "n", "n", sort = FALSE, unique = FALSE)
saveRDS(x, file.path(out, "nmatch.rds"))

# detect_region: plain codes, codes inside longer strings, codes in two NUTS3, no match, NA
reg <- list()
for (cc in c("DE", "CY", "PL")) {
  z <- zips$CODE[zips$country_code == cc]
  p <- c(sample(z, 150), paste0("D-", sample(z, 10)), paste(sample(z, 5), sample(z, 5)), "99999999", "", NA)
  reg[[cc]] <- list(input = p, output = detect_region(p, cc))
}
saveRDS(reg, file.path(out, "detect_region.rds"))

# harmonize_cities and detect_address: city names with noise around them
pick_city <- function(cc, k) {
  d <- cities[cities$Country.Code == cc, ]
  sample(substr(d$Alternate.Names, 6, nchar(d$Alternate.Names) - 5), k, TRUE)
}
noise <- c("", "", "ul. 5", "D-80331", "Zentrum", "rue de la", "(BE)", "St.")
hc <- list()
for (cc in c("LU", "PL")) {
  ca <- paste(sample(noise, 150, TRUE), tolower(pick_city(cc, 150)), sample(noise, 150, TRUE))
  cb <- paste(sample(noise, 150, TRUE), pick_city(cc, 150))
  ca[1:4] <- c(NA, "", "Nowhere-sur-Mer", paste(pick_city(cc, 1), pick_city(cc, 1)))
  r <- data.frame(id = 1:150, city_a = ca, city_b = cb, stringsAsFactors = FALSE)
  hc[[cc]] <- list(input = r,
                   new_col = harmonize_cities(r, "city_a", "city_b", cc),
                   replace = harmonize_cities(r, "city_a", "city_b", cc, new_col = FALSE))
}
saveRDS(hc, file.path(out, "harmonize_cities.rds"))

ad <- paste("ul.", sample(c("Polna", "Lesna", "Dluga"), 150, TRUE), sample(1:99, 150, TRUE),
            sample(c("00-950", "31-123", ""), 150, TRUE), tolower(pick_city("PL", 150)),
            sample(c("", "Centrum"), 150, TRUE))
ad[1:3] <- c(NA, "", "nothing here")
r <- data.frame(id = 1:150, adr = ad, stringsAsFactors = FALSE)
saveRDS(list(input = r, output = detect_address(r, "adr", "x", "PL")), file.path(out, "detect_address.rds"))

# clean: matched pairs with zip codes, cities and streets
m <- data.frame(id_a = c(1, 1, 2, 3, 4), id_b = c(10, 11, 12, 12, 13),
                name_a = c("Alpha GmbH", "Alpha GmbH", "Beta AG", "Beta AG", "Nord KG"),
                name_b = c("ALPHA GMBH", "ALPHA HOLDING", "BETA AG", "BETA", "NORD"),
                lf_a = c("GMBH", "GMBH", "AG", "AG", NA), lf_b = c("GMBH", "", "AG", "AG", "KG"),
                zip_a = c("80331", "80331", "D-10115", NA, "20095"), zip_b = c("80331", "80333", "10115", "10117", NA),
                city_a = c("Munchen", "Munchen", "Berlin", NA, "Hamburg"), city_b = c("MUENCHEN", "MUENCHEN", "BERLIN", "BERLIN", "HAMBURG"),
                street_a = c("Marienplatz 1", "Marienplatz 1", "Unter den Linden 5", "", NA), street_b = c("MARIENPLATZ 1", "KAUFINGERSTR 2", "UNTER DEN LINDEN 5", "X", "Y"),
                nuts = c("DE212", "DE212", "DE300", "DE300", "DE600"), cc = "DE", stringsAsFactors = FALSE)
cl <- clean(m, "id_a", "id_b", "name_a", "name_b", "lf_a", "lf_b", "zip_a", "zip_b",
            "city_a", "city_b", "street_a", "street_b", "nuts", "cc")
saveRDS(list(input = m, output = cl), file.path(out, "clean.rds"))
