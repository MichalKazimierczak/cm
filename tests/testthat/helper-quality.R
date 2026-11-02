# Shared by the tests of match_quality() and the first-name functions
## a small matching run: a sure match, an ambiguous one, a legal-form conflict, a person with the surname first,
## and a sole trader ('h.o.d.n.') matched as person and as firm
quality_example <- function() {
  a <- data.frame(id_a = 1:5, name_a = c("Alpha GmbH", "Jan de Vries", "Beta AG", "Nord KG", "Jan de Vries h.o.d.n. Bakkerij de Molen"),
                  country = c("DE", "DE", "DE", "DE", "NL"), zip_a = c("80331", NA, "10115", "20095", NA),
                  city_a = c("Muenchen", NA, "Berlin", "Hamburg", NA), street_a = c("Marienplatz 1", NA, "Unter den Linden 5", NA, NA),
                  type = c("Legal entity", "Natural person", "Legal entity", "Legal entity", "Natural person"), stringsAsFactors = FALSE)
  b <- data.frame(id_b = 11:16, name_b = c("ALPHA GMBH", "DE VRIES JAN", "BETA AG", "BETA AKTIENGESELLSCHAFT", "NORD GMBH", "BAKKERIJ DE MOLEN"),
                  country = c(rep("DE", 5), "NL"), zip_b = c("80331", NA, "10115", "50667", NA, NA),
                  city_b = c("MUENCHEN", NA, "BERLIN", "KOELN", "HAMBURG", NA), street_b = NA, nuts = NA,
                  psn_sector = c("COMPANY", "INDIVIDUAL", "COMPANY", NA, "COMPANY", "COMPANY"), stringsAsFactors = FALSE)
  na <- norma(a, "name_a", "country", "id_a")
  nb <- norma(b, "name_b", "country", "id_b")
  m <- nmatch(na, nb, "norm_name", "norm_name")
  names(m) <- sub("\\.x$", "_a", sub("\\.y$", "_b", names(m)))
  cl <- clean(m, "id_a", "id_b", "name_a", "name_b", "lf_a", "lf_b", "zip_a", "zip_b", "city_a", "city_b",
              "street_a", "street_b", "nuts", "country_a")
  list(na = na, nb = nb, cl = cl, rk = suppressMessages(rankmatch(cl)))
}
quality_full <- function(x, pairs = x$rk) {
  match_quality(pairs, "id_a", "id_b", "name_a", "name_b", lf_a = "lf_a", lf_b = "lf_b", zip_a = "zip_a", zip_b = "zip_b",
                city_a = "city_a", city_b = "city_b", street_a = "street_a", street_b = "street_b",
                norm_a = x$na, norm_b = x$nb, norm_key_a = "id_a", norm_key_b = "id_b", type_a = "type", type_b = "psn_sector")
}
pick <- function(q, a, b) q[q$id_a == a & q$id_b == b, ]

