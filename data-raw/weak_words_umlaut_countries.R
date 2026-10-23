# Limits the umlaut rule (U|A|O)E -> \1 in weak_words_regex to German-speaking countries.
# The rule makes umlauts written as UE, AE, OE equal to umlauts already removed
# (MUELLER and MULLER both give MULLER; MUELLER from MÜLLER via latin-ascii too).
# - removes it for BG, where Latin-script names have no umlaut spellings and the
#   rule only damages them (BOEV -> BOV, RAEV -> RAV);
# - adds it for CH and LI, as the last rule of each country.
# LU is left out: norma_legal() copies BE, NL and LU rules to BX, and the rule would
# then change Dutch names (HOEK -> HOK, BOER -> BOR).
# Run once from the package root; it changes data/weak_words_regex.rda (2026-10-23).
# Already applied: running it again stops at the check below.

load("data/weak_words_regex.rda")
umlaut <- w$to.replace == "(U|A|O)E"
stopifnot(sum(umlaut & w$Country == "BG") == 1, !any(umlaut & w$Country %in% c("CH", "LI")))
w <- w[!(umlaut & w$Country == "BG"), ]
add <- data.frame(Country = c("CH", "LI"), to.replace = "(U|A|O)E", replacement = "\\1",
                  order = max(w$order) + 1:2, stringsAsFactors = FALSE)
w <- rbind(w, add)
rownames(w) <- NULL
save(w, file = "data/weak_words_regex.rda", compress = "gzip", version = 3)
