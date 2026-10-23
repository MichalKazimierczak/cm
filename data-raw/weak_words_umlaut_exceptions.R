# Replaces the umlaut rule (U|A|O)E -> \1 in weak_words_regex (DE, AT, CH, LI) by three
# rules that skip the common spots where UE, AE, OE are not umlauts:
#   UE -> U, but not after A, E or Q (BAUER, NEUE, FEUER, STEUER, QUELLE) and not in a final
#            -UEL (SAMUEL, MANUEL);
#   AE -> A, but not in a final -AEL (MICHAEL, ISRAEL, RAFAEL);
#   OE -> O, always (a final -OEL is usually OEL, oil, as in MINERALOEL).
# Umlauts written as UE, AE, OE still match umlauts already removed (MUELLER = MULLER,
# GUENTHER = GUNTHER, WAELDER = WALDER). No exception after G, because of GUENTHER and
# GUETERSLOH. Still collapsed: POET, BLUE, AERO, MICHAELA, RAFFAELE (rare in DE, AT, CH, LI).
# Run once from the package root; it changes data/weak_words_regex.rda (2026-10-23).
# Already applied: running it again stops at the check below.

load("data/weak_words_regex.rda")
old <- which(w$to.replace == "(U|A|O)E")
stopifnot(setequal(w$Country[old], c("DE", "AT", "CH", "LI")))
rules <- data.frame(to.replace = c("(?<![AEQ])UE(?!L\\b)", "AE(?!L\\b)", "OE"),
                    replacement = c("U", "A", "O"), stringsAsFactors = FALSE)
countries <- w$Country[old][order(w$order[old])]
add <- do.call(rbind, lapply(countries, function(cc) data.frame(Country = cc, rules, stringsAsFactors = FALSE)))
add$order <- max(w$order) + seq_len(nrow(add))
w <- rbind(w[-old, ], add)
rownames(w) <- NULL
save(w, file = "data/weak_words_regex.rda", compress = "gzip", version = 3)
