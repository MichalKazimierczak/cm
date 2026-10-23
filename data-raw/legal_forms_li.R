# Adds the German-language legal forms of Liechtenstein (LI) to legal_forms_regex.
# The LI list recognised the AG only in its French form (SA, SOCIETE ANONYME), so
# "MUELLER AG" kept AG in the name. Added, modelled on the Swiss (CH) rules and their order:
#   AKTIENGESELLSCHAFT and A G / AG             -> AG
#   STIFTUNG                                    -> FOUNDATION (as for CH)
#   ANSTALT                                     -> ANSTALT    (establishment, specific to LI)
#   TRUST REG, TREUUNTERNEHMEN                  -> TRUST REG  (trust enterprise, specific to LI)
# Run once from the package root; it changes data/legal_forms_regex.rda (2026-10-23).
# Already applied: running it again stops at the check below.

load("data/legal_forms_regex.rda")
stopifnot(!any(l$Country == "LI" & l$replacement %in% c("FOUNDATION", "ANSTALT", "TRUST REG")))
add <- data.frame(
  Country = "LI",
  to.replace = c("AKTIENGESELL?SC?HAFT", "^A ?G | A ?G | A ?G$", "STIFTUNG", "\\bANSTALT\\b",
                 "\\bTRUST REG\\b|TREUUNTERNEHMEN"),
  replacement = c("AG", "AG", "FOUNDATION", "ANSTALT", "TRUST REG"),
  order = c(117L, 140L, 81L, 82L, 83L),
  stringsAsFactors = FALSE
)
l <- rbind(l, add)
save(l, file = "data/legal_forms_regex.rda", compress = "gzip", version = 3)
