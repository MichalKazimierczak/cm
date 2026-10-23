# Adds word boundaries to the Greek (GR) weak-word rules in weak_words_regex.
# Six rules had none, so they removed parts of longer words: ATHINAS? cut
# ATHINAIKI ZYTHOPOIIA down to IKI ZYTHOPOIIA, GREEK cut GREEKFOOD to FOOD.
# The fourth rule started with \H (in ICU regex: any character that is not a
# horizontal space) instead of \b, so ANELLADA became A.
# Run once from the package root; it changes data/weak_words_regex.rda (2026-10-23).

load("data/weak_words_regex.rda")
fix <- c(
  "\\H?ELLAD[OA]S?\\b" = "\\bH?ELLAD[OA]S?\\b",
  "GREECE"             = "\\bGREECE\\b",
  "INTERNATIONAL"      = "\\bINTERNATIONAL\\b",
  "ATHENS"             = "\\bATHENS\\b",
  "ATHINAS?"           = "\\bATHINAS?\\b",
  "ATHINAIK[OI]"       = "\\bATHINAIK[OI]\\b",
  "GREEK"              = "\\bGREEK\\b"
)
i <- match(names(fix), w$to.replace[w$Country == "GR"])
stopifnot(!anyNA(i))
rows <- which(w$Country == "GR")[i]
w$to.replace[rows] <- unname(fix)
save(w, file = "data/weak_words_regex.rda", compress = "gzip", version = 3)
