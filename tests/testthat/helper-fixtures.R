# Inputs and their reference outputs (first from 0.1.0, updated after intended changes; see NEWS.md);
# built by fixtures/make_fixtures.R
fixture <- function(name) readRDS(test_path("fixtures", paste0(name, ".rds")))
