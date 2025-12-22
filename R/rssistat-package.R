#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

# Suppress R CMD check NOTEs for data.table column references
utils::globalVariables(c(
  "url",
  "source_url",
  "discovered_at"
))
