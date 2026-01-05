#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

# Suppress R CMD check NOTEs for data.table column references
utils::globalVariables(c(
  # discover_feeds.R
  "url",
  "source_url",
  "discovered_at",
  # fetch_feed.R
  "pub_date",
  "type",
  "item_title",
  "item_link",
  "source",
  "text",
  "link",
  ".",
  # download_files.R
  "status",
  "N",
  # fetch_calendar.R
  "start_date"
))
