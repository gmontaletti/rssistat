# 1. Date parsing utilities -----

#' Parse RFC 822 date format from RSS feeds
#'
#' Converts RSS pubDate strings (RFC 822 format) to POSIXct objects in UTC timezone.
#' Handles NA values and empty strings gracefully.
#'
#' @param date_str Character vector of RFC 822 formatted dates
#'   (e.g., "Mon, 22 Dec 2025 09:00:33 +0000")
#'
#' @return POSIXct vector in UTC timezone, NA for invalid inputs
#'
#' @keywords internal
#' @importFrom tools file_ext
#'
#' @examples
#' \dontrun{
#' .parse_rss_date("Mon, 22 Dec 2025 09:00:33 +0000")
#' .parse_rss_date(c("Mon, 22 Dec 2025 09:00:33 +0000", NA, ""))
#' }
.parse_rss_date <- function(date_str) {
  if (length(date_str) == 0) {
    return(as.POSIXct(character(0), tz = "UTC"))
  }

  result <- rep(as.POSIXct(NA), length(date_str))

  valid_idx <- !is.na(date_str) & nzchar(trimws(date_str))

  if (any(valid_idx)) {
    parsed <- suppressWarnings(
      as.POSIXct(date_str[valid_idx], format = "%a, %d %b %Y %H:%M:%S", tz = "UTC")
    )
    result[valid_idx] <- parsed
  }

  result
}


# 2. Theme extraction utilities -----

#' Extract theme from RSS feed title
#'
#' Processes feed titles to extract standardized theme identifiers.
#' Removes " - Istat" suffix (handles both regular and em dash), converts to
#' lowercase, and replaces spaces with hyphens.
#'
#' @param title Character vector of feed titles
#'   (e.g., "Prices - Istat", "Environment and energy - Istat")
#'
#' @return Character vector of theme identifiers (e.g., "prices",
#'   "environment-and-energy"). Returns "general" for empty results or NA inputs.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' .extract_theme("Prices - Istat")
#' .extract_theme("Environment and energy - Istat")
#' .extract_theme(c("Prices - Istat", NA, ""))
#' }
.extract_theme <- function(title) {
  if (length(title) == 0) {
    return(character(0))
  }

  result <- rep("general", length(title))

  valid_idx <- !is.na(title) & nzchar(trimws(title))

  if (any(valid_idx)) {
    processed <- title[valid_idx]

    # Remove " - Istat" suffix (regular dash)
    processed <- sub(" - Istat$", "", processed, ignore.case = TRUE)
    # Remove " – Istat" suffix (em dash)
    processed <- sub(" \u2013 Istat$", "", processed, ignore.case = TRUE)

    # Convert to lowercase
    processed <- tolower(processed)

    # Replace spaces with hyphens
    processed <- gsub(" ", "-", processed)

    # Replace empty results with "general"
    processed[!nzchar(trimws(processed))] <- "general"

    result[valid_idx] <- processed
  }

  result
}


# 3. Link classification utilities -----

#' Classify URL by file extension
#'
#' Determines the type of a URL based on its file extension.
#'
#' @param url Character vector of URLs
#'
#' @return Character vector with values: "pdf", "xlsx", "xls", "csv", "zip",
#'   "doc", "docx", or "page" (default for web pages or unknown extensions)
#'
#' @keywords internal
#' @importFrom tools file_ext
#'
#' @examples
#' \dontrun{
#' .classify_link_type("https://example.com/report.pdf")
#' .classify_link_type("https://example.com/data.xlsx")
#' .classify_link_type("https://example.com/page.html")
#' }
.classify_link_type <- function(url) {
  if (length(url) == 0) {
    return(character(0))
  }

  result <- rep("page", length(url))

  valid_idx <- !is.na(url) & nzchar(trimws(url))

  if (any(valid_idx)) {
    extensions <- tolower(tools::file_ext(url[valid_idx]))

    known_types <- c("pdf", "xlsx", "xls", "csv", "zip", "doc", "docx")

    for (i in seq_along(extensions)) {
      if (extensions[i] %in% known_types) {
        result[which(valid_idx)[i]] <- extensions[i]
      }
    }
  }

  result
}


# 4. Empty structure utilities -----

#' Create empty RSS feed structure
#'
#' Returns an empty rss_feed S3 object with the standard structure used by
#' rssistat functions. Useful for initializing results or handling error cases.
#'
#' @return List of class c("rss_feed", "list") containing:
#'   \item{meta}{data.table with columns: title, link, description, language,
#'     last_build, theme, feed_url, fetched_at}
#'   \item{items}{data.table with columns: title, link, description, content,
#'     pub_date, creator, categories, guid}
#'   \item{links}{data.table with columns: item_title, item_link, url, text,
#'     type, source}
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' empty_feed <- .empty_rss_feed()
#' str(empty_feed)
#' }
.empty_rss_feed <- function() {
  meta <- data.table::data.table(
    title = character(0),
    link = character(0),
    description = character(0),
    language = character(0),
    last_build = as.POSIXct(character(0), tz = "UTC"),
    theme = character(0),
    feed_url = character(0),
    fetched_at = as.POSIXct(character(0), tz = "UTC")
  )

  items <- data.table::data.table(
    title = character(0),
    link = character(0),
    description = character(0),
    content = character(0),
    pub_date = as.POSIXct(character(0), tz = "UTC"),
    creator = character(0),
    categories = list(),
    guid = character(0)
  )

  links <- data.table::data.table(
    item_title = character(0),
    item_link = character(0),
    url = character(0),
    text = character(0),
    type = character(0),
    source = character(0)
  )

  result <- list(
    meta = meta,
    items = items,
    links = links
  )

  class(result) <- c("rss_feed", "list")
  result
}
