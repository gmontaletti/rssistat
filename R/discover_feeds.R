#' Discover RSS and Atom Feeds from a Web Page
#'
#' Scans an HTML page for RSS and Atom feed URLs using the standard
#' autodiscovery mechanism (link tags with rel="alternate").
#'
#' @param url Character string. The URL of the web page to scan.
#' @param include_anchors Logical. If TRUE, also search anchor tags for
#'   potential feed links. Default: FALSE.
#' @param timeout Numeric. Request timeout in seconds. Default: 30.
#' @param user_agent Character or NULL. Custom user agent string.
#' @param verbose Logical. Print progress messages. Default: FALSE.
#'
#' @return A [data.table::data.table] with columns:
#' \describe{
#'   \item{url}{Absolute URL of the discovered feed}
#'   \item{title}{Feed title (may be NA)}
#'   \item{type}{MIME type (application/rss+xml or application/atom+xml)}
#'   \item{source}{Detection method ("autodiscovery" or "anchor")}
#'   \item{source_url}{The page URL where the feed was discovered}
#'   \item{discovered_at}{Timestamp of discovery}
#' }
#'
#' @details
#' The function uses the RSS autodiscovery standard, which looks for
#' `<link>` tags in the HTML head with `rel="alternate"` and appropriate
#' MIME types.
#'
#' @examples
#' \dontrun{
#' # Discover feeds from ISTAT
#' feeds <- discover_feeds("https://www.istat.it/en/other-services/feed-rss/")
#' print(feeds)
#'
#' # Save for later use
#' saveRDS(feeds, "istat_feeds.rds")
#' }
#'
#' @export
#' @importFrom data.table data.table :=
#' @importFrom httr2 request req_timeout req_retry req_perform resp_body_string resp_status
#' @importFrom xml2 read_html xml_find_all xml_attr url_absolute
discover_feeds <- function(url,
                           include_anchors = FALSE,
                           timeout = 30,
                           user_agent = NULL,
                           verbose = FALSE) {



  # 1. Input validation -----
  # Store input URL before validation (avoid column name collision)
page_url <- url

  if (!is.character(url) || length(url) != 1 || is.na(url)) {
    stop("'url' must be a single character string", call. = FALSE)
  }

  if (!grepl("^https?://", url)) {
    stop("'url' must start with http:// or https://", call. = FALSE)
  }

  if (!is.logical(include_anchors) || length(include_anchors) != 1) {
    stop("'include_anchors' must be TRUE or FALSE", call. = FALSE)
  }

  if (!is.numeric(timeout) || length(timeout) != 1 || timeout < 1) {
    stop("'timeout' must be a positive number", call. = FALSE)
  }

  if (!is.null(user_agent) && (!is.character(user_agent) || length(user_agent) != 1)) {
    stop("'user_agent' must be NULL or a single character string", call. = FALSE)
  }

  if (!is.logical(verbose) || length(verbose) != 1) {
    stop("'verbose' must be TRUE or FALSE", call. = FALSE)
  }

  # 2. Build HTTP request -----
  if (verbose) message("Fetching: ", url)

  req <- httr2::request(url)
  req <- httr2::req_timeout(req, timeout)
  req <- httr2::req_retry(req, max_tries = 3, backoff = ~ 2)

  if (!is.null(user_agent)) {
    req <- httr2::req_user_agent(req, user_agent)
  }

  # 3. Perform request with error handling -----
  resp <- tryCatch({
    httr2::req_perform(req)
  }, error = function(e) {
    stop(
      sprintf("Failed to fetch URL '%s': %s", url, conditionMessage(e)),
      call. = FALSE
    )
  })

  # Check HTTP status
  status <- httr2::resp_status(resp)
  if (status >= 400) {
    warning(
      sprintf("HTTP error %d when fetching '%s'", status, url),
      call. = FALSE
    )
    return(.empty_feeds_dt())
  }

  # 4. Parse HTML content -----
  if (verbose) message("Parsing HTML...")

  html_content <- httr2::resp_body_string(resp)
  html <- tryCatch({
    xml2::read_html(html_content)
  }, error = function(e) {
    warning(
      sprintf("Failed to parse HTML from '%s': %s", url, conditionMessage(e)),
      call. = FALSE
    )
    return(.empty_feeds_dt())
  })

  # 5. Extract autodiscovery link tags -----
  if (verbose) message("Searching for RSS/Atom autodiscovery links...")

  xpath_autodiscovery <- paste0(
    "//link[@rel='alternate' and ",
    "(@type='application/rss+xml' or @type='application/atom+xml')]"
  )

  link_nodes <- xml2::xml_find_all(html, xpath_autodiscovery)

  feeds_list <- list()

  if (length(link_nodes) > 0) {
    feeds_list[["autodiscovery"]] <- data.table::data.table(
      url = xml2::xml_attr(link_nodes, "href"),
      title = xml2::xml_attr(link_nodes, "title"),
      type = xml2::xml_attr(link_nodes, "type"),
      source = "autodiscovery"
    )
  }

  # 6. Optional: Search anchor tags -----
  if (include_anchors) {
    if (verbose) message("Searching anchor tags for feed links...")

    xpath_anchors <- paste0(
      "//a[contains(@href, '.rss') or contains(@href, '.xml') or ",
      "contains(@href, '/feed') or contains(@href, '/rss')]"
    )

    anchor_nodes <- xml2::xml_find_all(html, xpath_anchors)

    if (length(anchor_nodes) > 0) {
      anchor_hrefs <- xml2::xml_attr(anchor_nodes, "href")
      anchor_titles <- xml2::xml_text(anchor_nodes)

      # Clean up titles (remove extra whitespace)
      anchor_titles <- trimws(gsub("\\s+", " ", anchor_titles))
      anchor_titles[anchor_titles == ""] <- NA_character_

      feeds_list[["anchor"]] <- data.table::data.table(
        url = anchor_hrefs,
        title = anchor_titles,
        type = NA_character_,
        source = "anchor"
      )
    }
  }

  # 7. Combine results -----
  if (length(feeds_list) == 0) {
    if (verbose) message("No feeds found.")
    return(.empty_feeds_dt())
  }

  feeds_dt <- data.table::rbindlist(feeds_list, use.names = TRUE, fill = TRUE)

  # 8. Resolve relative URLs to absolute -----
  feeds_dt[, url := xml2::url_absolute(url, base = page_url)]

  # 9. Add metadata -----
  feeds_dt[, source_url := page_url]
  feeds_dt[, discovered_at := Sys.time()]

  # 10. Remove duplicates -----
  feeds_dt <- unique(feeds_dt, by = "url")

  # Reorder columns
  data.table::setcolorder(
    feeds_dt,
    c("url", "title", "type", "source", "source_url", "discovered_at")
  )

  if (verbose) message("Found ", nrow(feeds_dt), " feed(s).")

  feeds_dt
}


#' Create Empty Feeds Data Table
#'
#' Internal function to return an empty data.table with the correct structure.
#'
#' @return An empty data.table with the expected columns.
#' @keywords internal
.empty_feeds_dt <- function() {
  data.table::data.table(
    url = character(0),
    title = character(0),
    type = character(0),
    source = character(0),
    source_url = character(0),
    discovered_at = as.POSIXct(character(0))
  )
}
