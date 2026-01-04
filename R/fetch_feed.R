# 1. Main fetch function -----

# Declare global variables for data.table operations (R CMD check)
utils::globalVariables(c("pub_date", ".N", "type", "N"))

#' Fetch and parse RSS feed
#'
#' Downloads and parses an RSS feed, extracting metadata, items, and embedded
#' links. Supports optional date filtering and custom HTTP configuration.
#'
#' @param url RSS feed URL (character string starting with http(s)://)
#' @param since Filter items on or after this date. Accepts Date, POSIXct, or
#'   character string in "YYYY-MM-DD" format. Default NULL (no lower bound).
#' @param until Filter items before this date (same formats as `since`).
#'   Default NULL (no upper bound).
#' @param timeout Request timeout in seconds. Default 30.
#' @param user_agent Custom HTTP User-Agent header. Default NULL (uses httr2 default).
#' @param verbose Print progress messages during fetching and parsing. Default FALSE.
#'
#' @return S3 object of class "rss_feed" containing:
#'   \item{meta}{data.table with single row of feed metadata: title, link,
#'     description, language, last_build, theme, feed_url, fetched_at}
#'   \item{items}{data.table of feed items: title, link, description, content,
#'     pub_date, creator, categories (list column), guid}
#'   \item{links}{data.table of extracted links: item_title, item_link, url,
#'     text, type, source}
#'
#' @export
#'
#' @importFrom httr2 request req_timeout req_retry req_user_agent req_perform resp_status resp_body_string
#' @importFrom xml2 read_xml xml_find_all xml_find_first xml_text xml_attr read_html
#' @importFrom data.table data.table rbindlist setDT
#'
#' @examples
#' \dontrun{
#' # Fetch ISTAT prices feed
#' feed <- fetch_feed("https://www.istat.it/en/tema/prices/feed")
#' print(feed)
#'
#' # Filter by date
#' recent <- fetch_feed("https://www.istat.it/en/feed", since = "2025-01-01")
#'
#' # Verbose output
#' feed <- fetch_feed("https://www.istat.it/en/feed", verbose = TRUE)
#' }
fetch_feed <- function(url,
                       since = NULL,
                       until = NULL,
                       timeout = 30,
                       user_agent = NULL,
                       verbose = FALSE) {

  # 1.1 Input validation -----
  if (!is.character(url) || length(url) != 1 || is.na(url)) {
    stop("url must be a single non-NA character string", call. = FALSE)
  }

  if (!grepl("^https?://", url, ignore.case = TRUE)) {
    stop("url must start with http:// or https://", call. = FALSE)
  }

  # Validate and convert date parameters
  since_posix <- .validate_date_param(since, "since")
  until_posix <- .validate_date_param(until, "until")

  if (!is.null(since_posix) && !is.null(until_posix) && since_posix >= until_posix) {
    stop("since must be earlier than until", call. = FALSE)
  }

  if (!is.numeric(timeout) || length(timeout) != 1 || is.na(timeout) || timeout <= 0) {
    stop("timeout must be a single positive number", call. = FALSE)
  }

  if (!is.null(user_agent) && (!is.character(user_agent) || length(user_agent) != 1 || is.na(user_agent))) {
    stop("user_agent must be NULL or a single non-NA character string", call. = FALSE)
  }

  if (!is.logical(verbose) || length(verbose) != 1 || is.na(verbose)) {
    stop("verbose must be a single non-NA logical value", call. = FALSE)
  }

  # 1.2 Build HTTP request -----
  if (verbose) message("Building HTTP request for: ", url)

  req <- httr2::request(url)
  req <- httr2::req_timeout(req, timeout)
  req <- httr2::req_retry(req, max_tries = 3, backoff = ~ 2)

  if (!is.null(user_agent)) {
    req <- httr2::req_user_agent(req, user_agent)
  }

  # 1.3 Perform request with error handling -----
  if (verbose) message("Fetching feed...")

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      warning("Failed to fetch feed from ", url, ": ", conditionMessage(e), call. = FALSE)
      return(NULL)
    }
  )

  if (is.null(resp)) {
    return(.empty_rss_feed())
  }

  # Check HTTP status
  status <- httr2::resp_status(resp)
  if (status >= 400) {
    warning("HTTP error ", status, " when fetching ", url, call. = FALSE)
    return(.empty_rss_feed())
  }

  # 1.4 Parse RSS XML -----
  if (verbose) message("Parsing XML...")

  doc <- tryCatch(
    xml2::read_xml(httr2::resp_body_string(resp)),
    error = function(e) {
      warning("Failed to parse XML from ", url, ": ", conditionMessage(e), call. = FALSE)
      return(NULL)
    }
  )

  if (is.null(doc)) {
    return(.empty_rss_feed())
  }

  # 1.5 Define XML namespaces -----
  ns <- c(
    dc = "http://purl.org/dc/elements/1.1/",
    content = "http://purl.org/rss/1.0/modules/content/",
    atom = "http://www.w3.org/2005/Atom"
  )

  # 1.6 Extract channel metadata -----
  if (verbose) message("Extracting metadata...")

  channel <- xml2::xml_find_first(doc, "//channel")

  meta_title <- .safe_xml_text(xml2::xml_find_first(channel, "./title"))
  meta_link <- .safe_xml_text(xml2::xml_find_first(channel, "./link"))
  meta_desc <- .safe_xml_text(xml2::xml_find_first(channel, "./description"))
  meta_lang <- .safe_xml_text(xml2::xml_find_first(channel, "./language"))
  meta_build <- .parse_rss_date(.safe_xml_text(xml2::xml_find_first(channel, "./lastBuildDate")))

  theme <- .extract_theme(meta_title)
  fetched_at <- Sys.time()
  attr(fetched_at, "tzone") <- "UTC"

  meta <- data.table::data.table(
    title = meta_title,
    link = meta_link,
    description = meta_desc,
    language = meta_lang,
    last_build = meta_build,
    theme = theme,
    feed_url = url,
    fetched_at = fetched_at
  )

  # 1.7 Extract items -----
  if (verbose) message("Extracting items...")

  item_nodes <- xml2::xml_find_all(doc, "//item")

  if (length(item_nodes) == 0) {
    if (verbose) message("No items found in feed")
    result <- list(
      meta = meta,
      items = .empty_rss_feed()$items,
      links = .empty_rss_feed()$links
    )
    class(result) <- c("rss_feed", "list")
    return(result)
  }

  items_list <- lapply(item_nodes, function(node) {
    item_title <- .safe_xml_text(xml2::xml_find_first(node, "./title"))
    item_link <- .safe_xml_text(xml2::xml_find_first(node, "./link"))
    item_desc <- .safe_xml_text(xml2::xml_find_first(node, "./description"))
    item_content <- .safe_xml_text(xml2::xml_find_first(node, "./content:encoded", ns = ns))
    item_pubdate <- .parse_rss_date(.safe_xml_text(xml2::xml_find_first(node, "./pubDate")))
    item_creator <- .safe_xml_text(xml2::xml_find_first(node, "./dc:creator", ns = ns))
    item_guid <- .safe_xml_text(xml2::xml_find_first(node, "./guid"))

    # Extract categories (may be multiple)
    cat_nodes <- xml2::xml_find_all(node, "./category")
    item_categories <- if (length(cat_nodes) > 0) {
      list(xml2::xml_text(cat_nodes))
    } else {
      list(character(0))
    }

    data.table::data.table(
      title = item_title,
      link = item_link,
      description = item_desc,
      content = item_content,
      pub_date = item_pubdate,
      creator = item_creator,
      categories = item_categories,
      guid = item_guid
    )
  })

  items <- data.table::rbindlist(items_list, use.names = TRUE, fill = TRUE)

  # 1.8 Apply date filtering -----
  if (!is.null(since_posix) || !is.null(until_posix)) {
    if (verbose) message("Applying date filters...")

    original_count <- nrow(items)

    if (!is.null(since_posix)) {
      items <- items[pub_date >= since_posix | is.na(pub_date)]
    }

    if (!is.null(until_posix)) {
      items <- items[pub_date < until_posix | is.na(pub_date)]
    }

    if (verbose) {
      filtered_count <- original_count - nrow(items)
      if (filtered_count > 0) {
        message("Filtered out ", filtered_count, " items based on date range")
      }
    }
  }

  # 1.9 Extract links from content -----
  if (verbose) message("Extracting links from content...")

  if (nrow(items) > 0) {
    links_list <- lapply(seq_len(nrow(items)), function(i) {
      item <- items[i, ]

      # Extract from content field
      content_links <- if (!is.na(item$content) && nzchar(item$content)) {
        .extract_links_from_html(item$content, item$title, item$link, "content")
      } else {
        NULL
      }

      # Extract from description field
      desc_links <- if (!is.na(item$description) && nzchar(item$description)) {
        .extract_links_from_html(item$description, item$title, item$link, "description")
      } else {
        NULL
      }

      rbind(content_links, desc_links)
    })

    links <- data.table::rbindlist(links_list, use.names = TRUE, fill = TRUE)

    # Remove duplicates within same item
    if (nrow(links) > 0) {
      data.table::setDT(links)
      links <- unique(links, by = c("item_link", "url"))
    }
  } else {
    links <- .empty_rss_feed()$links
  }

  # 1.10 Build and return rss_feed object -----
  if (verbose) message("Done. Fetched ", nrow(items), " items with ", nrow(links), " links")

  result <- list(
    meta = meta,
    items = items,
    links = links
  )

  class(result) <- c("rss_feed", "list")
  result
}


# 2. Helper functions -----

#' Validate and convert date parameter
#'
#' @param date Date, POSIXct, character, or NULL
#' @param param_name Parameter name for error messages
#'
#' @return POSIXct in UTC or NULL
#' @keywords internal
.validate_date_param <- function(date, param_name) {
  if (is.null(date)) {
    return(NULL)
  }

  if (inherits(date, "POSIXct")) {
    result <- date
    attr(result, "tzone") <- "UTC"
    return(result)
  }

  if (inherits(date, "Date")) {
    result <- as.POSIXct(date, tz = "UTC")
    return(result)
  }

  if (is.character(date) && length(date) == 1 && !is.na(date)) {
    parsed <- tryCatch(
      as.POSIXct(date, format = "%Y-%m-%d", tz = "UTC"),
      error = function(e) NA
    )

    if (!is.na(parsed)) {
      return(parsed)
    }
  }

  stop(param_name, " must be NULL, Date, POSIXct, or character in 'YYYY-MM-DD' format", call. = FALSE)
}


#' Safely extract text from XML node
#'
#' @param node XML node or nodeset
#'
#' @return Character string or NA_character_
#' @keywords internal
.safe_xml_text <- function(node) {
  if (length(node) == 0) {
    return(NA_character_)
  }

  text <- xml2::xml_text(node)

  if (length(text) == 0 || !nzchar(trimws(text))) {
    return(NA_character_)
  }

  trimws(text)
}


#' Extract links from HTML content
#'
#' @param html_string HTML content as character string
#' @param item_title Title of the RSS item
#' @param item_link Link of the RSS item
#' @param source Source field name ("content" or "description")
#'
#' @return data.table with columns: item_title, item_link, url, text, type, source
#' @keywords internal
.extract_links_from_html <- function(html_string, item_title, item_link, source) {
  if (is.na(html_string) || !nzchar(html_string)) {
    return(NULL)
  }

  html_doc <- tryCatch(
    xml2::read_html(html_string),
    error = function(e) NULL
  )

  if (is.null(html_doc)) {
    return(NULL)
  }

  link_nodes <- xml2::xml_find_all(html_doc, "//a[@href]")

  if (length(link_nodes) == 0) {
    return(NULL)
  }

  links_data <- lapply(link_nodes, function(node) {
    href <- xml2::xml_attr(node, "href")
    text <- trimws(xml2::xml_text(node))

    if (is.na(href) || !nzchar(href)) {
      return(NULL)
    }

    # Skip relative URLs or anchors
    if (!grepl("^https?://", href, ignore.case = TRUE)) {
      return(NULL)
    }

    data.table::data.table(
      item_title = item_title,
      item_link = item_link,
      url = href,
      text = if (nzchar(text)) text else NA_character_,
      type = .classify_link_type(href),
      source = source
    )
  })

  links_dt <- data.table::rbindlist(links_data, use.names = TRUE, fill = TRUE)

  if (nrow(links_dt) == 0) {
    return(NULL)
  }

  links_dt
}


# 3. S3 methods -----

#' Print method for rss_feed objects
#'
#' @param x Object of class "rss_feed"
#' @param ... Additional arguments (ignored)
#'
#' @return Invisible x
#' @export
#'
#' @examples
#' \dontrun{
#' feed <- fetch_feed("https://www.istat.it/en/feed")
#' print(feed)
#' }
print.rss_feed <- function(x, ...) {
  cat("RSS Feed:", x$meta$title, "\n")
  cat("Theme:", x$meta$theme, "\n")
  cat("Items:", nrow(x$items), "\n")
  cat("Links:", nrow(x$links), "\n")

  if (nrow(x$items) > 0 && any(!is.na(x$items$pub_date))) {
    date_min <- min(x$items$pub_date, na.rm = TRUE)
    date_max <- max(x$items$pub_date, na.rm = TRUE)
    cat("Date range:", format(date_min, "%Y-%m-%d"),
        "to", format(date_max, "%Y-%m-%d"), "\n")
  }

  invisible(x)
}


#' Summary method for rss_feed objects
#'
#' Summarizes link types found in the feed.
#'
#' @param object Object of class "rss_feed"
#' @param ... Additional arguments (ignored)
#'
#' @return data.table of link type counts, sorted by frequency
#' @export
#'
#' @examples
#' \dontrun{
#' feed <- fetch_feed("https://www.istat.it/en/feed")
#' summary(feed)
#' }
summary.rss_feed <- function(object, ...) {
  if (nrow(object$links) == 0) {
    cat("No links found in feed\n")
    return(invisible(NULL))
  }

  link_summary <- object$links[, .N, by = type][order(-N)]
  link_summary
}
