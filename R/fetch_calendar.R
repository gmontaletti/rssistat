# 1. Main fetch function -----

#' Fetch ISTAT Calendar Events
#'
#' Fetches and parses calendar events from ISTAT's Google Calendar ICS feeds.
#' ISTAT publishes release dates for press releases, database updates,
#' editorial products, and other events across 5 separate calendars.
#'
#' @param calendars Character vector specifying which calendars to fetch.
#'   Options: "comunicati" (press releases), "note_stampa" (other press notes),
#'   "prodotti" (editorial products), "banche_dati" (database updates),
#'   "convegni" (conferences and events), or "all" (default, fetches all 5).
#' @param since Filter events on or after this date. Accepts Date, POSIXct, or
#'   character string in "YYYY-MM-DD" format. Default NULL (no lower bound).
#' @param until Filter events before this date (same formats as `since`).
#'   Default NULL (no upper bound).
#' @param timeout Request timeout in seconds per calendar. Default 30.
#' @param verbose Print progress messages during fetching. Default FALSE.
#'
#' @return A data.table with columns:
#'   \item{title}{Event title/summary}
#'   \item{start_date}{Event start date (POSIXct, UTC)}
#'   \item{end_date}{Event end date (POSIXct, UTC)}
#'   \item{description}{Event description text}
#'   \item{location}{Event location}
#'   \item{calendar_type}{Type of calendar: "comunicati", "note_stampa",
#'     "prodotti", "banche_dati", or "convegni"}
#'   \item{uid}{Unique event identifier from ICS}
#'   \item{updated_at}{Last modification timestamp (POSIXct, UTC)}
#'   \item{ics_url}{Source ICS URL}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Fetch all calendars
#' events <- fetch_calendar()
#'
#' # Fetch only press releases from 2025
#' releases <- fetch_calendar(
#'   calendars = "comunicati",
#'   since = "2025-01-01",
#'   until = "2026-01-01"
#' )
#'
#' # Fetch multiple calendar types
#' events <- fetch_calendar(calendars = c("comunicati", "banche_dati"))
#' }
fetch_calendar <- function(
  calendars = "all",
  since = NULL,
  until = NULL,
  timeout = 30,
  verbose = FALSE
) {
  # 1.1 Input validation -----
  valid_calendars <- c(
    "comunicati",
    "note_stampa",
    "prodotti",
    "banche_dati",
    "convegni",
    "all"
  )

  if (!is.character(calendars) || length(calendars) == 0) {
    stop("calendars must be a non-empty character vector", call. = FALSE)
  }

  if (!all(calendars %in% valid_calendars)) {
    stop(
      "calendars must be one of: ",
      paste(valid_calendars, collapse = ", "),
      call. = FALSE
    )
  }

  # Expand "all" to all calendar types
  if ("all" %in% calendars) {
    calendars <- c(
      "comunicati",
      "note_stampa",
      "prodotti",
      "banche_dati",
      "convegni"
    )
  }

  # Validate dates using existing helper

  since_posix <- .validate_date_param(since, "since")
  until_posix <- .validate_date_param(until, "until")

  # Validate date range
  if (
    !is.null(since_posix) && !is.null(until_posix) && since_posix >= until_posix
  ) {
    stop("since must be earlier than until", call. = FALSE)
  }

  if (!is.numeric(timeout) || length(timeout) != 1 || timeout <= 0) {
    stop("timeout must be a positive number", call. = FALSE)
  }

  if (!is.logical(verbose) || length(verbose) != 1 || is.na(verbose)) {
    stop("verbose must be a single non-NA logical value", call. = FALSE)
  }

  # 1.2 Get calendar URLs -----
  all_urls <- .istat_calendar_urls()
  urls_to_fetch <- all_urls[calendars]

  # 1.3 Fetch each calendar -----
  results_list <- lapply(names(urls_to_fetch), function(cal_type) {
    if (verbose) {
      message("Fetching ", cal_type, " calendar...")
    }
    .fetch_single_ics(urls_to_fetch[cal_type], cal_type, timeout, verbose)
  })

  # 1.4 Combine results -----
  events <- data.table::rbindlist(results_list, use.names = TRUE, fill = TRUE)

  if (nrow(events) == 0) {
    if (verbose) {
      message("No events found")
    }
    return(.empty_calendar_dt())
  }

  # 1.5 Apply date filtering -----
  if (!is.null(since_posix)) {
    events <- events[start_date >= since_posix | is.na(start_date)]
  }
  if (!is.null(until_posix)) {
    events <- events[start_date < until_posix | is.na(start_date)]
  }

  # 1.6 Sort by start_date -----
  data.table::setorder(events, start_date)

  if (verbose) {
    message("Found ", nrow(events), " events")
  }

  events
}


# 2. Calendar URL registry -----

#' Get ISTAT calendar URLs
#'
#' Returns a named character vector of ISTAT Google Calendar ICS URLs.
#'
#' @return Named character vector with calendar types as names and ICS URLs
#'   as values
#'
#' @keywords internal
.istat_calendar_urls <- function() {
  c(
    comunicati = "https://calendar.google.com/calendar/ical/4s57ih6d08n330qrm9ee575nog@group.calendar.google.com/public/basic.ics",
    note_stampa = "https://calendar.google.com/calendar/ical/vfjfh1jpkirilum8s3eu7932c8@group.calendar.google.com/public/basic.ics",
    prodotti = "https://calendar.google.com/calendar/ical/gd4th8t929jsuj3nh4np8r25as@group.calendar.google.com/public/basic.ics",
    banche_dati = "https://calendar.google.com/calendar/ical/tbv42ne55nm9f3u83plt96tio4@group.calendar.google.com/public/basic.ics",
    convegni = "https://calendar.google.com/calendar/ical/3vk31mkd3ocvufmqol6ie25mso@group.calendar.google.com/public/basic.ics"
  )
}


# 3. ICS fetching helpers -----

#' Fetch and parse a single ICS feed
#'
#' Downloads an ICS calendar feed and parses it into a data.table of events.
#'
#' @param url ICS feed URL
#' @param calendar_type Calendar type identifier (e.g., "comunicati")
#' @param timeout Request timeout in seconds
#' @param verbose Print progress messages
#'
#' @return data.table of events or empty data.table on failure
#'
#' @keywords internal
.fetch_single_ics <- function(url, calendar_type, timeout, verbose) {
  # Build request
  req <- httr2::request(url)
  req <- httr2::req_timeout(req, timeout)
  req <- httr2::req_retry(req, max_tries = 3, backoff = ~2)

  # Perform request
  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      warning(
        "Failed to fetch ",
        calendar_type,
        " calendar: ",
        conditionMessage(e),
        call. = FALSE
      )
      return(NULL)
    }
  )

  if (is.null(resp)) {
    return(.empty_calendar_dt())
  }

  # Check HTTP status
  status <- httr2::resp_status(resp)
  if (status >= 400) {
    warning(
      "HTTP error ",
      status,
      " when fetching ",
      calendar_type,
      " calendar",
      call. = FALSE
    )
    return(.empty_calendar_dt())
  }

  # Get response body
  ics_content <- tryCatch(
    httr2::resp_body_string(resp),
    error = function(e) {
      warning(
        "Failed to read response body: ",
        conditionMessage(e),
        call. = FALSE
      )
      return(NULL)
    }
  )

  if (is.null(ics_content) || !nzchar(ics_content)) {
    return(.empty_calendar_dt())
  }

  # Parse ICS content
  .parse_ics_content(ics_content, calendar_type, url)
}


# 4. ICS parsing helpers -----

#' Parse ICS content to data.table
#'
#' Parses raw ICS calendar content into a standardized data.table format.
#' Handles line unfolding (RFC 5545 continuation lines) and field extraction.
#'
#' @param ics_content Raw ICS content as character string
#' @param calendar_type Calendar type identifier
#' @param ics_url Source URL for reference
#'
#' @return data.table with event columns
#'
#' @keywords internal
.parse_ics_content <- function(ics_content, calendar_type, ics_url) {
  # Unfold continuation lines (RFC 5545: lines starting with space/tab
  # are continuations)
  ics_content <- gsub("\r\n[ \t]", "", ics_content)
  ics_content <- gsub("\n[ \t]", "", ics_content)

  # Split into lines
  lines <- strsplit(ics_content, "\r\n|\n")[[1]]

  # Find VEVENT blocks
  event_starts <- which(lines == "BEGIN:VEVENT")
  event_ends <- which(lines == "END:VEVENT")

  if (length(event_starts) == 0 || length(event_starts) != length(event_ends)) {
    return(.empty_calendar_dt())
  }

  # Parse each event
  events_list <- lapply(seq_along(event_starts), function(i) {
    event_lines <- lines[(event_starts[i] + 1):(event_ends[i] - 1)]
    .parse_ics_event(event_lines, calendar_type, ics_url)
  })

  # Combine into data.table
  data.table::rbindlist(events_list, use.names = TRUE, fill = TRUE)
}


#' Parse a single ICS event
#'
#' Extracts fields from ICS event lines and returns a single-row data.table.
#'
#' @param event_lines Character vector of lines within a VEVENT block
#' @param calendar_type Calendar type identifier
#' @param ics_url Source URL
#'
#' @return Single-row data.table with event data
#'
#' @keywords internal
.parse_ics_event <- function(event_lines, calendar_type, ics_url) {
  # Helper to extract field value
  extract_field <- function(field_name) {
    # Match field with optional parameters (e.g., DTSTART;VALUE=DATE:20150313)
    pattern <- paste0("^", field_name, "([;:].*)$")
    matches <- grep(pattern, event_lines, value = TRUE)

    if (length(matches) == 0) {
      return(NA_character_)
    }

    # Get the value after the colon
    value <- sub("^[^:]*:", "", matches[1])

    # Unescape ICS special characters
    value <- gsub("\\\\n", "\n", value)
    value <- gsub("\\\\,", ",", value)
    value <- gsub("\\\\;", ";", value)
    value <- gsub("\\\\\\\\", "\\\\", value)

    trimws(value)
  }

  data.table::data.table(
    title = extract_field("SUMMARY"),
    start_date = .parse_ics_datetime(extract_field("DTSTART")),
    end_date = .parse_ics_datetime(extract_field("DTEND")),
    description = extract_field("DESCRIPTION"),
    location = extract_field("LOCATION"),
    calendar_type = calendar_type,
    uid = extract_field("UID"),
    updated_at = .parse_ics_datetime(extract_field("LAST-MODIFIED")),
    ics_url = ics_url
  )
}


#' Parse ICS datetime strings
#'
#' Converts ICS datetime format to POSIXct. Handles both datetime
#' (YYYYMMDDTHHMMSSZ) and date-only (YYYYMMDD) formats.
#'
#' @param dt_string Character string with ICS datetime
#'
#' @return POSIXct in UTC or NA
#'
#' @keywords internal
.parse_ics_datetime <- function(dt_string) {
  if (is.na(dt_string) || !nzchar(trimws(dt_string))) {
    return(as.POSIXct(NA, tz = "UTC"))
  }

  # Clean the string
  dt_string <- trimws(dt_string)

  # Try datetime format with UTC (YYYYMMDDTHHMMSSZ)
  if (grepl("^\\d{8}T\\d{6}Z?$", dt_string)) {
    parsed <- suppressWarnings(
      as.POSIXct(dt_string, format = "%Y%m%dT%H%M%SZ", tz = "UTC")
    )
    if (!is.na(parsed)) {
      return(parsed)
    }
    # Try without Z suffix
    parsed <- suppressWarnings(
      as.POSIXct(dt_string, format = "%Y%m%dT%H%M%S", tz = "UTC")
    )
    if (!is.na(parsed)) {
      return(parsed)
    }
  }

  # Try date-only format (YYYYMMDD)
  if (grepl("^\\d{8}$", dt_string)) {
    parsed <- suppressWarnings(
      as.POSIXct(dt_string, format = "%Y%m%d", tz = "UTC")
    )
    if (!is.na(parsed)) {
      return(parsed)
    }
  }

  as.POSIXct(NA, tz = "UTC")
}


# 5. Empty structure utilities -----

#' Create empty calendar data.table
#'
#' Returns an empty data.table with the correct column structure for
#' calendar events. Useful for error handling and empty results.
#'
#' @return Empty data.table with columns: title, start_date, end_date,
#'   description, location, calendar_type, uid, updated_at, ics_url
#'
#' @keywords internal
.empty_calendar_dt <- function() {
  data.table::data.table(
    title = character(0),
    start_date = as.POSIXct(character(0), tz = "UTC"),
    end_date = as.POSIXct(character(0), tz = "UTC"),
    description = character(0),
    location = character(0),
    calendar_type = character(0),
    uid = character(0),
    updated_at = as.POSIXct(character(0), tz = "UTC"),
    ics_url = character(0)
  )
}
