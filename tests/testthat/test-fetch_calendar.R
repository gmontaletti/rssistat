# 1. Input validation tests -----

test_that("fetch_calendar validates calendars parameter", {
  expect_error(
    fetch_calendar(calendars = "invalid"),
    "calendars must be one of"
  )
  expect_error(
    fetch_calendar(calendars = 123),
    "calendars must be a non-empty character vector"
  )
  expect_error(
    fetch_calendar(calendars = character(0)),
    "calendars must be a non-empty character vector"
  )
})

test_that("fetch_calendar validates date parameters", {
  expect_error(
    fetch_calendar(since = "invalid-date"),
    "since must be"
  )
  expect_error(
    fetch_calendar(until = "not-a-date"),
    "until must be"
  )
  expect_error(
    fetch_calendar(since = "2025-12-31", until = "2025-01-01"),
    "since must be earlier than until"
  )
})

test_that("fetch_calendar validates timeout parameter", {
  expect_error(
    fetch_calendar(timeout = "fast"),
    "timeout must be a positive number"
  )
  expect_error(
    fetch_calendar(timeout = -10),
    "timeout must be a positive number"
  )
  expect_error(
    fetch_calendar(timeout = c(10, 20)),
    "timeout must be a positive number"
  )
})

test_that("fetch_calendar validates verbose parameter", {
  expect_error(
    fetch_calendar(verbose = "yes"),
    "verbose must be a single non-NA logical value"
  )
  expect_error(
    fetch_calendar(verbose = NA),
    "verbose must be a single non-NA logical value"
  )
})


# 2. Helper function tests -----

test_that(".istat_calendar_urls returns correct structure", {
  urls <- .istat_calendar_urls()

  expect_type(urls, "character")
  expect_named(urls)
  expect_length(urls, 5)
  expect_true(all(
    c("comunicati", "note_stampa", "prodotti", "banche_dati", "convegni") %in%
      names(urls)
  ))

  # All should be valid URLs
  expect_true(all(grepl("^https://", urls)))
  expect_true(all(grepl("\\.ics$", urls)))
})

test_that(".parse_ics_datetime handles various formats", {
  # Standard UTC format
  result <- .parse_ics_datetime("20150313T090000Z")
  expect_s3_class(result, "POSIXct")
  expect_equal(format(result, "%Y-%m-%d %H:%M:%S"), "2015-03-13 09:00:00")

  # Without Z suffix
  result <- .parse_ics_datetime("20150313T090000")
  expect_s3_class(result, "POSIXct")
  expect_equal(format(result, "%Y-%m-%d %H:%M:%S"), "2015-03-13 09:00:00")

  # Date-only format
  result <- .parse_ics_datetime("20150313")
  expect_s3_class(result, "POSIXct")
  expect_equal(format(result, "%Y-%m-%d"), "2015-03-13")

  # NA handling
  expect_true(is.na(.parse_ics_datetime(NA_character_)))
  expect_true(is.na(.parse_ics_datetime("")))
  expect_true(is.na(.parse_ics_datetime("   ")))

  # Invalid format
  expect_true(is.na(.parse_ics_datetime("not-a-date")))
})

test_that(".empty_calendar_dt returns correct structure", {
  result <- .empty_calendar_dt()

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
  expect_named(
    result,
    c(
      "title",
      "start_date",
      "end_date",
      "description",
      "location",
      "calendar_type",
      "uid",
      "updated_at",
      "ics_url"
    )
  )

  # Check column types
  expect_type(result$title, "character")
  expect_s3_class(result$start_date, "POSIXct")
  expect_s3_class(result$end_date, "POSIXct")
  expect_type(result$description, "character")
  expect_type(result$calendar_type, "character")
})

test_that(".parse_ics_event extracts fields correctly", {
  event_lines <- c(
    "DTSTART:20150313T090000Z",
    "DTEND:20150313T100000Z",
    "SUMMARY:Test Event Title",
    "DESCRIPTION:Test description with\\, escaped comma",
    "LOCATION:Test Location",
    "UID:test123@google.com",
    "LAST-MODIFIED:20150101T120000Z"
  )

  result <- .parse_ics_event(event_lines, "comunicati", "https://example.com")

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
  expect_equal(result$title, "Test Event Title")
  expect_equal(result$description, "Test description with, escaped comma")
  expect_equal(result$location, "Test Location")
  expect_equal(result$uid, "test123@google.com")
  expect_equal(result$calendar_type, "comunicati")
  expect_equal(format(result$start_date, "%Y-%m-%d %H:%M"), "2015-03-13 09:00")
})

test_that(".parse_ics_content handles valid ICS content", {
  ics_content <- "BEGIN:VCALENDAR
BEGIN:VEVENT
DTSTART:20150313T090000Z
DTEND:20150313T100000Z
SUMMARY:Event One
UID:event1@google.com
END:VEVENT
BEGIN:VEVENT
DTSTART:20150314T090000Z
DTEND:20150314T100000Z
SUMMARY:Event Two
UID:event2@google.com
END:VEVENT
END:VCALENDAR"

  result <- .parse_ics_content(ics_content, "comunicati", "https://example.com")

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2)
  expect_equal(result$title, c("Event One", "Event Two"))
})

test_that(".parse_ics_content handles line folding", {
  ics_content <- "BEGIN:VCALENDAR
BEGIN:VEVENT
DTSTART:20150313T090000Z
SUMMARY:Short Title
DESCRIPTION:This is a very long description that continues
 on the next line with a leading space
UID:test@google.com
END:VEVENT
END:VCALENDAR"

  result <- .parse_ics_content(ics_content, "comunicati", "https://example.com")

  expect_equal(nrow(result), 1)
  expect_true(grepl("continues", result$description))
  expect_true(grepl("on the next line", result$description))
})


# 3. Integration tests -----

test_that("fetch_calendar returns correct structure on success", {
  skip_if_offline()

  result <- tryCatch(
    fetch_calendar(calendars = "comunicati", timeout = 15),
    error = function(e) NULL
  )

  skip_if(is.null(result), "Calendar unavailable for testing")

  expect_s3_class(result, "data.table")
  expect_named(
    result,
    c(
      "title",
      "start_date",
      "end_date",
      "description",
      "location",
      "calendar_type",
      "uid",
      "updated_at",
      "ics_url"
    )
  )

  # All rows should have calendar_type = "comunicati"
  if (nrow(result) > 0) {
    expect_true(all(result$calendar_type == "comunicati"))
    expect_true(all(!is.na(result$uid)))
  }
})

test_that("fetch_calendar date filtering works", {
  skip_if_offline()

  # Get recent events only
  result <- tryCatch(
    fetch_calendar(
      calendars = "comunicati",
      since = "2024-01-01",
      timeout = 15
    ),
    error = function(e) NULL
  )

  skip_if(is.null(result), "Calendar unavailable for testing")

  if (nrow(result) > 0) {
    # All start_date should be >= 2024-01-01
    expect_true(all(
      result$start_date >= as.POSIXct("2024-01-01", tz = "UTC") |
        is.na(result$start_date)
    ))
  }
})

test_that("fetch_calendar verbose mode works", {
  skip_if_offline()

  expect_message(
    tryCatch(
      fetch_calendar(calendars = "comunicati", verbose = TRUE, timeout = 15),
      error = function(e) NULL
    ),
    "Fetching comunicati"
  )
})
