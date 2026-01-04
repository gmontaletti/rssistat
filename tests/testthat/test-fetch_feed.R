# 1. Input validation tests -----

test_that("fetch_feed validates url parameter", {
  expect_error(
    fetch_feed(123),
    "url must be a single non-NA character string"
  )

  expect_error(
    fetch_feed(c("http://example.com", "http://example2.com")),
    "url must be a single non-NA character string"
  )

  expect_error(
    fetch_feed(NA_character_),
    "url must be a single non-NA character string"
  )

  expect_error(
    fetch_feed("not-a-url"),
    "url must start with http:// or https://"
  )

  expect_error(
    fetch_feed("ftp://example.com"),
    "url must start with http:// or https://"
  )
})

test_that("fetch_feed validates since parameter", {
  expect_error(
    fetch_feed("http://example.com", since = "invalid-date"),
    "since must be NULL, Date, POSIXct, or character in 'YYYY-MM-DD' format"
  )

  expect_error(
    fetch_feed("http://example.com", since = 123),
    "since must be NULL, Date, POSIXct, or character in 'YYYY-MM-DD' format"
  )

  # Valid formats should not error (even if URL fails later)
  expect_error(
    fetch_feed("http://example.com", since = "2025-01-01"),
    NA,
    class = "error"
  )

  expect_error(
    fetch_feed("http://example.com", since = as.Date("2025-01-01")),
    NA,
    class = "error"
  )

  expect_error(
    fetch_feed("http://example.com", since = as.POSIXct("2025-01-01", tz = "UTC")),
    NA,
    class = "error"
  )
})

test_that("fetch_feed validates until parameter", {
  expect_error(
    fetch_feed("http://example.com", until = "invalid-date"),
    "until must be NULL, Date, POSIXct, or character in 'YYYY-MM-DD' format"
  )

  expect_error(
    fetch_feed("http://example.com", until = TRUE),
    "until must be NULL, Date, POSIXct, or character in 'YYYY-MM-DD' format"
  )
})

test_that("fetch_feed validates date range", {
  expect_error(
    fetch_feed("http://example.com", since = "2025-12-31", until = "2025-01-01"),
    "since must be earlier than until"
  )

  expect_error(
    fetch_feed("http://example.com", since = "2025-06-01", until = "2025-06-01"),
    "since must be earlier than until"
  )
})

test_that("fetch_feed validates timeout parameter", {
  expect_error(
    fetch_feed("http://example.com", timeout = "30"),
    "timeout must be a single positive number"
  )

  expect_error(
    fetch_feed("http://example.com", timeout = -5),
    "timeout must be a single positive number"
  )

  expect_error(
    fetch_feed("http://example.com", timeout = 0),
    "timeout must be a single positive number"
  )

  expect_error(
    fetch_feed("http://example.com", timeout = c(10, 20)),
    "timeout must be a single positive number"
  )
})

test_that("fetch_feed validates user_agent parameter", {
  expect_error(
    fetch_feed("http://example.com", user_agent = 123),
    "user_agent must be NULL or a single non-NA character string"
  )

  expect_error(
    fetch_feed("http://example.com", user_agent = c("agent1", "agent2")),
    "user_agent must be NULL or a single non-NA character string"
  )

  expect_error(
    fetch_feed("http://example.com", user_agent = NA_character_),
    "user_agent must be NULL or a single non-NA character string"
  )
})

test_that("fetch_feed validates verbose parameter", {
  expect_error(
    fetch_feed("http://example.com", verbose = "true"),
    "verbose must be a single non-NA logical value"
  )

  expect_error(
    fetch_feed("http://example.com", verbose = c(TRUE, FALSE)),
    "verbose must be a single non-NA logical value"
  )

  expect_error(
    fetch_feed("http://example.com", verbose = NA),
    "verbose must be a single non-NA logical value"
  )
})


# 2. Error handling tests -----

test_that("fetch_feed handles network errors gracefully", {
  # Non-existent domain should produce warning and return empty feed
  expect_warning(
    result <- fetch_feed("http://this-domain-does-not-exist-12345.com", timeout = 5)
  )

  expect_s3_class(result, "rss_feed")
  expect_equal(nrow(result$meta), 0)
  expect_equal(nrow(result$items), 0)
  expect_equal(nrow(result$links), 0)
})

test_that("fetch_feed handles HTTP errors gracefully", {
  # 404 error should produce warning and return empty feed
  # Note: httr2 may throw error instead of returning 404 response
  result <- suppressWarnings(
    fetch_feed("https://httpbin.org/status/404", timeout = 10)
  )

  expect_s3_class(result, "rss_feed")
  expect_equal(nrow(result$meta), 0)
})


# 3. Structure tests -----

test_that("fetch_feed returns correct structure", {
  skip_if_offline()

  # Use a known stable RSS feed for testing
  # Note: This test requires internet connection
  result <- tryCatch(
    fetch_feed("https://www.istat.it/en/feed", timeout = 10),
    error = function(e) NULL
  )

  # Skip test if feed unavailable
  skip_if(is.null(result), "Feed unavailable for testing")

  # Check S3 class
  expect_s3_class(result, "rss_feed")
  expect_s3_class(result, "list")

  # Check structure
  expect_named(result, c("meta", "items", "links"))

  # Check meta structure
  expect_s3_class(result$meta, "data.table")
  expect_named(
    result$meta,
    c("title", "link", "description", "language", "last_build",
      "theme", "feed_url", "fetched_at")
  )
  expect_equal(nrow(result$meta), 1)

  # Check items structure
  expect_s3_class(result$items, "data.table")
  expect_named(
    result$items,
    c("title", "link", "description", "content", "pub_date",
      "creator", "categories", "guid")
  )

  # Check links structure
  expect_s3_class(result$links, "data.table")
  expect_named(
    result$links,
    c("item_title", "item_link", "url", "text", "type", "source")
  )

  # Check data types
  expect_type(result$meta$title, "character")
  expect_s3_class(result$meta$fetched_at, "POSIXct")
  expect_s3_class(result$items$pub_date, "POSIXct")
  expect_type(result$items$categories, "list")
})


# 4. S3 method tests -----

test_that("print.rss_feed works correctly", {
  skip_if_offline()

  result <- tryCatch(
    fetch_feed("https://www.istat.it/en/feed", timeout = 10),
    error = function(e) NULL
  )

  skip_if(is.null(result), "Feed unavailable for testing")

  # Should print without error and return invisibly
  expect_output(print(result), "RSS Feed:")
  expect_output(print(result), "Theme:")
  expect_output(print(result), "Items:")
  expect_output(print(result), "Links:")

  expect_invisible(print(result))
})

test_that("summary.rss_feed works correctly", {
  skip_if_offline()

  result <- tryCatch(
    fetch_feed("https://www.istat.it/en/feed", timeout = 10),
    error = function(e) NULL
  )

  skip_if(is.null(result), "Feed unavailable for testing")

  if (nrow(result$links) > 0) {
    summ <- summary(result)

    expect_s3_class(summ, "data.table")
    expect_true(all(c("type", "N") %in% names(summ)))
    expect_true(all(summ$N > 0))

    # Should be sorted by frequency (descending)
    expect_true(all(diff(summ$N) <= 0))
  } else {
    # No links case
    expect_output(summary(result), "No links found")
  }
})

test_that("print.rss_feed handles empty feed", {
  empty <- .empty_rss_feed()

  expect_output(print(empty), "RSS Feed:")
  expect_output(print(empty), "Items: 0")
  expect_output(print(empty), "Links: 0")
})


# 5. Date filtering tests -----

test_that("date filtering works correctly", {
  skip_if_offline()

  # Fetch full feed
  full_result <- tryCatch(
    fetch_feed("https://www.istat.it/en/feed", timeout = 10),
    error = function(e) NULL
  )

  skip_if(is.null(full_result), "Feed unavailable for testing")
  skip_if(nrow(full_result$items) == 0, "No items in feed")

  # Get a date in the middle of the range
  dates <- full_result$items$pub_date[!is.na(full_result$items$pub_date)]
  skip_if(length(dates) < 2, "Not enough dated items for filtering test")

  mid_date <- median(dates)

  # Filter with since
  recent_result <- fetch_feed(
    "https://www.istat.it/en/feed",
    since = mid_date,
    timeout = 10
  )

  # Should have fewer or equal items
  expect_lte(nrow(recent_result$items), nrow(full_result$items))

  # All non-NA dates should be >= since date
  recent_dates <- recent_result$items$pub_date[!is.na(recent_result$items$pub_date)]
  if (length(recent_dates) > 0) {
    expect_true(all(recent_dates >= mid_date))
  }
})


# 6. Helper function tests -----

test_that(".validate_date_param works correctly", {
  # NULL input
  expect_null(.validate_date_param(NULL, "test"))

  # Date input
  date_obj <- as.Date("2025-01-15")
  result <- .validate_date_param(date_obj, "test")
  expect_s3_class(result, "POSIXct")
  expect_equal(attr(result, "tzone"), "UTC")

  # POSIXct input
  posix_obj <- as.POSIXct("2025-01-15 10:30:00", tz = "America/New_York")
  result <- .validate_date_param(posix_obj, "test")
  expect_s3_class(result, "POSIXct")
  expect_equal(attr(result, "tzone"), "UTC")

  # Character input
  result <- .validate_date_param("2025-01-15", "test")
  expect_s3_class(result, "POSIXct")
  expect_equal(attr(result, "tzone"), "UTC")

  # Invalid inputs
  expect_error(
    .validate_date_param("invalid", "test"),
    "test must be NULL"
  )

  expect_error(
    .validate_date_param(123, "test"),
    "test must be NULL"
  )
})

test_that(".safe_xml_text handles edge cases", {
  # Empty node
  expect_equal(.safe_xml_text(xml2::xml_missing()), NA_character_)

  # Whitespace-only text
  doc <- xml2::read_xml("<root>   </root>")
  expect_equal(.safe_xml_text(doc), NA_character_)
})

test_that(".extract_links_from_html extracts links correctly", {
  html <- '
    <div>
      <a href="https://example.com/report.pdf">Download Report</a>
      <a href="https://example.com/data.xlsx">Excel Data</a>
      <a href="https://example.com/page">Web Page</a>
    </div>
  '

  result <- .extract_links_from_html(
    html,
    item_title = "Test Item",
    item_link = "https://example.com/item",
    source = "content"
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 3)
  expect_equal(result$type, c("pdf", "xlsx", "page"))
  expect_true(all(result$source == "content"))
  expect_true(all(result$item_title == "Test Item"))
})

test_that(".extract_links_from_html handles empty/invalid HTML", {
  expect_null(.extract_links_from_html("", "title", "link", "content"))
  expect_null(.extract_links_from_html(NA_character_, "title", "link", "content"))
  expect_null(.extract_links_from_html("<div>No links here</div>", "title", "link", "content"))
})

test_that(".extract_links_from_html skips relative URLs", {
  html <- '
    <div>
      <a href="relative/path">Relative</a>
      <a href="/absolute/path">Absolute</a>
      <a href="#anchor">Anchor</a>
      <a href="https://example.com/valid">Valid</a>
    </div>
  '

  result <- .extract_links_from_html(html, "title", "link", "content")

  # Should only extract the valid absolute http(s) URL
  expect_equal(nrow(result), 1)
  expect_equal(result$url, "https://example.com/valid")
})
