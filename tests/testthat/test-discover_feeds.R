# 1. Test input validation -----

test_that("discover_feeds validates url parameter", {
  expect_error(discover_feeds(NULL), "'url' must be a single character string")
  expect_error(discover_feeds(123), "'url' must be a single character string")
  expect_error(discover_feeds(c("a", "b")), "'url' must be a single character string")
  expect_error(discover_feeds(NA_character_), "'url' must be a single character string")
  expect_error(discover_feeds("not-a-url"), "'url' must start with http://")
})

test_that("discover_feeds validates include_anchors parameter", {
  expect_error(
    discover_feeds("http://example.com", include_anchors = "yes"),
    "'include_anchors' must be TRUE or FALSE"
  )
  expect_error(
    discover_feeds("http://example.com", include_anchors = c(TRUE, FALSE)),
    "'include_anchors' must be TRUE or FALSE"
  )
})

test_that("discover_feeds validates timeout parameter", {
  expect_error(
    discover_feeds("http://example.com", timeout = "30"),
    "'timeout' must be a positive number"
  )
  expect_error(
    discover_feeds("http://example.com", timeout = -1),
    "'timeout' must be a positive number"
  )
})

test_that("discover_feeds validates user_agent parameter", {
  expect_error(
    discover_feeds("http://example.com", user_agent = 123),
    "'user_agent' must be NULL or a single character string"
  )
})

test_that("discover_feeds validates verbose parameter", {
  expect_error(
    discover_feeds("http://example.com", verbose = "yes"),
    "'verbose' must be TRUE or FALSE"
  )
})


# 2. Test return structure -----

test_that(".empty_feeds_dt returns correct structure", {
  empty_dt <- rssistat:::.empty_feeds_dt()

  expect_s3_class(empty_dt, "data.table")
  expect_equal(nrow(empty_dt), 0)
  expect_named(
    empty_dt,
    c("url", "title", "type", "source", "source_url", "discovered_at")
  )
  expect_type(empty_dt$url, "character")
  expect_type(empty_dt$title, "character")
  expect_type(empty_dt$type, "character")
  expect_type(empty_dt$source, "character")
  expect_type(empty_dt$source_url, "character")
  expect_s3_class(empty_dt$discovered_at, "POSIXct")
})


# 3. Integration tests (skip on CRAN and offline) -----

test_that("discover_feeds works with real URL", {
  skip_on_cran()
  skip_if_offline()

  # Use a stable test page
  feeds <- discover_feeds("https://www.r-project.org/")

  expect_s3_class(feeds, "data.table")
  expect_named(
    feeds,
    c("url", "title", "type", "source", "source_url", "discovered_at")
  )
})

test_that("discover_feeds handles page without feeds", {
  skip_on_cran()
  skip_if_offline()

  # Example.com has no RSS feeds
  feeds <- discover_feeds("https://example.com/")

  expect_s3_class(feeds, "data.table")
  expect_equal(nrow(feeds), 0)
})

test_that("discover_feeds handles invalid domain", {
  skip_on_cran()
  skip_if_offline()

  expect_error(
    discover_feeds("https://this-domain-does-not-exist-12345.com/"),
    "Failed to fetch URL"
  )
})
