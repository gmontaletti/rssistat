#' Download Attachments from RSS Feed
#'
#' Downloads files (PDF, Excel, CSV, ZIP, etc.) linked in RSS feed items.
#' Supports automatic directory organization by theme and date, with progress
#' tracking and error handling.
#'
#' @param feed An rss_feed object created by \code{\link{fetch_feed}}.
#' @param types Character vector of file extensions to download. Case-insensitive.
#'   Default: \code{c("pdf", "xlsx", "xls", "csv", "zip")}.
#' @param destdir Destination directory for downloads. Created if it does not exist.
#'   Default: \code{"."} (current directory).
#' @param create_subdir Logical. If \code{TRUE}, creates subdirectory structure
#'   \code{{theme}/{YYYYMMDD}/} based on feed metadata. Default: \code{TRUE}.
#' @param overwrite Logical. If \code{TRUE}, overwrites existing files. If \code{FALSE},
#'   skips downloads for existing files. Default: \code{FALSE}.
#' @param timeout Numeric. Download timeout per file in seconds. Default: \code{60}.
#' @param verbose Logical. If \code{TRUE}, prints progress messages during download.
#'   Default: \code{FALSE}.
#'
#' @return A \code{data.table} with one row per download attempt, containing:
#' \describe{
#'   \item{url}{Character. Original URL of the file.}
#'   \item{filename}{Character. Downloaded file name.}
#'   \item{path}{Character. Full local path to downloaded file.}
#'   \item{size}{Numeric. File size in bytes (NA if download failed).}
#'   \item{status}{Character. One of "downloaded", "skipped" (file exists), or "failed".}
#'   \item{error}{Character. Error message if status is "failed", NA otherwise.}
#' }
#'
#' @examples
#' \dontrun{
#' # Fetch feed and download all PDFs
#' feed <- fetch_feed("https://www.istat.it/en/theme/prices/feed")
#' results <- download_attachments(feed, types = "pdf", destdir = "downloads")
#' print(results)
#'
#' # Download multiple file types with progress messages
#' results <- download_attachments(
#'   feed,
#'   types = c("pdf", "xlsx", "csv"),
#'   destdir = "data",
#'   verbose = TRUE
#' )
#'
#' # Download without subdirectory structure, overwrite existing
#' results <- download_attachments(
#'   feed,
#'   types = "xlsx",
#'   destdir = "flat_downloads",
#'   create_subdir = FALSE,
#'   overwrite = TRUE
#' )
#' }
#'
#' @export
#' @importFrom data.table data.table setDT
#' @importFrom httr2 request req_timeout req_perform req_error
download_attachments <- function(feed,
                                 types = c("pdf", "xlsx", "xls", "csv", "zip"),
                                 destdir = ".",
                                 create_subdir = TRUE,
                                 overwrite = FALSE,
                                 timeout = 60,
                                 verbose = FALSE) {

  # 1. Input validation -----

  if (!inherits(feed, "rss_feed")) {
    stop("'feed' must be an rss_feed object created by fetch_feed()", call. = FALSE)
  }

  if (!is.character(types) || length(types) == 0) {
    stop("'types' must be a non-empty character vector", call. = FALSE)
  }

  if (!is.character(destdir) || length(destdir) != 1 || nchar(destdir) == 0) {
    stop("'destdir' must be a single non-empty string", call. = FALSE)
  }

  if (!is.logical(create_subdir) || length(create_subdir) != 1 || is.na(create_subdir)) {
    stop("'create_subdir' must be TRUE or FALSE", call. = FALSE)
  }

  if (!is.logical(overwrite) || length(overwrite) != 1 || is.na(overwrite)) {
    stop("'overwrite' must be TRUE or FALSE", call. = FALSE)
  }

  if (!is.logical(verbose) || length(verbose) != 1 || is.na(verbose)) {
    stop("'verbose' must be TRUE or FALSE", call. = FALSE)
  }

  if (!is.numeric(timeout) || length(timeout) != 1 || is.na(timeout) || timeout <= 0) {
    stop("'timeout' must be a positive number", call. = FALSE)
  }

  # 2. Filter links by type -----

  # Normalize types to lowercase for case-insensitive matching
  types <- tolower(types)

  # Check if feed has links component
  if (is.null(feed$links) || nrow(feed$links) == 0) {
    if (verbose) {
      message("No links found in feed")
    }
    return(data.table::data.table(
      url = character(0),
      filename = character(0),
      path = character(0),
      size = numeric(0),
      status = character(0),
      error = character(0)
    ))
  }

  # Filter links by type
  to_download <- feed$links[tolower(type) %in% types]

  if (nrow(to_download) == 0) {
    if (verbose) {
      message(sprintf("No links found with types: %s", paste(types, collapse = ", ")))
    }
    return(data.table::data.table(
      url = character(0),
      filename = character(0),
      path = character(0),
      size = numeric(0),
      status = character(0),
      error = character(0)
    ))
  }

  if (verbose) {
    message(sprintf("Found %d file(s) to download", nrow(to_download)))
  }

  # 3. Create destination directory -----

  download_dir <- destdir

  if (create_subdir) {
    # Extract theme from feed metadata
    theme <- feed$meta$theme
    if (is.null(theme) || is.na(theme) || nchar(theme) == 0) {
      theme <- "rss_downloads"
    }

    # Clean theme for use in directory name (remove special characters)
    theme <- gsub("[^[:alnum:]_-]", "_", theme)

    # Get date from most recent item or current date
    date_str <- if (!is.null(feed$items) && nrow(feed$items) > 0 && "pub_date" %in% names(feed$items)) {
      max_date <- max(feed$items$pub_date, na.rm = TRUE)
      if (!is.na(max_date)) {
        format(max_date, "%Y%m%d")
      } else {
        format(Sys.Date(), "%Y%m%d")
      }
    } else {
      format(Sys.Date(), "%Y%m%d")
    }

    download_dir <- file.path(destdir, theme, date_str)
  }

  # Create directory if it doesn't exist
  if (!dir.exists(download_dir)) {
    dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
    if (verbose) {
      message(sprintf("Created directory: %s", download_dir))
    }
  }

  # 4. Download each file -----

  results <- vector("list", nrow(to_download))

  for (i in seq_len(nrow(to_download))) {
    url <- to_download$url[i]

    if (verbose) {
      message(sprintf("[%d/%d] Downloading: %s", i, nrow(to_download), url))
    }

    # Initialize result for this download
    result <- list(
      url = url,
      filename = NA_character_,
      path = NA_character_,
      size = NA_real_,
      status = "failed",
      error = NA_character_
    )

    tryCatch({
      # Generate filename
      filename <- .generate_filename(url)
      full_path <- file.path(download_dir, filename)

      result$filename <- filename
      result$path <- full_path

      # Check if file exists
      if (file.exists(full_path) && !overwrite) {
        result$status <- "skipped"
        result$size <- file.info(full_path)$size
        if (verbose) {
          message(sprintf("  Skipped (file exists): %s", filename))
        }
      } else {
        # Download file
        req <- httr2::request(url)
        req <- httr2::req_timeout(req, timeout)

        # Perform download
        resp <- httr2::req_perform(req, path = full_path)

        # Check file was created and get size
        if (file.exists(full_path)) {
          result$status <- "downloaded"
          result$size <- file.info(full_path)$size
          if (verbose) {
            message(sprintf("  Downloaded: %s (%s bytes)", filename, result$size))
          }
        } else {
          result$status <- "failed"
          result$error <- "File not created after download"
        }
      }
    }, error = function(e) {
      result$status <<- "failed"
      result$error <<- conditionMessage(e)
      if (verbose) {
        message(sprintf("  Failed: %s", conditionMessage(e)))
      }
    })

    results[[i]] <- result
  }

  # 5. Return results data.table -----

  results_dt <- data.table::rbindlist(results, fill = TRUE)

  if (verbose) {
    n_downloaded <- sum(results_dt$status == "downloaded")
    n_skipped <- sum(results_dt$status == "skipped")
    n_failed <- sum(results_dt$status == "failed")
    message(sprintf(
      "Download complete: %d downloaded, %d skipped, %d failed",
      n_downloaded, n_skipped, n_failed
    ))
  }

  return(results_dt)
}


#' Generate Filename from URL
#'
#' Internal helper function to extract or generate a valid filename from a URL.
#' Attempts to use the basename of the URL path, removing query strings and
#' ensuring the result is a valid filename.
#'
#' @param url Character. The URL to extract filename from.
#' @param resp Optional httr2 response object. If provided, attempts to extract
#'   filename from Content-Disposition header (currently not implemented).
#'
#' @return Character. A valid filename.
#'
#' @noRd
#' @keywords internal
.generate_filename <- function(url, resp = NULL) {

  # Future enhancement: extract from Content-Disposition header if resp provided
  # For now, use basename approach

  # Parse URL to get path
  parsed <- tryCatch(
    httr2::url_parse(url),
    error = function(e) list(path = url)
  )

  # Get basename
  filename <- basename(parsed$path)

  # Remove query string if present
  filename <- gsub("\\?.*$", "", filename)

  # If empty or just extension, generate name
  if (nchar(filename) == 0 || filename == ".") {
    filename <- paste0("download_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  }

  # Sanitize filename (remove/replace invalid characters)
  filename <- gsub("[<>:\"/\\|?*]", "_", filename)

  # Ensure we have an extension
  if (!grepl("\\.[[:alnum:]]+$", filename)) {
    filename <- paste0(filename, ".bin")
  }

  return(filename)
}
