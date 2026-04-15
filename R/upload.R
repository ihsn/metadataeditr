#' Get server upload limits
#'
#' Returns the server's maximum chunk size, recommended chunk size, and maximum
#' file size for resumable uploads.
#'
#' @param api_key API key (optional if set via set_api_key)
#' @param api_base_url API base endpoint (optional if set via set_api_url)
#'
#' @examples
#'
#' upload_limits()
#'
#' @export
upload_limits <- function(api_key=NULL, api_base_url=NULL) {
  if (is.null(api_key)) api_key <- get_api_key()

  if (is.null(api_base_url)) {
    url <- get_api_url("uploads/limits")
  } else {
    url <- paste0(api_base_url, "/uploads/limits")
  }

  httpResponse <- GET(url,
                      add_headers("X-API-KEY" = api_key),
                      accept_json(),
                      verbose(get_verbose()))

  if (httpResponse$status_code != 200) {
    warning(content(httpResponse, "text"))
  }

  list(
    status_code = httpResponse$status_code,
    response    = metadataedit_http_response_json(httpResponse)
  )
}


#' Initialize a resumable upload session
#'
#' Creates a new upload session on the server and returns an upload_id to use
#' for subsequent chunk uploads.
#'
#' @param filename (required) Name of the file being uploaded
#' @param total_size (required) Total file size in bytes
#' @param chunk_size (required) Size of each chunk in bytes
#' @param metadata Optional named list of additional metadata to store with the upload
#' @param api_key API key (optional if set via set_api_key)
#' @param api_base_url API base endpoint (optional if set via set_api_url)
#'
#' @return List with status_code and response (includes upload_id, upload_url, status_url)
#'
#' @export
upload_init <- function(filename,
                        total_size,
                        chunk_size,
                        metadata=list(),
                        api_key=NULL,
                        api_base_url=NULL) {
  if (is.null(api_key)) api_key <- get_api_key()

  if (is.null(api_base_url)) {
    url <- get_api_url("uploads/init")
  } else {
    url <- paste0(api_base_url, "/uploads/init")
  }

  total_chunks <- ceiling(total_size / chunk_size)

  body <- list(
    filename     = filename,
    total_size   = total_size,
    chunk_size   = chunk_size,
    total_chunks = total_chunks,
    metadata     = metadata
  )

  httpResponse <- POST(url,
                       add_headers("X-API-KEY" = api_key),
                       body          = body,
                       content_type_json(),
                       encode        = "json",
                       accept_json(),
                       verbose(get_verbose()))

  if (httpResponse$status_code != 200) {
    warning(content(httpResponse, "text"))
  }

  list(
    status_code = httpResponse$status_code,
    response    = metadataedit_http_response_json(httpResponse)
  )
}


#' Upload a single chunk
#'
#' Sends one binary chunk to the server for an active upload session.
#'
#' @param upload_id (required) Upload session ID returned by upload_init
#' @param chunk_number (required) Zero-based chunk index
#' @param chunk_data (required) Raw bytes for this chunk (use readBin)
#' @param chunk_size (required) Size of the chunk in bytes (for server-side validation)
#' @param api_key API key (optional if set via set_api_key)
#' @param api_base_url API base endpoint (optional if set via set_api_url)
#'
#' @return List with status_code and response (includes upload_status and progress)
#'
#' @export
upload_chunk <- function(upload_id,
                         chunk_number,
                         chunk_data,
                         chunk_size,
                         api_key=NULL,
                         api_base_url=NULL) {
  if (is.null(api_key)) api_key <- get_api_key()

  if (is.null(api_base_url)) {
    url <- get_api_url(paste0("uploads/chunk/", upload_id))
  } else {
    url <- paste0(api_base_url, "/uploads/chunk/", upload_id)
  }

  httpResponse <- POST(url,
                       add_headers(
                         "X-API-KEY"            = api_key,
                         "X-Upload-Chunk-Number" = as.character(chunk_number),
                         "X-Upload-Chunk-Size"   = as.character(chunk_size),
                         "Content-Type"          = "application/octet-stream"
                       ),
                       body   = chunk_data,
                       encode = "raw",
                       verbose(get_verbose()))

  if (httpResponse$status_code != 200) {
    warning(content(httpResponse, "text"))
  }

  list(
    status_code = httpResponse$status_code,
    response    = metadataedit_http_response_json(httpResponse)
  )
}


#' Get the status of a resumable upload
#'
#' Returns the current progress, list of uploaded chunks, and overall status of
#' an upload session.
#'
#' @param upload_id (required) Upload session ID
#' @param api_key API key (optional if set via set_api_key)
#' @param api_base_url API base endpoint (optional if set via set_api_url)
#'
#' @examples
#'
#' upload_status("abc123-upload-id")
#'
#' @export
upload_status <- function(upload_id, api_key=NULL, api_base_url=NULL) {
  if (is.null(api_key)) api_key <- get_api_key()

  if (is.null(api_base_url)) {
    url <- get_api_url(paste0("uploads/status/", upload_id))
  } else {
    url <- paste0(api_base_url, "/uploads/status/", upload_id)
  }

  httpResponse <- GET(url,
                      add_headers("X-API-KEY" = api_key),
                      accept_json(),
                      verbose(get_verbose()))

  if (httpResponse$status_code != 200) {
    warning(content(httpResponse, "text"))
  }

  list(
    status_code = httpResponse$status_code,
    response    = metadataedit_http_response_json(httpResponse)
  )
}


#' Delete a resumable upload session
#'
#' Cancels and removes an upload session and its temporary chunks from the server.
#'
#' @param upload_id (required) Upload session ID to delete
#' @param api_key API key (optional if set via set_api_key)
#' @param api_base_url API base endpoint (optional if set via set_api_url)
#'
#' @examples
#'
#' upload_delete("abc123-upload-id")
#'
#' @export
upload_delete <- function(upload_id, api_key=NULL, api_base_url=NULL) {
  if (is.null(api_key)) api_key <- get_api_key()

  if (is.null(api_base_url)) {
    url <- get_api_url(paste0("uploads/delete/", upload_id))
  } else {
    url <- paste0(api_base_url, "/uploads/delete/", upload_id)
  }

  httpResponse <- DELETE(url,
                         add_headers("X-API-KEY" = api_key),
                         accept_json(),
                         verbose(get_verbose()))

  if (httpResponse$status_code != 200) {
    warning(content(httpResponse, "text"))
  }

  list(
    status_code = httpResponse$status_code,
    response    = metadataedit_http_response_json(httpResponse)
  )
}


#' Upload a file using chunked resumable upload
#'
#' Splits a local file into chunks and uploads them to the server one at a time.
#' Already-uploaded chunks are skipped automatically, making the upload resumable
#' if interrupted.
#'
#' The chunk size defaults to 10 MB. You can override it or call \code{upload_limits()}
#' first to fetch the server's recommended chunk size.
#'
#' @param file_path (required) Path to the local file to upload
#' @param chunk_size Chunk size in bytes. Defaults to 10 MB (10485760)
#' @param metadata Optional named list of metadata to store with the upload session
#' @param resume_upload_id Upload session ID to resume a previous upload. If NULL a new
#'   session is initialized automatically.
#' @param show_progress Print progress to the console (TRUE/FALSE). Default TRUE.
#' @param api_key API key (optional if set via set_api_key)
#' @param api_base_url API base endpoint (optional if set via set_api_url)
#'
#' @return List with:
#'   \itemize{
#'     \item \code{status_code} - HTTP status of the final chunk response
#'     \item \code{upload_id}   - The upload session ID
#'     \item \code{response}    - Full response body from the last chunk POST
#'   }
#'
#' @examples
#'
#' # Basic usage
#' result <- upload_file_chunked(file_path = "/path/to/large-file.zip")
#'
#' # Use a custom 5 MB chunk size
#' result <- upload_file_chunked(
#'   file_path  = "/path/to/large-file.zip",
#'   chunk_size = 5242880
#' )
#'
#' # Resume an interrupted upload
#' result <- upload_file_chunked(
#'   file_path        = "/path/to/large-file.zip",
#'   resume_upload_id = "abc123-upload-id"
#' )
#'
#' @export
upload_file_chunked <- function(file_path,
                                chunk_size=10485760,
                                metadata=list(),
                                resume_upload_id=NULL,
                                show_progress=TRUE,
                                api_key=NULL,
                                api_base_url=NULL) {
  if (is.null(api_key)) api_key <- get_api_key()

  if (!file.exists(file_path)) {
    stop(paste("File not found:", file_path))
  }

  filename   <- basename(file_path)
  total_size <- file.info(file_path)$size
  total_chunks <- ceiling(total_size / chunk_size)

  # ── Initialize or resume session ─────────────────────────────────────────
  upload_id      <- resume_upload_id
  chunks_done    <- c()

  if (!is.null(upload_id)) {
    status_result <- upload_status(upload_id, api_key=api_key, api_base_url=api_base_url)
    if (status_result$status_code == 200) {
      chunks_done <- as.integer(status_result$response$uploaded_chunks)
      if (show_progress) {
        message(sprintf("Resuming upload %s — %d/%d chunks already uploaded",
                        upload_id, length(chunks_done), total_chunks))
      }
    } else {
      warning("Could not retrieve status for upload_id ", upload_id, ". Starting a new session.")
      upload_id <- NULL
    }
  }

  if (is.null(upload_id)) {
    init_result <- upload_init(
      filename     = filename,
      total_size   = total_size,
      chunk_size   = chunk_size,
      metadata     = metadata,
      api_key      = api_key,
      api_base_url = api_base_url
    )

    if (init_result$status_code != 200) {
      stop("Failed to initialize upload session")
    }

    upload_id <- init_result$response$upload_id

    if (show_progress) {
      message(sprintf("Initialized upload session %s — %d chunks of %s bytes each",
                      upload_id, total_chunks, format(chunk_size, big.mark=",")))
    }
  }

  # ── Upload chunks ─────────────────────────────────────────────────────────
  con <- file(file_path, "rb")
  on.exit(close(con), add=TRUE)

  last_response <- NULL

  for (i in seq_len(total_chunks)) {
    chunk_number <- i - 1L  # zero-based

    # Skip chunks the server already has
    if (chunk_number %in% chunks_done) {
      if (show_progress) {
        message(sprintf("  Chunk %d/%d — skipped (already uploaded)", i, total_chunks))
      }
      next
    }

    # Seek to the correct byte offset before reading (safe for both normal and
    # resume paths where some chunks may have been skipped above)
    seek(con, where = chunk_number * chunk_size)
    chunk_data <- readBin(con, what="raw", n=chunk_size)

    if (length(chunk_data) == 0) break

    result <- upload_chunk(
      upload_id    = upload_id,
      chunk_number = chunk_number,
      chunk_data   = chunk_data,
      chunk_size   = length(chunk_data),  # actual bytes sent (last chunk may be smaller)
      api_key      = api_key,
      api_base_url = api_base_url
    )

    if (result$status_code != 200) {
      stop(sprintf("Chunk %d failed (HTTP %d): %s",
                   i, result$status_code, content(result$response, "text")))
    }

    last_response <- result

    if (show_progress) {
      progress <- result$response$progress
      message(sprintf("  Chunk %d/%d — %.1f%% complete", i, total_chunks, progress))
    }
  }

  if (show_progress) {
    message(sprintf("Upload complete: %s (%s bytes)", filename,
                    format(total_size, big.mark=",")))
  }

  list(
    status_code = if (!is.null(last_response)) last_response$status_code else 200L,
    upload_id   = upload_id,
    response    = if (!is.null(last_response)) last_response$response else NULL
  )
}
