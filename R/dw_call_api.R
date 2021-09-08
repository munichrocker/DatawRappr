#' Calls the Datawrapper API, retrying when neccessary, and validating the JSON response
#'
#' \lifecycle{experimental}
#' Light wrapper around httr::RETRY
#'
#' @return parsed JSON result
#' @importFrom utils str
#' @rdname dw_call-api
dw_call_api <- function(..., return_raw_response=F, enforce_json_response=T) {
  r <- httr::RETRY(...)

  httr::handle_reset("https://api.datawrapper.de/")

  if (!(httr::status_code(r) %in% c(200, 201, 202, 204))) {
    stop(paste0("There has been an error in an API call. Statuscode of the response: ", httr::status_code(r)), immediate. = TRUE)
  }

  if (return_raw_response) {
    return (r)
  }

  if (!enforce_json_response) {
    return (httr::content(r))
  }

  parsed <- dw_handle_errors(r)

  return(parsed)
}
