#' Handle errors in API call
#'
#' Internal function: Handles errors while calling the Datawrapper-API.
#'
#' @param r Required. A httr-response-object.
#' @return A list with the content of the respone-object - or an error,
#' @author Benedict Witzenberger
#' @examples
#'
#' \dontrun{dw_handle_errors(r)}
#'
#' @rdname dw_handle_errors
#' @keywords Internal

dw_handle_errors <- function(r) {
  # error handling
  if (httr::http_type(r) != "application/json" & httr::http_type(r) != "application/octet-stream") {
    stop("API did not return json", call. = FALSE)
  }

  if (httr::http_error(r)) {
    stop(
      sprintf(
        "Datawrapper API request failed [%s]\n%s",
        httr::status_code(r),
        parsed$message
      ),
      call. = FALSE
    )
  }

  # separate check for type application/json;
  # to avoid raising an error when exporting a chart which returns application/octet-stream

  if (httr::http_type(r) != "application/json") {
    parsed <- jsonlite::fromJSON(httr::content(r, "text"), simplifyVector = FALSE)

    if (length(parsed[["data"]]) > 1) {
      parsed[["data"]] <- list(parsed[["data"]])
    }

    return(parsed)
  }

  # end of error handling

}
