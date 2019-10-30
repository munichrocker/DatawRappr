#' Get Datawrapper-API-Key
#'
#' Retrieves the \href{https://developer.datawrapper.de/}{Datawrapper-API}-key from the .Renviron-file, if it was saved there earlier by \code{\link{datawrapper_auth}}.
#'
#' @return The API-key as a character string
#' @author Benedict Witzenberger
#' @note This is a helper function, that shouldn't be of any use outside of this package.
#' @examples
#'
#' dw_get_api_key()
#' @rdname dw_get_api_key
#' @export
dw_get_api_key <- function() {

  if (Sys.getenv("DW_KEY") != "") {

    api_key <- Sys.getenv("DW_KEY")
    return(api_key)

  } else {

    warning("No Datawrapper-key was found. Please add a new key first using datawrapper_auth().", immediate. = TRUE)

  }

}
