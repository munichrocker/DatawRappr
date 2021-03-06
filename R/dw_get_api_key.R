#' Get Datawrapper-API-Key
#'
#' Retrieves the \href{https://developer.datawrapper.de/}{Datawrapper-API}-key from the `DW_KEY`
#' environment variable if it exists.
#'
#' You can use [datawrapper_auth()] to store your API key into your local `.Renviron` file.
#'
#' @md
#' @return The API-key as a character string
#' @author Benedict Witzenberger
#' @note This is a helper function, that shouldn't be of any use outside of this package.
#' @examples
#'
#' \dontrun{dw_get_api_key()}
#'
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
