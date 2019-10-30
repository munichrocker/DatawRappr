#' Tests the Datawrapper-API-key
#'
#' Tests the key by querying the \href{https://developer.datawrapper.de/}{Datawrapper-API} for user information.
#'
#' @param api_key Optional. A Datawrapper-API-key as character string. Defaults to "environment" - tries to automatically retrieve the key that's stored in the .Reviron-file by \code{\link{datawrapper_auth}}.
#'
#' @return A list with the elements from the \href{https://developer.datawrapper.de/}{Datawrapper-API}
#' \item{status}{Returns 'ok' if the API-key used was correct.}
#' \item{data$user$id}{Returns the internal user id.}
#' \item{data$user$email}{The users e-mail adress.}
#' \item{data$user$isAdmin}{Specifies, if the current user is admin in his/her organization.}
#' \item{data$user$organization$id}{Returns the organization's id or short name.}
#' \item{data$user$organization$name}{Returns the organization's full name.}
#' @author Benedict Witzenberger
#' @note This function tests the API key by retrieving information about the current user from the API. If this works, the API-key is set correctly and ready to go.
#' @examples
#'
#' dw_test_key() # uses the preset key in the .Renviron-file
#'
#' dw_test_key(api_key = "1234ABCD") # uses the specified key
#' @rdname dw_test_key
#' @export
dw_test_key <- function(api_key = "environment") {

  if (api_key == "environment") {
    api_key <- dw_get_api_key()
  }

  r <- httr::GET("https://api.datawrapper.de/account", httr::add_headers(Authorization = paste("Bearer", api_key, sep = " ")))

  try(if(httr::status_code(r) != 200) stop("Fehler bei der Verbindung. Statuscode ist nicht 200."))

  return(httr::content(r))
}
