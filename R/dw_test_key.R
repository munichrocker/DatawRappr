#' Tests the Datawrapper-API-key
#'
#' Tests the key by querying the \href{https://developer.datawrapper.de/}{Datawrapper-API} for user information.
#'
#' @param api_key Required. A Datawrapper-API-key as character string. Defaults to "environment" - tries to automatically retrieve the key that's stored in the .Reviron-file by \code{\link{datawrapper_auth}}.
#'
#' @return A S3-structure of type \strong{dw_user} with the elements from the \href{https://developer.datawrapper.de/}{Datawrapper-API}, and the following fields:
#' \item{status}{Returns 'ok' if the API-key used was correct.}
#' \item{data$user$id}{Returns the internal user id.}
#' \item{data$user$email}{The users e-mail adress.}
#' \item{data$user$isAdmin}{Specifies, if the current user is admin in his/her organization.}
#' \item{data$user$organization$id}{Returns the organization's id or short name.}
#' \item{data$user$organization$name}{Returns the organization's full name.}
#' @author Benedict Witzenberger
#' @note This function tests the API key by retrieving information about the current user from the API. If this works, the API-key is set correctly and ready to go.
#' @importFrom utils str
#' @examples
#'
#' \dontrun{dw_test_key()} # uses the preset key in the .Renviron-file
#'
#' \dontrun{dw_test_key(api_key = "1234ABCD")} # uses the specified key
#'
#' @rdname dw_test_key
#' @export
dw_test_key <- function(api_key = "environment") {

  if (api_key == "environment") {
    api_key <- dw_get_api_key()
  }

  parsed <- dw_call_api("GET", "https://api.datawrapper.de/v3/me",
                 httr::add_headers(Authorization = paste("Bearer", api_key, sep = " ")),
                 .DATAWRAPPR_UA)

  structure(
    list(
      content = parsed,
      path = "https://api.datawrapper.de/v3/me",
      response = r
    ),
    class = "dw_user"
  )
}

#' @export

print.dw_user <- function(x, ...) {
  message("<Datawrapper ", x$path, ">\n", sep = "")
  message("Response:\n")
  str(x$response)
  message("Content:\n")
  str(x$content)
  invisible(x)
}

