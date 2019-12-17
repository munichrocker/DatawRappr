#' Lists all folders
#'
#' Returns all existing folders
#'
#' @param api_key Optional. A Datawrapper-API-key as character string. Defaults to "environment" - tries to automatically retrieve the key that's stored in the .Reviron-file by \code{\link{datawrapper_auth}}.
#'
#' @return Returns a large list of all folders, created by user or team, which then includes all folders with their designated charts.
#' @author Benedict Witzenberger
#' @note This function builds a body for a API-call to the Datawrapper-API, and retrieves all folders a user or team have created.
#' @examples
#'
#' \dontrun{dw_list_folders()} # uses the preset key in the .Renviron-file
#'
#' \dontrun{response_list <- dw_list_folders()} # save the result to a variable
#'
#' @rdname dw_list_folders
#' @export
dw_list_folders <- function(api_key = "environment"){

  if (api_key == "environment") {
    api_key <- dw_get_api_key()
  }

  r <- httr::GET("https://api.datawrapper.de/v3/folders", httr::add_headers(Authorization = paste("Bearer", api_key, sep = " ")),
                 encode = "json", .DATAWRAPPR_UA)

  parsed <- dw_handle_errors(r)

  return(parsed)

}

