#' Delete a Datawrapper chart
#'
#' Deletes a chart on Datawrapper.
#'
#' @param chart_id Required. A Datawrapper-chart-id as character string, usually a five character combination of digits and letters, e.g. "aBcDe". Or a \strong{dw_chart}-object.
#' @param api_key Optional. A Datawrapper-API-key as character string. Defaults to "environment" - tries to automatically retrieve the key that's stored in the .Reviron-file by \code{\link{datawrapper_auth}}.
#'
#' @return A message that specifies, if the deletion was successful.
#' @author Benedict Witzenberger
#' @note This function deletes a chart in Datawrapper.
#' @examples
#'
#' \dontrun{dw_delete_chart("aBcDE")} # uses the preset key in the .Renviron-file
#' \dontrun{dw_delete_chart(chart_id = "a1B2Cd", api_key = "1234ABCD")} # uses the specified key
#'
#' @rdname dw_delete_chart
#' @export
dw_delete_chart <- function(chart_id, api_key = "environment") {

  if (api_key == "environment") {
    api_key <- dw_get_api_key()
  }

  chart_id <- dw_check_chart_id(chart_id)

  url <- paste0("https://api.datawrapper.de/v3/charts/", chart_id)

  r <- httr::DELETE(url, httr::add_headers(Authorization = paste("Bearer", api_key, sep = " ")),
                    .DATAWRAPPR_UA)

  if (httr::status_code(r) == "204") {
    cat(paste0("Chart ", chart_id, " sucessfully deleted!", "\n"))
  } else {
    stop("There has been an error while deleting the chart!", immediate. = TRUE)
    return(r)
  }

}
