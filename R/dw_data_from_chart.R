#' Download the data in a Datawrapper chart
#'
#' \lifecycle{maturing}
#' Downloads the data from an existing Datawrapper chart, that ypu have access to. This is the mirrored function to \code{\link{dw_data_to_chart}}.
#'
#' @param chart_id Required. A Datawrapper-chart-id as character string, usually a five character combination of digits and letters, e.g. "aBcDe". Or a \emph{dw_chart}-object. You need to have access to the chart (e.g. it was created by yourself or in one of your teams).
#' @param api_key Optional. A Datawrapper-API-key as character string. Defaults to "environment" - tries to automatically retrieve the key that's stored in the .Reviron-file by \code{\link{datawrapper_auth}}.
#'
#' @return A data.frame.
#' @author Benedict Witzenberger
#' @note This function downloads a R-dataframe from a Datawrapper chart.
#' @examples
#'
#' \dontrun{dw_data_from_chart(df, "aBcDE")} # uses the preset key in the .Renviron-file
#'
#' \dontrun{df <- dw_data_from_chart(df, chart_id = "a1B2Cd", api_key = "1234ABCD")} # uses the specified key
#'
#' @rdname dw_data_from_chart
#' @export
dw_data_from_chart <- function(chart_id, api_key = "environment") {

  if (api_key == "environment") {
    api_key <- dw_get_api_key()
  }

  chart_id <- dw_check_chart_id(chart_id)

  url <- paste0("https://api.datawrapper.de/v3/charts/", chart_id, "/data")

  r <- httr::GET(url, httr::add_headers(Authorization = paste("Bearer", api_key, sep = " ")), .DATAWRAPPR_UA)

  if (!(httr::status_code(r) %in% c(200, 201, 202, 204))) {
    stop(paste0("There has been an error in the download process. Statuscode of the response: ", httr::status_code(r)), immediate. = TRUE)
  }

  x <- as.data.frame(httr::content(r))

  return(x)

}
