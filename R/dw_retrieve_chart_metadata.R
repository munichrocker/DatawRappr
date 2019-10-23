#' Retrieves a Datawrapper chart's metadata
#'
#' Return the metadata of a existing Datawrapper chart.
#'
#' @param chart_id Required. A Datawrapper-chart-id as character string, usually a five character combination of digits and letters, e.g. "aBcDe".
#' @param api_key Optional. A Datawrapper-API-key as character string. Defaults to "environment" - tries to automatically retrieve the key that's stored in the .Reviron-file.
#'
#' @return A list with the elements from the Datawrapper-API
#' \item{status}{Returns 'ok' if the API-key used was correct.}
#' \item{$data$id}{Returns the internal id of the chart - the same as used in chart_id.}
#' \item{$data$title}{Returns the chart's title.}
#' \item{$data$theme}{Returns the chart's theme.}
#' \item{$data$createdAt}{Chart's creation date.}
#' \item{$data$lastModifiedAt}{Chart's last modification date.}
#' \item{$data$metadata}{Contains chart's specifications, like transpose, column formats, visualization settings, colors, titles and texts}
#' \item{$data$metadata$visualize}{Contains the chart's visualization settings.}
#' \item{$data$metadata$describe}{Contains the chart's description settings, like title, source name and url, byline}
#' \item{$data$publishedAt}{Chart's publication date - if published yet.}
#' \item{$data$author$id}{The chart-author's id.}
#' @author Benedict Witzenberger
#' @note This function retrieves all metadata about a chart that's stored by Datawrapper. It is helpful to gain insights in the different options that might be changed via the API.
#' @examples
#'
#' dw_retrieve_chart_metadata("aBcDE") # uses the preset key in the .Renviron-file
#'
#' dw_retrieve_chart_metadata(chart_id = "a1B2Cd", api_key = "1234ABCD") # uses the specified key
#' @rdname dw_retrieve_chart_metadata
#' @export
dw_retrieve_chart_metadata <- function(chart_id, api_key = "environment") {

  url <- paste0("https://api.datawrapper.de/charts/", chart_id)

  if (api_key == "environment") {
    api_key <- dw_get_api_key()
  }

  r <- httr::GET(url, httr::add_headers(Authorization = paste("Bearer", api_key, sep = " ")))

  try(if(httr::status_code(r) != 200) stop("Fehler bei der Verbindung. Statuscode ist nicht 200."))

  return(httr::content(r))
}