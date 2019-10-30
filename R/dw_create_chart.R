#' Creates a new Datawrapper chart
#'
#' Creates and returns the metadata of the new Datawrapper chart.
#'
#' @param api_key Optional. A Datawrapper-API-key as character string. Defaults to "environment" - tries to automatically retrieve the key that's stored in the .Reviron-file by \code{\link{datawrapper_auth}}.
#'
#' @return A list with the elements from the Datawrapper-API, the same as in dw_retrieve_chart_metadata()
#' @author Benedict Witzenberger
#' @note This function retrieves all metadata about the new chart that's stored by Datawrapper. The new chart will by default be created without a title and with the type d3-lines.
#' @examples
#'
#' dw_create_chart() # uses the preset key in the .Renviron-file
#'
#' dw_create_chart(api_key = "1234ABCD") # uses the specified key
#' @rdname dw_create_chart
#' @export
dw_create_chart <- function(api_key = "environment") {

  if (api_key == "environment") {
    api_key <- dw_get_api_key()
  }

  r <- httr::POST("https://api.datawrapper.de/charts", httr::add_headers(Authorization = paste("Bearer", api_key, sep = " ")))

  try(if(httr::status_code(r) != 200) stop("Fehler bei der Verbindung. Statuscode ist nicht 200."))

  chart_content <- httr::content(r)

  print(paste0("New chart's id: ", chart_content$data[[1]]$id))

  return(chart_content)

}
