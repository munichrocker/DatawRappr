#' Publish or republish a Datawrapper chart
#'
#' Publish a chart on Datawrapper.
#'
#' @param chart_id Required. A Datawrapper-chart-id as character string, usually a five character combination of digits and letters, e.g. "aBcDe".
#' @param api_key Optional. A Datawrapper-API-key as character string. Defaults to "environment" - tries to automatically retrieve the key that's stored in the .Reviron-file by \code{\link{datawrapper_auth}}.
#' @param return_urls Optional. If TRUE (default) it returns the code for the responsive iFrame and an URL to the chart.
#'
#' @return A message that specifies, if the publication was successfull. If set, including the iFrame-Code and chart-URL.
#' @author Benedict Witzenberger
#' @note This function publishes a chart in Datawrapper.
#' @examples
#'
#' \dontrun{dw_publish_chart("aBcDE")} # uses the preset key in the .Renviron-file
#'
#' \dontrun{dw_publish_chart(chart_id = "a1B2Cd", api_key = "1234ABCD")} # uses the specified key
#'
#' \dontrun{dw_publish_chart(chart_id = "a1B2Cd", return_urls = TRUE)} # won't return code and URLs for chart
#'
#' @rdname dw_publish_chart
#' @export
dw_publish_chart <- function(chart_id, api_key = "environment", return_urls = TRUE) {

  if (api_key == "environment") {
    api_key <- dw_get_api_key()
  }

  url <- paste0("https://api.datawrapper.de/charts/", chart_id, "/publish")

  r <- httr::POST(url, httr::add_headers(Authorization = paste("Bearer", api_key, sep = " ")))

  try(if(httr::status_code(r) != 200) stop("Fehler bei der Verbindung. Statuscode ist nicht 200."))

  if (httr::status_code(r) == 200) {
    print(paste0("Chart ", chart_id, " published!"))

    if (return_urls == TRUE){
      response_content <- httr::content(r)
      iframe_code <- response_content$data$metadata$publish$`embed-codes`$`embed-method-responsive`
      chart_url <- response_content$data$publicUrl

      print(paste0("### Responsive iFrame-code: ###\n", iframe_code, "\n\n", "### Chart-URL:###\n", chart_url))
    }

  } else {
    warning("There has been an error.", immediate. = TRUE)
  }

}
