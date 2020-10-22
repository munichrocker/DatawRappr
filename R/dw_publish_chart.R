#' Publish or republish a Datawrapper chart
#'
#' Publish a chart on Datawrapper.
#'
#' @param chart_id Required. A Datawrapper-chart-id as character string, usually a five character combination of digits and letters, e.g. "aBcDe". Or a \strong{dw_chart}-object.
#' @param api_key Optional. A Datawrapper-API-key as character string. Defaults to "environment" - tries to automatically retrieve the key that's stored in the .Reviron-file by \code{\link{datawrapper_auth}}.
#' @param return_urls Optional. If TRUE (default) it returns the code for the responsive iFrame and an URL to the chart.
#' @param return_object Optional. Defaults to FALSE. Returns an object if set to TRUE which contains \code{publicUrl} and \code{iframeCode}.
#'
#' @return A message that specifies, if the publication was successful. If set, including the iFrame-Code and chart-URL. If \code{return_object} is set to TRUE it returns a S3-structure of type \strong{dw_chart}
#' @author Benedict Witzenberger
#' @note This function publishes a chart in Datawrapper.
#' @examples
#'
#' \dontrun{
#' dw_publish_chart("aBcDE")
#' } # uses the preset key in the .Renviron-file
#'
#' \dontrun{
#' dw_publish_chart(chart_id = "a1B2Cd", api_key = "1234ABCD")
#' } # uses the specified key
#'
#' \dontrun{
#' dw_publish_chart(chart_id = "a1B2Cd", return_urls = FALSE)
#' } # won't return code and URLs for chart
#'
#' \dontrun{
#' published_chart <- dw_publish_chart(chart_id = "a1B2Cd", return_object = TRUE)
#' } # returns an object with URL and IframeCode
#'
#' @rdname dw_publish_chart
#' @export
dw_publish_chart <- function(chart_id, api_key = "environment", return_urls = TRUE, return_object = FALSE) {

  if (api_key == "environment") {
    api_key <- dw_get_api_key()
  }

  chart_id <- dw_check_chart_id(chart_id)

  url <- paste0("https://api.datawrapper.de/charts/", chart_id, "/publish")

  r <- httr::POST(url, httr::add_headers(Authorization = paste("Bearer", api_key, sep = " ")),
                  .DATAWRAPPR_UA)

  parsed <- dw_handle_errors(r)

  httr::handle_reset("https://api.datawrapper.de/")

  parsed

  if (httr::status_code(r) %in% c(200, 201, 202, 203, 204)) {
    cat(paste0("Chart ", chart_id, " published!"))

    if (isTRUE(return_urls)) {

      iframe_code <- parsed$data[[1]]$metadata$publish$`embed-codes`$`embed-method-responsive`
      chart_url <- parsed$data[[1]]$publicUrl

      writeLines(paste0("### Responsive iFrame-code: ###\n", iframe_code, "\n\n", "### Chart-URL:###\n", chart_url))

      }

    if (isTRUE(return_object)) {

      structure(
        list(
          content = parsed,
          path = "https://api.datawrapper.de/v3/charts",
          id = parsed[["id"]],
          publicUrl = parsed$data[[1]]$publicUrl,
          iframeCode = parsed$data[[1]]$metadata$publish$`embed-codes`$`embed-method-responsive`
        ),
        class = "dw_chart"
      )

      }

  } else {
    stop(paste0("There has been an error in the publication process. Statuscode of the response: ", httr::status_code(r)), immediate. = TRUE)
  }
}

#' @export

print.dw_chart <- function(x, ...) {
  cat("<Datawrapper ", x$path, ">\n", sep = "")
  cat("Chart-ID: ", x$id, "\n", sep = "")
  str(x$content)
  invisible(x)
}
