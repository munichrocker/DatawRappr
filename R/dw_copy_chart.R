#' Makes a copy of an existing Datawrapper chart
#'
#' \lifecycle{maturing}
#' Creates a new Datawrapper chart object from an existing chart. This function starts the chart-making process like \code{\link{dw_create_chart}}.
#'
#' @md
#' @param copy_from Required. Chart_id from an existing chart which serves as the template.
#' @param api_key Required. A Datawrapper-API-key as character string. Defaults to "environment" - tries to automatically retrieve the key that's stored in the .Reviron-file by \code{\link{datawrapper_auth}}.
#'
#' @return It prints the new chart's id and returns a S3-structure of type \strong{dw_chart} with the elements from the Datawrapper-API, the same as in \code{\link{dw_retrieve_chart_metadata}} and \code{\link{dw_create_chart}}.
#' @author Benedict Witzenberger
#' @importFrom utils str
#' @examples
#'
#' \dontrun{
#' dw_copy_chart(copy_from = "ABCDE")
#' # uses api-key from environment - if set.
#' }
#'
#' \dontrun{
#' dw_copy_chart(copy_from = "ABCDE", api_key = "1234ABCD")
#' } # uses the specified key
#'
#' @rdname dw_copy_chart
#' @export
dw_copy_chart <- function(copy_from, api_key = "environment") {

  if (api_key == "environment") {
    api_key <- dw_get_api_key()
  }

  if (copy_from == "") {
    stop("You have to specify an chart_id to the copy_from argument.")
  }

  copy_from <- dw_check_chart_id(copy_from)

  url <- paste0("https://api.datawrapper.de/v3/charts/", copy_from, "/copy")

  r <- httr::RETRY("POST", url, httr::add_headers(Authorization = paste("Bearer", api_key, sep = " ")),
                  encode = "json", .DATAWRAPPR_UA)

  parsed <- dw_handle_errors(r)

  cat(paste0("New chart's id: ", parsed[["id"]], "\n"))

  structure(
    list(
      content = parsed,
      path = "https://api.datawrapper.de/v3/charts",
      id = parsed[["id"]]
    ),
    class = "dw_chart"
  )

}

#' @export

print.dw_chart <- function(x, ...) {
  cat("<Datawrapper ", x$path, ">\n", sep = "")
  cat("Chart-ID: ", x$id, "\n", sep = "")
  str(x$content)
  invisible(x)
}

