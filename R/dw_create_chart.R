#' Creates a new Datawrapper chart
#'
#' Creates and returns the metadata of the new Datawrapper chart.
#'
#' @param api_key Optional. A Datawrapper-API-key as character string. Defaults to "environment" - tries to automatically retrieve the key that's stored in the .Reviron-file by \code{\link{datawrapper_auth}}.
#' @param title Optional. Will set a chart's title on creation.
#' @param type Optional. Changes the type of the chart. See \href{https://developer.datawrapper.de/docs/chart-types-2}{the documentation} for the different types.
#'
#' @return It prints the new chart's id and returns a S3-structure of type _dw_chart_ with the elements from the Datawrapper-API, the same as in dw_retrieve_chart_metadata().
#' @author Benedict Witzenberger
#' @note This function retrieves all metadata about the new chart that's stored by Datawrapper. If not specified, the new chart will by default be created without a title and with the type d3-lines.
#' @importFrom utils str
#' @examples
#'
#' \dontrun{dw_create_chart()} # uses the preset key in the .Renviron-file
#' \dontrun{dw_create_chart(title = "Testtitle")}
#'
#' \dontrun{dw_create_chart(api_key = "1234ABCD")} # uses the specified key
#' @rdname dw_create_chart
#' @export
dw_create_chart <- function(api_key = "environment", title = "", type = "") {

  if (api_key == "environment") {
    api_key <- dw_get_api_key()
  }

  call_body <- list(metadata = list())

  if (title != "") {call_body <- rlist::list.append(call_body, title = title)}
  if (type != "") {call_body <- rlist::list.append(call_body, type = type)}

  r <- httr::POST("https://api.datawrapper.de/charts", httr::add_headers(Authorization = paste("Bearer", api_key, sep = " ")),
                  body = call_body, encode = "json")

  # error handling
  if (httr::http_type(r) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(httr::content(r, "text"), simplifyVector = FALSE)

  if (httr::http_error(r)) {
    stop(
      sprintf(
        "Datawrapper API request failed [%s]\n%s\n<%s>",
        httr::status_code(r),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }
  # end of error handling

  structure(
    list(
      content = parsed,
      path = "https://api.datawrapper.de/charts",
      response = r,
      key = api_key
    ),
    class = "dw_chart"
  )

  print(paste0("New chart's id: ", parsed$data[[1]]$id))

}

#' @export

print.dw_chart <- function(x, ...) {
  cat("<Datawrapper ", x$path, ">\n", sep = "")
  cat("API-Key: ", x$key, "\n", sep = "")
  str(x$content)
  invisible(x)
}

