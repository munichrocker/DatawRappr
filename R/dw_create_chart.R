#' Creates a new Datawrapper chart
#'
#' \lifecycle{stable}
#' Creates and returns a new Datawrapper chart object. This function starts the chart-making process
#'
#' @section Chart Types:
#'
#' - `d3-bars` : Bar Chart
#' - `d3-bars-split` : Split Bars
#' - `d3-bars-stacked` : Stacked Bars
#' - `d3-bars-bullet` : Bullet Bars
#' - `d3-dot-plot` : Dot Plot
#' - `d3-range-plot` : Range Plot
#' - `d3-arrow-plot` : Arrow Plot
#' - `column-chart` : Column Chart
#' - `grouped-column-chart` : Grouped Column Chart
#' - `stacked-column-chart` : Stacked Column Chart
#' - `d3-area` : Area Chart
#' - `d3-lines` : Line Chart
#' - `d3-pies` : Pie Chart
#' - `d3-donuts` : Donut Chart
#' - `d3-multiple-pies` : Multiple Pies
#' - `d3-multiple-donuts` : Multiple Donuts
#' - `d3-scatter-plot` : Scatter Plot
#' - `election-donut-chart` : Election Donut
#' - `tables` : Table
#' - `d3-maps-choropleth` : Choropleth Map
#' - `d3-maps-symbols` : Symbol Map
#' - `locator-map` : Locator Map
#'
#' @md
#' @param api_key Required. A Datawrapper-API-key as character string. Defaults to "environment" - tries to automatically retrieve the key that's stored in the .Reviron-file by \code{\link{datawrapper_auth}}.
#' @param title Optional. Will set a chart's title on creation.
#' @param type Optional. Changes the type of the chart. E.g. "d3-bars" for bars, "tables" for a table. Default is "d3-lines". See \href{https://developer.datawrapper.de/docs/chart-types}{the documentation} for the different types or the Chart Types section below. If you do not set this here you can set it with [dw_edit_chart()]
#' @param folderId Optional. Creates chart in specified folder.
#' @param theme Optional. Creates chart with specified theme.
#'
#' @return It prints the new chart's id and returns a S3-structure of type \strong{dw_chart} with the elements from the Datawrapper-API, the same as in \code{\link{dw_retrieve_chart_metadata}}.
#' @author Benedict Witzenberger
#' @note If not specified, the new chart will by default be created without a title and with the type \code{d3-lines}.
#' @importFrom utils str
#' @examples
#'
#' \dontrun{
#' dw_create_chart()
#' } # uses the preset key in the .Renviron-file
#'
#' \dontrun{dw_create_chart(title = "Testtitle")}
#'
#' \dontrun{dw_create_chart(api_key = "1234ABCD")} # uses the specified key
#' @rdname dw_create_chart
#' @export
dw_create_chart <- function(api_key = "environment", title = "", type = "", folderId = "", theme = "") {

  if (api_key == "environment") {
    api_key <- dw_get_api_key()
  }

  call_body <- list(metadata = list())

  if (title != "") {call_body <- rlist::list.append(call_body, title = title)}
  if (type != "") {call_body <- rlist::list.append(call_body, type = type)}
  if (folderId != "") {call_body <- rlist::list.append(call_body, folderId = folderId)}
  if (theme != "") {call_body <- rlist::list.append(call_body, theme = theme)}

  r <- httr::POST("https://api.datawrapper.de/v3/charts", httr::add_headers(Authorization = paste("Bearer", api_key, sep = " ")),
                  body = call_body, encode = "json", .DATAWRAPPR_UA)

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

