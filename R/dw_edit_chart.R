#' Edits a existing Datawrapper chart
#'
#' Modifies an existing Datawrapper chart.
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
#' @param chart_id Required. A Datawrapper-chart-id as character string, usually a five character combination of digits and letters, e.g. "aBcDe". Or a \strong{dw_chart}-object.
#' @param api_key Required. A Datawrapper-API-key as character string. Defaults to "environment" - tries to automatically retrieve the key that's stored in the .Reviron-file by \code{\link{datawrapper_auth}}.
#' @param title Optional. Adds a title to the plot.
#' @param intro Optional. Adds an intro below the title.
#' @param annotate Optional. Adds a annotation below the plot.
#' @param byline Optional. Adds the name of the chart's creator.
#' @param type Optional. Changes the type of the chart. See \href{https://developer.datawrapper.de/docs/chart-types-2}{the documentation} for the different types or the Chart Type section below.
#' @param source_name Optional. Adds a source name to the plot.
#' @param source_url Optional. Adds a URL to the source name (displayed only, if source name specified). Include http(s):// before URL.
#' @param folderId Optional. Moves the chart to the specified folder (by folder-id, which can be found using \code{\link{dw_list_folders}}).
#' @param data Optional. A list. Add separate arguments for the data. See \href{https://developer.datawrapper.de/docs/chart-properties-1}{the documentation} for details.
#' @param visualize Optional. A list. Add separate arguments for the visualization. See \href{https://developer.datawrapper.de/docs/chart-properties-1}{the documentation} for details.
#' @param describe Optional. A list. Add separate arguments for the description. See \href{https://developer.datawrapper.de/docs/chart-properties-1}{the documentation} for details.
#' @param publish Optional. A list. Add separate arguments for publication. See \href{https://developer.datawrapper.de/docs/chart-properties-1}{the documentation} for details.
#'
#' @return A terminal message: "Chart xyz succesfully updated." - or an error message.
#' @author Benedict Witzenberger
#' @note This function builds a body for a API-call to the Datawrapper-API.
#' @note Check their \href{https://developer.datawrapper.de/docs/reference-guide}{reference guide} or \href{https://developer.datawrapper.de/reference#patchchartsid}{API-documentation}.
#' @examples
#'
#' \dontrun{dw_edit_chart("aBcDE")} # uses the preset key in the .Renviron-file, no changes
#'
#' \dontrun{dw_edit_chart(chart_id = "a1B2Cd", api_key = "1234ABCD")} # uses the specified key, no changes
#'
#' \dontrun{dw_edit_chart(chart_id = "a1B2Cd", title = "I'm a title",
#' intro = "Data showing daily results")} # changes title and intro
#'
#' \dontrun{dw_edit_chart(chart_id = "a1B2Cd", title = "I'm a title",
#' data = list("transpose" = "true"))} # transpose data
#'
#' @rdname dw_edit_chart
#' @export
dw_edit_chart <- function(chart_id, api_key = "environment", title = "", intro = "", annotate = "", byline = "",
                          type = "", source_name = "", source_url = "", folderId = "", data = list(), visualize = list(),
                          describe = list(), publish = list()) {

  if (api_key == "environment") {
    api_key <- dw_get_api_key()
  }

  chart_id <- dw_check_chart_id(chart_id)

  # create empty body for API-call
  call_body <- list(metadata = list())

  # change only specified parts of existing data
  if (title != "") {call_body <- rlist::list.append(call_body, title = title)}
  if (type != "") {call_body <- rlist::list.append(call_body, type = type)}
  if (folderId != "") {call_body <- rlist::list.append(call_body, folderId = folderId)}

  if (intro != "") {call_body$metadata$describe$intro <- intro}
  if (annotate != "") {call_body$metadata$annotate$notes <- annotate}

  if (byline != "") {call_body$metadata$describe$byline <- byline}
  if (source_name != "") {call_body$metadata$describe$`source-name` <- source_name}
  if (source_url != "") {

    if (grepl("^http", source_url) == FALSE) {  # include simple test, if url is starting with http(s)
      source_url <- paste0("http://", source_url)
    }

    call_body$metadata$describe$`source-url` <- source_url
  }

  # work in additional arguments, if specified
  if (length(data) > 0) {
    if (!is.list(call_body$metadata$data)) {
      call_body$metadata$data <- list()
    }
    call_body$metadata$data <- utils::modifyList(call_body$metadata$data, data)
  }

  if (length(visualize) > 0) {
    if (!is.list(call_body$metadata$visualize)) {
      call_body$metadata$visualize <- list()
    }

    call_body$metadata$visualize <- utils::modifyList(call_body$metadata$visualize, visualize)
  }

  if (length(describe) > 0) {
    if (!is.list(call_body$metadata$describe)) {
      call_body$metadata$describe <- list()
    }

    call_body$metadata$describe <- utils::modifyList(call_body$metadata$describe, describe)
  }

  if (length(publish) > 0) {
    if (!is.list(call_body$metadata$publish)) {
      call_body$metadata$publish <- list()
    }

    call_body$metadata$publish <- utils::modifyList(call_body$metadata$publish, publish)
  }

  # send call to API
  # upload modified data
  # solution for API v1:
  # url_upload <- paste0("https://api.datawrapper.de/charts/", chart_id)
  #
  # r <- httr::PUT(url_upload, httr::add_headers(Authorization = paste("Bearer", api_key, sep = " ")),
  #                       body = call_body, encode = "json", .DATAWRAPPR_UA)

  url_upload <- paste0("https://api.datawrapper.de/v3/charts/", chart_id)
  r <- httr::PATCH(url_upload, httr::add_headers(Authorization = paste("Bearer", api_key, sep = " ")),
                   body = call_body, encode = "json", .DATAWRAPPR_UA)

  parsed <- dw_handle_errors(r)

  chart_id_response <- parsed["id"][[1]] #for v3: parsed["id"][[1]], for v1: parsed[["data"]][[1]][["id"]]

  try(if (chart_id != chart_id_response) stop(paste0("The chart_ids between call (",  chart_id ,") and response (",  chart_id_response ,") do not match. Try again and check API.")))

  if (chart_id == chart_id_response & httr::status_code(r) == 200) {
    print(paste0("Chart ", chart_id_response, " succesfully updated."))
  }

}
