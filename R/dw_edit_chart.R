#' Edits a existing Datawrapper chart
#'
#' Modifies an existing Datawrapper chart.
#'
#' @param chart_id Required. A Datawrapper-chart-id as character string, usually a five character combination of digits and letters, e.g. "aBcDe".
#' @param api_key Optional. A Datawrapper-API-key as character string. Defaults to "environment" - tries to automatically retrieve the key that's stored in the .Reviron-file by \code{\link{datawrapper_auth}}.
#' @param title Optional. Adds a title to the plot.
#' @param intro Optional. Adds an intro below the title.
#' @param annotate Optional. Adds a annotation below the plot.
#' @param type Optional. Changes the type of the chart. See \href{https://developer.datawrapper.de/docs/chart-types-2}{the documentation} for the different types.
#' @param source_name Optional. Adds a source name to the plot.
#' @param source_url Optional. Adds a URL to the source name (displayed only, if source name specified). Include http(s):// before URL.
#' @param data Optional. A list. Add separate arguments for the data. See \href{https://developer.datawrapper.de/docs/chart-properties-1}{the documentation} for details.
#' @param visualize Optional. A list. Add separate arguments for the visualization. See \href{https://developer.datawrapper.de/docs/chart-properties-1}{the documentation} for details.
#' @param describe Optional. A list. Add separate arguments for the description. See \href{https://developer.datawrapper.de/docs/chart-properties-1}{the documentation} for details.
#' @param publish Optional. A list. Add separate arguments for publication. See \href{https://developer.datawrapper.de/docs/chart-properties-1}{the documentation} for details.
#'
#' @return A terminal message: "Chart xyz succesfully updated." - or an error message.
#' @author Benedict Witzenberger
#' @note This function builds a body for a API-call to the Datawrapper-API, which contains changes to an existing chart.
#' @note Check their \href{https://developer.datawrapper.de/docs/reference-guide}{reference guide for examples}.
#' @examples
#'
#' \dontrun{dw_edit_chart("aBcDE")} # uses the preset key in the .Renviron-file
#'
#' \dontrun{dw_edit_chart(chart_id = "a1B2Cd", api_key = "1234ABCD")} # uses the specified key
#'
#' \dontrun{dw_edit_chart(chart_id = "a1B2Cd", title = "I'm a title",
#' intro = "Data showing daily results")} # changes title and intro
#'
#' \dontrun{dw_edit_chart(chart_id = "a1B2Cd", title = "I'm a title",
#' visualize = list(`show-tooltips` = "false"))} # adds manual changes as lists
#'
#' @rdname dw_edit_chart
#' @export
dw_edit_chart <- function(chart_id, api_key = "environment", title = "", intro = "", annotate = "",
                          type = "", source_name = "", source_url = "", data = list(), visualize = list(),
                          describe = list(), publish = list()) {

  if (api_key == "environment") {
    api_key <- dw_get_api_key()
  }

  # create empty body for API-call
  call_body <- list(metadata = list())

  # change only specified parts of existing data
  if (title != "") {call_body <- rlist::list.append(call_body, title = title)}
  if (type != "") {call_body <- rlist::list.append(call_body, type = type)}

  if (intro != "") {call_body$metadata$describe$intro <- intro}
  if (annotate != "") {call_body$metadata$annotate$notes <- annotate}

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
  url_upload <- paste0("https://api.datawrapper.de/charts/", chart_id)

  r <- httr::PUT(url_upload, httr::add_headers(Authorization = paste("Bearer", api_key, sep = " ")),
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

  chart_id_response <- parsed$data$id

  try(if (chart_id != chart_id_response) stop("The chart_ids between call and response do not match. Try again and check API."))

  if (httr::status_code(r) == 200) {
    print(paste0("Chart ", chart_id_response, " succesfully updated."))
  }

}
