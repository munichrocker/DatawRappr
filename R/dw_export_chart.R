#' Export a Datawrapper chart to PNG, PDF, or SVG
#'
#' Uses the [export endpoint](https://developer.datawrapper.de/docs/exporting-as-pdfsvg) to export a chart as PNG, PDF, or SVG
#' (the latter two require paid plans).
#'
#' @md
#' @param chart_id Required. A Datawrapper-chart-id as character string, usually a five character combination of digits and letters, e.g. "aBcDe". Or a \strong{dw_chart}-object.
#' @param type Required. One of `png`, `pdf`, or `svg`. Defaults to `png`.
#' @param unit Define units which `border_width`, `height` and `width` are measured in. `png` only works with `px`;
#'        `pdf` works with any of them; `svg` should have this set to `NULL`.
#' @param mode color movde for output. Defaults to `rgb`. Only applicable to `pdf` output type.
#' @param width,height width and height of the visualization; if not specified it takes the chart width/height.
#' @param plain if `TRUE` (the default) only the visualization will be exported; `FALSE` includes header and footer.
#' @param scale defines multiplier for the size; e.g. if `3` then the chart will be 3x wider and taller; Defaults to `2`.
#' @param border_width margin around the visualization; e.g. if `border_width` is `20` and `unit` is `px`, the visualization
#'        will have a 20px margin. Default is `0`.
#' @param border_color color of the border; The default is the same as the visualization (likely `white`).
#' @param transparent make the background transparent; The default is `FALSE`; will override `border_color`.
#' @param dark activate the dark mode; The default is `FALSE`.
#' @param api_key Optional. A Datawrapper-API-key as character string. Defaults to "environment" - tries to automatically retrieve the key that's stored in the .Reviron-file by \code{\link{datawrapper_auth}}.
#' @note PDF and SVG export require paid plans
#' @return for `png`, a `{magick}` object; for `pdf`, invisible raw vector with PDF content; for `svg`, invisible character vector with SVG content
#' @author Bob Rudis <bob@@rud.is>
#' @export
#' @examples
#' \dontrun{dw_export_chart("aBcDE")} # uses the preset key in the .Renviron-file
dw_export_chart <- function(chart_id,
                            type = c("png", "pdf", "svg"),
                            unit = c("px", "inch", "mm"),
                            mode = c("rgb", "cmyk"),
                            width = NULL,
                            height = NULL,
                            plain = TRUE,
                            scale = 2,
                            border_width = 0,
                            border_color = NULL,
                            transparent = FALSE,
                            dark = FALSE,
                            api_key = "environment") {

  if (api_key == "environment") {
    api_key <- dw_get_api_key()
  }

  chart_id <- dw_check_chart_id(chart_id)

  type <- match.arg(trimws(tolower(type[1])), c("png", "pdf", "svg"))

  url <- sprintf("https://api.datawrapper.de/v3/charts/%s/export/%s", chart_id, type)

  r <- dw_call_api("GET", url, httr::add_headers(Authorization = paste("Bearer", api_key, sep = " ")),
                 query = list(
                   unit =  match.arg(trimws(tolower(unit[1])), c("px", "inch", "mm")),
                   mode =  match.arg(trimws(tolower(mode[1])), c("rgb", "cmyk")),
                   width = width[1],
                   height = height[1],
                   plain = trimws(tolower(as.character(plain[1]))),
                   scale = scale[1],
                   zoom = scale[1],
                   borderWidth = as.numeric(border_width[1]),
                   borderColor = border_color[1],
                   transparent = trimws(tolower(as.character(transparent[1]))),
                   dark = trimws(tolower(as.character(dark[1])))
                 ),
                 .DATAWRAPPR_UA, return_raw_response=T)

  if (httr::status_code(r) == 200) {

    # ct <- httr::headers(r)[["content-type"]]

    # including a workaround, because API is returning application/octet-stream, instead of specific types
    if (type == "png") return(magick::image_read(r$content))
    if (type == "pdf") return(invisible(r$content))
    if (type == "svg") return(invisible(rawToChar(r$content)))

    # if (grepl("png", ct[1])) return(magick::image_read(r$content))
    # if (grepl("pdf", ct[1])) return(invisible(r$content))
    # if (grepl("svg", ct[1])) return(invisible(rawToChar(r$content)))

  }

  parsed <- dw_handle_errors(r)

}
