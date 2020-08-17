#' Creates a legend from a Datawrapper chart
#'
#' \lifecycle{maturing}
#' A helper function that creates a legend as string, vector or HTML from an existing legend in a Datawrapper chart.
#'
#' @section How to:
#'
#' Create a chart that displays the required legend. Then use \code{\link{dw_retrieve_chart_metadata(chart_id)}} to download the meta-information of your chart into a R-variable.
#' You may then extract the legend as a string. It might for example be stored in \code{metadata[["content"]][["metadata"]][["visualize"]][["categories"]]} or in \code{metadata[["content"]][["metadata"]][["visualize"]][["custom-colors"]]}.
#'
#' @md
#' @param legend Required. A Datawrapper-API-key as character string. Defaults to "environment" - tries to automatically retrieve the key that's stored in the .Reviron-file by \code{\link{datawrapper_auth}}.
#' @param return_val Required. One of c("string", "vector", "html"). Set the output type. Defaults to "string".
#' @param use_ranges Optional. Filters out automatically generated ranges (e.g. `(0,10]`). Defaults to FALSE.
#'
#' @return A string, vector or HTML (as string), as defined in `return_val`.
#' @author Benedict Witzenberger
#' @examples
#'
#' \dontrun{
#'
#' dw_legend_to_string(legend)
#'
#' dw_legend_to_string(legend, return_val = "html") # return simple HTML-code
#'
#' dw_legend_to_string(legend, use_ranges = TRUE) # filter out ranges
#' }
#' @rdname dw_legend_to_string
#' @export
dw_legend_to_string <- function(legend, return_val = c("string", "vector", "html"), use_ranges = FALSE) {
  if (!is.vector(legend)) {stop("legend is not a vector.")}
  if (!exists("return_val")) {return_val <- "string"}

  color_vector <- unlist(legend, use.names=TRUE)

  if (isTRUE(use_ranges)) {
    color_vector <- color_vector[!grepl("^\\(.+]$", names(color_vector))]
  }

  if (return_val == "string") {

    return(paste(names(color_vector), color_vector, sep = " = "))

  } else if (return_val == "html") {

    return(paste0(
      "<span style='background-color:", color_vector,
      "'>&nbsp;&nbsp;&nbsp;&nbsp;</span> ", names(color_vector)
      , collapse = " "))

  } else if (return_val == "vector") {

    return(color_vector)

  }
}
