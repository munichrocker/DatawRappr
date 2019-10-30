#' Fill a Datawrapper chart with data from R
#'
#' Uploads a dataframe to Datawrapper, returns a status code and a URL to the uploaded data.
#'
#' @param x Required. A R object of class 'data.frame',to be uploaded as the Datawrapper data.
#' @param chart_id Required. A Datawrapper-chart-id as character string, usually a five character combination of digits and letters, e.g. "aBcDe".
#' @param api_key Optional. A Datawrapper-API-key as character string. Defaults to "environment" - tries to automatically retrieve the key that's stored in the .Reviron-file by \code{\link{datawrapper_auth}}.
#' @param display_response Optional. It TRUE (default) returns the status code and URL to the uploaded data as CSV.
#'
#' @return A list with the elements from the Datawrapper-API
#' \item{status}{Returns 'ok' if the upload was successfull.}
#' \item{data}{Returns a URL to the uploaded data in csv-format.}
#' @author Benedict Witzenberger
#' @note This function uploads a R-dataframe to Datawrapper.
#' @examples
#'
#' \dontrun{dw_data_to_chart(df, "aBcDE")} # uses the preset key in the .Renviron-file
#'
#' \dontrun{dw_data_to_chart(df, chart_id = "a1B2Cd", api_key = "1234ABCD")} # uses the specified key
#'
#' \dontrun{dw_data_to_chart(df, chart_id = "a1B2Cd", display_response = FALSE)} # wont't return response
#'
#' @rdname dw_data_to_chart
#' @export
dw_data_to_chart <- function(x, chart_id, api_key = "environment", display_response = TRUE) {

  if (api_key == "environment") {
    api_key <- dw_get_api_key()
  }

  # test class of input dataframe
  try(if (class(x) != "data.frame") stop("data is not of class data.frame!"))

  # collapse the data in the dataframe as a string
  df_content <- paste(t(sapply(seq(1, nrow(x), by = 1), function(i)
    paste(unlist(x[i,]), collapse = ","))), collapse = "\n")

  # test if header contains seperator symbol
  try(if (TRUE %in% grepl(",", names(x))) stop("The Dataframe's header contains a comma - which is used as the seperator. Remove the comma (e.g. with names()) and try again."))

  # collapse the header of the data as a string
  df_names <- paste(names(x), collapse = ",")

  # combine header and content of dataframe into character string
  data_body <- paste(df_names, df_content, sep = "\n")

  url <- paste0("https://api.datawrapper.de/charts/", chart_id, "/data")

  r <- httr::PUT(url, httr::add_headers(Authorization = paste("Bearer", api_key, sep = " ")),
                 body = data_body)

  try(if(httr::status_code(r) != 200) stop("Fehler bei der Verbindung. Statuscode ist nicht 200."))

  if(httr::status_code(r) == 200) {
    print("Chart updated.")
  }

  if (display_response == TRUE) {
    chart_content <- httr::content(r)
    return(chart_content)
  }

}
