#' Fill a Datawrapper chart with data from R
#'
#' \lifecycle{stable}
#' Uploads a dataframe to Datawrapper, returns a message.
#'
#' @param x Required. A R object of class 'data.frame',to be uploaded as the Datawrapper data.
#' @param chart_id Required. A Datawrapper-chart-id as character string, usually a five character combination of digits and letters, e.g. "aBcDe". Or a \emph{dw_chart}-object.
#' @param parse_dates Optional. Defaults to TRUE. Should columns that contain a Date or as POSIX-object be automatically converted to a character vector?
#' @param api_key Optional. A Datawrapper-API-key as character string. Defaults to "environment" - tries to automatically retrieve the key that's stored in the .Reviron-file by \code{\link{datawrapper_auth}}.
#'
#' @return A terminal message.
#' @author Benedict Witzenberger
#' @note This function uploads a R-dataframe to Datawrapper.
#' @examples
#'
#' \dontrun{dw_data_to_chart(df, "aBcDE")} # uses the preset key in the .Renviron-file
#'
#' \dontrun{dw_data_to_chart(df, chart_id = "a1B2Cd", api_key = "1234ABCD")} # uses the specified key
#'
#' \dontrun{dw_data_to_chart(df, chart_id = "a1B2Cd", parse_dates = FALSE)} # do not parse Dates to characters
#'
#' @rdname dw_data_to_chart
#' @export
dw_data_to_chart <- function(x, chart_id, parse_dates = TRUE, api_key = "environment") {

  if (api_key == "environment") {
    api_key <- dw_get_api_key()
  }

  chart_id <- dw_check_chart_id(chart_id)

  # try conversion - to avoid problems with tibbles
  x <- as.data.frame(x)

  # test class of input dataframe
  try(if (class(x) != "data.frame") stop("Data is not of class data.frame!"))

  # find indices that contain vectors of type Data or POSIXt (which includes both POSIXct and POSIXlt)
  if (parse_dates == TRUE) {
    idx <- sapply(x, function(df_x) inherits(df_x, "Date") || inherits(df_x, "POSIXt"))
    x[idx] <- lapply(x[idx], as.character)
  }

  # collapse the data in the dataframe as a string
  data_body <- readr::format_csv(x)

  # # test if header contains separator symbol
  # try(if (TRUE %in% grepl(",", names(x))) stop("The Dataframe's header contains a comma - which is used as the column separator. Remove the comma (e.g. with names()) and try again."))
  #
  # # collapse the header of the data as a string
  # df_names <- paste(names(x), collapse = ",")
  #
  # # combine header and content of dataframe into character string
  # data_body <- paste(df_names, df_content, sep = "\n")

  url <- paste0("https://api.datawrapper.de/v3/charts/", chart_id, "/data")

  r <- httr::PUT(url, httr::add_headers(Authorization = paste("Bearer", api_key, sep = " ")),
                 body = data_body, .DATAWRAPPR_UA)

  if (httr::status_code(r) %in% c(200, 201, 202, 204)) {
    cat(paste0("Data in ", chart_id, " successfully updated.", "\n"))
  } else {
    stop(paste0("There has been an error in the upload process. Statuscode of the response: ", httr::status_code(r)), immediate. = TRUE)
  }

}
