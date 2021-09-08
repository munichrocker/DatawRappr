#' Lists all charts
#'
#' Returns all created Datawrapper charts by the current user.
#'
#' @param api_key Required. A Datawrapper-API-key as character string. Defaults to "environment" - tries to automatically retrieve the key that's stored in the .Reviron-file by \code{\link{datawrapper_auth}}.
#' @param userId Optional. A numeric. Only return charts created by a certain user id.
#' @param published Optional. A string. Use true or false (lowercase) to use this filter.
#' @param search Optional. A string. Use this filter to search for a certain character string.
#' @param order Optional. A string. DESC by default, might be changed to "ASC".
#' @param orderBy Optional. A string. Is set to "createdAt" by default. Might be changed to: "id", "email", "name".
#' @param limit Optional. A numeric. Defaults to 100. Number of charts to be retrieved.
#'
#' @return A tibble of all created charts. Not including all metadata, which can be retrievd for a single chart using \code{\link{dw_retrieve_chart_metadata}}.
#' \item{total}{Total number of created charts.}
#' \item{next}{Returns the API-call for the next page}
#' @author Benedict Witzenberger, Bob Rudis
#' @examples
#'
#' \dontrun{dw_list_charts("aBcDE")} # uses the preset key in the .Renviron-file
#'
#' \dontrun{response_list <- dw_list_charts()} # save list of charts into variable
#'
#' @rdname dw_list_charts
#' @export
dw_list_charts <- function(api_key = "environment", userId = "", published = "", search = "", order = "DESC", orderBy = "createdAt", limit = "") {

  if (api_key == "environment") {
    api_key <- dw_get_api_key()
  }

  charts_query <- list()

  if (userId != "") {call_body <- rlist::list.append(charts_query, userId = userId)}
  if (published != "") {call_body <- rlist::list.append(charts_query, published = published)}
  if (search != "") {call_body <- rlist::list.append(charts_query, search = search)}

  if (order != "DESC") {
    call_body <- rlist::list.append(charts_query, order = order)
    } else {
    call_body <- rlist::list.append(charts_query, order = "DESC")
  }

  if (orderBy != "createdAt") {
    call_body <- rlist::list.append(charts_query, orderBy = orderBy)
  } else {
    call_body <- rlist::list.append(charts_query, orderBy = "createdAt")
  }

  if (limit != "") {call_body <- rlist::list.append(charts_query, limit = limit)}

  parsed <- dw_call_api("GET", "https://api.datawrapper.de/v3/charts",
                 httr::add_headers(Authorization = paste("Bearer", api_key, sep = " ")),
                 query = charts_query, encode = "json", .DATAWRAPPR_UA)

  out <- bind_rows(parsed$list)

  return(out)

}
