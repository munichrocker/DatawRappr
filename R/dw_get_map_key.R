#' Returns the Datawrapper-Map-keys
#'
#' Retrieve the keys for a certain basemap from the \href{https://developer.datawrapper.de/}{Datawrapper-API}.
#'
#' @param api_key Required. A Datawrapper-API-key as character string. Defaults to "environment" - tries to automatically retrieve the key that's stored in the .Reviron-file by \code{\link{datawrapper_auth}}.
#' @param basemap_id Required. Can be retrieved by looking at \code{\link{dw_basemaps}}.
#' @param basemap_value Required. Can be retrieved by looking at \code{\link{dw_basemaps}}.
#'
#' @return A S3-structure of type \strong{dw_basemap_keys} with the elements from the \href{https://developer.datawrapper.de/}{Datawrapper-API}, and the following fields:
#' \item{content}{A data.frame of the returned keys.}
#' \item{path}{The url of the API call.}
#' \item{id}{The basemap id provided.}
#' \item{value}{The basemap value provided.}
#'
#' @author Benedict Witzenberger
#' @note This function returns a data.frame with the keys for the requested map. These keys can be used to map data to the basemap.
#' @importFrom utils str
#' @examples
#'
#' \dontrun{
#'
#' dw_get_map_key(basemap_id = "world2019", basemap_value = "DW_NAME") # uses the preset key in the .Renviron-file
#'
#' }
#'
#' @rdname dw_get_map_key
#' @export
dw_get_map_key <- function(api_key = "environment", basemap_id, basemap_value) {

  if (api_key == "environment") {
    api_key <- dw_get_api_key()
  }

  url <- paste0("https://api.datawrapper.de/plugin/basemaps/", basemap_id, "/", basemap_value)

  r <- httr::GET(url,
                 httr::add_headers(Authorization = paste("Bearer", api_key, sep = " ")),
                 .DATAWRAPPR_UA)


  parsed <- dw_handle_errors(r)

  parsed_tibble <- tibble(parsed[["data"]])

  parsed_tibble %>%
    tidyr::unnest_wider(1) %>%
    tidyr::unnest(values) %>%
    mutate(values = unlist(values)) -> parsed_df

  structure(
    list(
      keys = parsed_df,
      path = url,
      id = basemap_id,
      value = basemap_value
    ),
    class = "dw_basemap_keys"
  )

}

#' @export

print.dw_basemap_keys <- function(x, ...) {
  message("<Datawrapper ", x$path, ">\n", sep = "")
  message("Basemap-ID: ", x$id, "\n", sep = "")
  message("Basemap-Value: ", x$value, "\n", sep = "")
  str(x$keys)
  invisible(x)
}


